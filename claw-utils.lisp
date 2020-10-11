(cl:defpackage :claw-utils
  (:use :cl :alexandria)
  (:export #:claw-pointer
           #:claw-string
           #:claw-array
           #:define-bitfield-from-enum
           #:define-bitfield-from-constants
           #:ignore-functions))

(cl:in-package :claw-utils)

(defvar *class* nil)

;;;
;;; CLAW POINTER
;;;
(cffi:define-foreign-type claw-pointer () ())


(cffi:define-parse-method claw-pointer (&optional (type :void))
  (make-instance 'claw-pointer
                 :actual-type `(:pointer ,(cffi::canonicalize-foreign-type type))))


(defmethod cffi:expand-to-foreign (value (type claw-pointer))
  (if value `(or ,value (cffi:null-pointer)) (cffi:null-pointer)))


(defmethod cffi:expand-from-foreign (value (type claw-pointer))
  (declare (ignore type))
  value)

;;;
;;; BITFIELD
;;;
(defmacro define-bitfield-from-enum (name enum)
  `(cffi:defbitfield ,name
     ,@(loop for keyword in (cffi:foreign-enum-keyword-list enum)
             collect (list keyword (cffi:foreign-enum-value enum keyword)))))


(defmacro define-bitfield-from-constants (name &body constants)
  (let ((prefix-len (length (claw.util:common-prefix constants))))
    `(cffi:defbitfield ,name
       ,@(loop for constant in constants
               collect (let ((stringified (string constant)))
                         `(,(make-keyword (subseq stringified
                                                  prefix-len
                                                  (1- (length stringified))))
                           ,(eval constant)))))))


;;;
;;; CLAW STRING
;;;
(cffi:define-foreign-type claw-string () ())


(cffi:define-parse-method claw-string ()
  (make-instance 'claw-string :actual-type '(:pointer :char)))


;; TODO: Use stack allocation
(defmacro with-foreign-string* ((var string-or-ptr &rest args) &body body)
  (with-gensyms (provided-p)
    (once-only (string-or-ptr)
      `(let* ((,provided-p (cffi:pointerp ,string-or-ptr))
              (,var (or (and ,string-or-ptr
                             (if ,provided-p
                                 ,string-or-ptr
                                 (cffi:foreign-string-alloc ,string-or-ptr ,@args)))
                        (cffi:null-pointer))))
         (unwind-protect
              (progn ,@body)
           (unless (or ,provided-p (cffi:null-pointer-p ,var))
             (cffi:foreign-string-free ,var)))))))


(defmethod cffi:expand-from-foreign (value (type claw-string))
  (declare (ignore type))
  value)


(defmethod cffi:expand-to-foreign-dyn (value var body (type claw-string))
  (declare (ignore type))
  `(with-foreign-string* (,var ,value)
     ,@body))


;;;
;;; CLAW ARRAY
;;;
(cffi:define-foreign-type claw-array ()
  ((array-type :initarg :array-type
               :initform (error ":array-type missing")
               :reader array-type-of)))


(cffi:define-parse-method claw-array (type &optional count)
  (let ((array-type `(:array ,(cffi::canonicalize-foreign-type type)
                             ,@(when count
                                 `(,count)))))
    (make-instance 'claw-array :array-type array-type
                               :actual-type array-type)))


(defmethod cffi:expand-from-foreign (value (type claw-array))
  (declare (ignore type))
  value)


(defmacro with-foreign-array* ((var array-or-ptr type) &body body)
  (with-gensyms (provided-p)
    (once-only (array-or-ptr)
      `(let* ((,provided-p (cffi:pointerp ,array-or-ptr))
              (,var (or (and ,array-or-ptr
                             (if ,provided-p
                                 ,array-or-ptr
                                 (cffi:foreign-array-alloc ,array-or-ptr ,type)))
                        (cffi:null-pointer))))
         (unwind-protect
              (progn ,@body)
           (unless (or ,provided-p (cffi:null-pointer-p ,var))
             (cffi:foreign-array-free ,var)))))))


(defmethod cffi:expand-to-foreign-dyn (value var body (type claw-array))
  `(with-foreign-array* (,var ,value ',(array-type-of type))
     ,@body))

;;;
;;;
;;;
(defun match-function-signature (entity owner-name name &rest param-type-names)
  (let ((owner (claw.spec:foreign-owner entity))
        (any-params (eq (first param-type-names) :any)))
    (and (typep entity 'claw.spec:foreign-function)
         (string= (if owner-name
                      (claw.spec:foreign-entity-name entity)
                      (claw.spec:format-full-foreign-entity-name entity))
                  (if (and owner (or (eq name :ctor) (eq name :dtor)))
                      (claw.spec:foreign-entity-name owner)
                      name))
         (if owner-name
             (when owner
               (string= owner-name (claw.spec:format-full-foreign-entity-name owner)))
             t)
         (or any-params
             (= (length (claw.spec:foreign-function-parameters entity))
                (length param-type-names)))
         (or any-params
             (loop for expected-param in (claw.spec:foreign-function-parameters entity)
                   for provided-type-name in param-type-names
                   for expected-unqualified = (claw.spec:unqualify-foreign-entity
                                               (claw.spec:foreign-enveloped-entity
                                                expected-param))
                   for expected-param-type-name = (when (claw.spec:foreign-named-p expected-unqualified)
                                                    (claw.spec:format-full-foreign-entity-name
                                                     expected-unqualified))
                   always (string= expected-param-type-name provided-type-name))))))


(defmacro ignore-functions (&body funcs)
  (let ((entity (gensym (string 'entity))))
    `(lambda (,entity)
       (or ,@(loop for fu in funcs
                   collect (if (eq (first fu) :in-class)
                               `(let ((*class* ,(second fu)))
                                  (funcall (ignore-functions ,@(cddr fu)) ,entity))
                               (destructuring-bind (name &rest params) fu
                                 `(match-function-signature ,entity
                                                            *class*
                                                            ,name
                                                            ,@params))))))))
