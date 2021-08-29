(cl:defpackage :claw-utils
  (:use :cl :alexandria)
  (:export #:claw-pointer
           #:claw-string
           #:claw-array
           #:define-bitfield-from-enum
           #:define-bitfield-from-constants

           #:common-prefix))

(cl:in-package :claw-utils)

;;;
;;; CLAW POINTER
;;;
(cffi:define-foreign-type claw-pointer () ())


(cffi:define-parse-method claw-pointer (&optional (type :void))
  (make-instance 'claw-pointer
                 :actual-type `(:pointer ,(cffi::canonicalize-foreign-type type))))


(defmethod cffi:expand-to-foreign (value (type claw-pointer))
  (if value `(or ,value (cffi:null-pointer)) `(cffi:null-pointer)))


(defmethod cffi:expand-from-foreign (value (type claw-pointer))
  (declare (ignore type))
  value)

;;;
;;; BITFIELD
;;;
(defun common-prefix (strings)
  (let ((len (length strings))
        (strings (map 'vector #'string strings)))
    (if (> len 1)
        (let* ((sorted-strings (sort strings #'string<))
               (first (aref sorted-strings 0))
               (last (aref sorted-strings (1- (length sorted-strings))))
               (mismatch-idx (mismatch first last)))
          (if mismatch-idx
              (if-let ((hyphenated-prefix-idx (position #\- first :from-end t
                                                                  :end mismatch-idx)))
                (subseq first 0 (1+ hyphenated-prefix-idx))
                "")
              ""))
        "")))


(defmacro define-bitfield-from-enum (name enum)
  `(cffi:defbitfield ,name
     ,@(loop for keyword in (cffi:foreign-enum-keyword-list enum)
             collect (list keyword (cffi:foreign-enum-value enum keyword)))))


(defmacro define-bitfield-from-constants (name &body constants)
  (let ((prefix-len (length (common-prefix constants))))
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
