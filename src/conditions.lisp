(in-package #:schema-protocol)

(define-condition schema-error (error)
  ((message :initarg :message :reader schema-error-message :initform nil))
  (:report (lambda (c s)
             (format s "Schema error~@[: ~A~]" (schema-error-message c)))))

(defstruct (schema-issue (:constructor make-schema-issue)
                         (:conc-name schema-issue-))
  (path nil)
  (message nil)
  (value nil)
  (slot nil))

(defun %format-path (path)
  (if path
      (format nil "~{~A~^.~}" path)
      "$"))

(define-condition schema-validation-error (schema-error)
  ((issues :initarg :issues :reader schema-validation-error-issues :initform nil))
  (:report (lambda (c s)
             (let ((issues (schema-validation-error-issues c)))
               (format s "Schema validation failed~@[: ~A~]~%~{~A~%~}"
                       (schema-error-message c)
                       (mapcar (lambda (i)
                                 (format nil "  ~A: ~A"
                                         (%format-path (schema-issue-path i))
                                         (schema-issue-message i)))
                               issues))))))

(define-condition schema-unknown (schema-error)
  ((name :initarg :name :reader schema-unknown-name))
  (:report (lambda (c s)
             (format s "Unknown schema ~S~@[: ~A~]"
                     (schema-unknown-name c)
                     (schema-error-message c)))))

(define-condition schema-unknown-format (schema-error)
  ((format :initarg :format :reader schema-unknown-format-format))
  (:report (lambda (c s)
             (format s "Unknown schema format ~S~@[: ~A~]"
                     (schema-unknown-format-format c)
                     (schema-error-message c)))))

