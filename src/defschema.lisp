(in-package #:schema-protocol)

;;; (defschema user (person)
;;;   "Interchange user."
;;;   (name string :min-length 1)
;;;   (age (integer 0) :default 0 :optional t)
;;;   (email string :format :email :optional t)
;;;   (address address)
;;;   (tags (vector string))
;;;   (:compute display-name (self)
;;;     (format nil "~A (~A)" (name self) (age self)))
;;;   (:validate (self)
;;;     (unless (plusp (age self))
;;;       (schema-issue 'age "age must be positive")))
;;;   (:validate-field email (value)
;;;     (unless (find #\@ value) (schema-issue 'email "need @"))
;;;     value)
;;;   (:extra :forbid)
;;;   (:key-style :kebab))

(defun %parse-defschema-body (body)
  (let ((doc nil)
        (slots '())
        (options '()))
    (dolist (form body)
      (cond
        ((stringp form)
         (setf doc form))
        ((and (consp form) (keywordp (first form)))
         (push form options))
        ((consp form)
         (push form slots))
        (t
         (error 'schema-error
                :message (format nil "defschema: bad form ~S" form)))))
    (values doc (nreverse slots) (nreverse options))))

(defun %slot-initarg (name)
  (intern (symbol-name name) :keyword))

(defun %expand-slot (form)
  (destructuring-bind (name type &rest keys) form
    (unless (symbolp name)
      (error 'schema-error :message (format nil "defschema: slot name must be a symbol, got ~S" name)))
    (let* ((accessor (getf keys :accessor name))
           (initarg (getf keys :initarg (%slot-initarg name)))
           (has-default (not (eq (getf keys :default '%missing) '%missing)))
           (default (getf keys :default))
           (has-initform (not (eq (getf keys :initform '%missing) '%missing)))
           (required (getf keys :required))
           (optional (getf keys :optional))
           (filtered (copy-list keys)))
      (dolist (k '(:accessor :initarg :default))
        (remf filtered k))
      (when (and required optional)
        (error 'schema-error
               :message (format nil "defschema: ~S is both :required and :optional" name)))
      (when (and has-default (not optional) (not required))
        (setf optional t
              filtered (list* :optional t filtered)))
      `(,name :type ,type
              :initarg ,initarg
              :accessor ,accessor
              ,@(cond
                  (has-initform nil)
                  (has-default `(:initform ',default)))
              ,@filtered))))

(defun %specialize-lambda-list (lambda-list class-name)
  (cond
    ((null lambda-list)
     `((,(gensym "SELF") ,class-name)))
    ((member (first lambda-list) '(&optional &key &rest &aux))
     (error 'schema-error :message "defschema :compute needs a required parameter"))
    ((and (consp (first lambda-list)) (symbolp (first (first lambda-list))))
     lambda-list)
    ((symbolp (first lambda-list))
     (cons `(,(first lambda-list) ,class-name) (rest lambda-list)))
    (t
     (error 'schema-error
            :message (format nil "bad :compute lambda-list ~S" lambda-list)))))

(defun %ensure-schema-supers (supers)
  (if (or (null supers)
          (not (some (lambda (s)
                       (let ((c (find-class s nil)))
                         (and c (schema-class-p c))))
                     supers)))
      (append supers '(schema-object))
      supers))

(defmacro defschema (name direct-superclasses &body body)
  "Define a CLOS interchange schema (metaclass SCHEMA-CLASS).

Slot forms: (name type &key required optional default initform initarg accessor
                           key aliases format pattern min-length max-length
                           minimum maximum description dump wire)
TYPE is a Lisp type specifier or nested schema class name.
Options:
  (:extra :forbid|:ignore|:allow)
  (:key-style :downcase|:kebab|:snake|:camel|:preserve)
  (:tag slot-name &optional variant*) — discriminator; subclasses (or VARIANT*)
  (:compute name lambda-list . body)
  (:validate (self) . body)           — VALIDATE-OBJECT :after
  (:validate-field slot (value) . body)"
  (multiple-value-bind (doc slots options)
      (%parse-defschema-body body)
    (let ((clos-slots (mapcar #'%expand-slot slots))
          (extra nil)
          (key-style nil)
          (tag nil)
          (computes '())
          (compute-forms '())
          (validate-forms '())
          (supers (%ensure-schema-supers direct-superclasses)))
      (dolist (opt options)
        (ecase (first opt)
          (:extra (setf extra (second opt)))
          (:key-style (setf key-style (second opt)))
          (:tag (setf tag (rest opt)))
          (:compute
           (destructuring-bind (cname lambda-list &body cbody) (rest opt)
             (push cname computes)
             (push `(defmethod ,cname ,(%specialize-lambda-list lambda-list name)
                      ,@cbody)
                   compute-forms)))
          (:validate
           (destructuring-bind (lambda-list &body vbody) (rest opt)
             (push `(defmethod validate-object :after
                        ,(%specialize-lambda-list lambda-list name)
                      ,@vbody)
                   validate-forms)))
          (:validate-field
           (destructuring-bind (slot-name lambda-list &body fbody) (rest opt)
             (push `(defmethod validate-field ((schema (eql ',name))
                                               (slot (eql ',slot-name))
                                               ,@(if (and (consp lambda-list)
                                                          (eq (first lambda-list) 'value)
                                                          (null (rest lambda-list)))
                                                     '(value)
                                                     lambda-list))
                      ,@fbody)
                   validate-forms)))))
      `(progn
         (defclass ,name ,supers
           ,clos-slots
           (:metaclass schema-class)
           ,@(when extra `((:extra ,extra)))
           ,@(when key-style `((:key-style ,key-style)))
           ,@(when tag `((:tag ,@tag)))
           ,@(when doc `((:documentation ,doc))))
         (setf (schema-class-computes (find-class ',name))
               ',(nreverse computes))
         ,@(nreverse compute-forms)
         ,@(nreverse validate-forms)
         (find-class ',name)))))
