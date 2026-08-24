(in-package #:schema-protocol)

(defvar *schema-registry* (make-hash-table :test #'eq)
  "Map schema class names to schema-class metaobjects.")

(defclass schema-direct-slot-definition (standard-direct-slot-definition)
  ((required :initarg :required :initform nil :reader slot-required-p)
   (optional :initarg :optional :initform nil :reader slot-optional-p)
   (key :initarg :key :initform nil :reader %slot-key)
   (aliases :initarg :aliases :initform nil :reader slot-aliases)
   (format :initarg :format :initform nil :reader slot-format)
   (pattern :initarg :pattern :initform nil :reader slot-pattern)
   (min-length :initarg :min-length :initform nil :reader slot-min-length)
   (max-length :initarg :max-length :initform nil :reader slot-max-length)
   (minimum :initarg :minimum :initform nil :reader slot-minimum)
   (maximum :initarg :maximum :initform nil :reader slot-maximum)
   (element-type :initarg :element-type :initform nil :reader slot-element-type)
   (description :initarg :description :initform nil :reader slot-description)
   (dump :initarg :dump :initform t :reader slot-dump-p)
   (wire :initarg :wire :initform t :reader slot-wire-p)))

(defclass schema-effective-slot-definition (standard-effective-slot-definition)
  ((required :initarg :required :initform nil :accessor slot-required-p)
   (optional :initarg :optional :initform nil :accessor slot-optional-p)
   (key :initarg :key :initform nil :accessor %slot-key)
   (aliases :initarg :aliases :initform nil :accessor slot-aliases)
   (format :initarg :format :initform nil :accessor slot-format)
   (pattern :initarg :pattern :initform nil :accessor slot-pattern)
   (min-length :initarg :min-length :initform nil :accessor slot-min-length)
   (max-length :initarg :max-length :initform nil :accessor slot-max-length)
   (minimum :initarg :minimum :initform nil :accessor slot-minimum)
   (maximum :initarg :maximum :initform nil :accessor slot-maximum)
   (element-type :initarg :element-type :initform nil :accessor slot-element-type)
   (description :initarg :description :initform nil :accessor slot-description)
   (dump :initarg :dump :initform t :accessor slot-dump-p)
   (wire :initarg :wire :initform t :accessor slot-wire-p)))

(defclass schema-class (standard-class)
  ((extra :initarg :extra :initform :forbid :accessor schema-class-extra)
   (key-style :initarg :key-style :initform :downcase :accessor schema-class-key-style)
   (computes :initarg :computes :initform nil :accessor schema-class-computes))
  (:documentation "Metaclass for interchange schemas. Slot options carry wire metadata."))

(defmethod validate-superclass ((class schema-class) (super standard-class))
  t)

(defmethod validate-superclass ((class schema-class) (super schema-class))
  t)

(defmethod direct-slot-definition-class ((class schema-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'schema-direct-slot-definition))

(defmethod effective-slot-definition-class ((class schema-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'schema-effective-slot-definition))

(defun %copy-slot-options (from to)
  (dolist (slot '(required optional key aliases format pattern
                  min-length max-length minimum maximum element-type
                  description dump wire))
    (when (and (slot-exists-p from slot) (slot-boundp from slot)
               (slot-exists-p to slot))
      (setf (slot-value to slot) (slot-value from slot))))
  to)

(defmethod compute-effective-slot-definition ((class schema-class) name direct-slots)
  (declare (ignore name))
  (let ((effective (call-next-method))
        (source (find-if (lambda (d) (typep d 'schema-direct-slot-definition))
                         direct-slots)))
    (when source
      (%copy-slot-options source effective))
    effective))

(defun finalize-schema (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  class)

(defun %class-option (value)
  "DEFCLASS custom options arrive as the remaining forms (often a 1-list)."
  (if (and (consp value) (null (rest value)))
      (first value)
      value))

(defmethod initialize-instance :after ((class schema-class) &key name extra key-style &allow-other-keys)
  (when extra
    (setf (schema-class-extra class) (%class-option extra)))
  (when key-style
    (setf (schema-class-key-style class) (%class-option key-style)))
  (when name
    (setf (gethash name *schema-registry*) class)))

(defmethod reinitialize-instance :after ((class schema-class) &key name extra key-style &allow-other-keys)
  (when extra
    (setf (schema-class-extra class) (%class-option extra)))
  (when key-style
    (setf (schema-class-key-style class) (%class-option key-style)))
  (when name
    (setf (gethash name *schema-registry*) class)))

(defclass schema-object ()
  ((%extras :initform nil :accessor schema-extras :wire nil :dump nil :optional t))
  (:metaclass schema-class)
  (:documentation "Optional mixin. Internal extras bag for :extra :allow."))

(defun schema-class-p (object)
  (typep object 'schema-class))

(defun schema-name-p (name)
  (let ((class (and (symbolp name) (find-class name nil))))
    (and class (schema-class-p class))))

(defun find-schema (name &optional (errorp t))
  (let ((class (or (gethash name *schema-registry*)
                   (let ((c (find-class name nil)))
                     (and c (schema-class-p c) c)))))
    (cond
      (class class)
      (errorp (error 'schema-unknown :name name :message "not a schema-class"))
      (t nil))))

(defun list-schemas ()
  (loop for name being the hash-keys of *schema-registry*
        collect name))

(defgeneric schema-of (designator)
  (:documentation "Return the schema-class for a class, name, or instance."))

(defmethod schema-of ((designator schema-class))
  designator)

(defmethod schema-of ((designator symbol))
  (find-schema designator))

(defmethod schema-of ((designator schema-object))
  (class-of designator))

(defmethod schema-of ((designator standard-object))
  (let ((class (class-of designator)))
    (if (schema-class-p class)
        class
        (error 'schema-unknown :name (class-name class)
                               :message "instance is not a schema-class"))))

(defun schema-slots (schema)
  (class-slots (finalize-schema (schema-of schema))))

(defun schema-slot (schema name)
  (find name (schema-slots schema) :key #'slot-definition-name))

(defun %to-camel (string)
  (let ((parts (uiop:split-string string :separator "-_")))
    (if (null parts)
        string
        (apply #'concatenate 'string
               (first parts)
               (mapcar #'string-capitalize (rest parts))))))

(defun style-key (name style)
  (let ((s (string-downcase (string name))))
    (ecase style
      ((:downcase :kebab) (substitute #\- #\_ s))
      (:snake (substitute #\_ #\- s))
      (:camel (%to-camel s))
      (:preserve (string name)))))

(defun slot-wire-key (slot schema)
  (or (%slot-key slot)
      (style-key (slot-definition-name slot)
                 (%class-option (schema-class-key-style (schema-of schema))))))
