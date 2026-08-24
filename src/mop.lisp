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
  ((extra :initarg :extra :initform nil :accessor schema-class-extra)
   (key-style :initarg :key-style :initform :downcase :accessor schema-class-key-style)
   (computes :initarg :computes :initform nil :accessor schema-class-computes)
   (tag :initarg :tag :initform nil :accessor schema-class-tag)
   (variants :initarg :variants :initform nil :accessor schema-class-variants))
  (:documentation "Metaclass for interchange schemas. Slot options carry wire metadata.
   :TAG names the discriminator slot; subclasses (or :VARIANTS) are the union.
   :EXTRA is inherited; NIL means take the parent policy, else default :FORBID."))

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
      (%copy-slot-options source effective)
      ;; Prefer the most specific direct type. CLOS intersection of
      ;; KEYWORD with (EQL "circ") is empty — useless for tagged slots.
      (let ((spec (slot-definition-type source)))
        (when spec
          (setf (slot-definition-type effective) spec))))
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

(defun %apply-tag-option (class value)
  (let ((v value))
    (when (and (consp v) (null (rest v)) (symbolp (first v)))
      (setf v (first v)))
    (cond
      ((null v) nil)
      ((symbolp v)
       (setf (schema-class-tag class) v))
      ((and (consp v) (symbolp (first v)))
       (setf (schema-class-tag class) (first v))
       (when (rest v)
         (setf (schema-class-variants class) (rest v))))
      (t
       (error 'schema-error
              :message (format nil "bad :tag option ~S" value))))))

(defmethod initialize-instance :after ((class schema-class) &key name extra key-style tag variants &allow-other-keys)
  (when extra
    (setf (schema-class-extra class) (%class-option extra)))
  (when key-style
    (setf (schema-class-key-style class) (%class-option key-style)))
  (when tag
    (%apply-tag-option class tag))
  (when variants
    (setf (schema-class-variants class) (%class-option variants)))
  (when name
    (setf (gethash name *schema-registry*) class)))

(defmethod reinitialize-instance :after ((class schema-class) &key name extra key-style tag variants &allow-other-keys)
  (when extra
    (setf (schema-class-extra class) (%class-option extra)))
  (when key-style
    (setf (schema-class-key-style class) (%class-option key-style)))
  (when tag
    (%apply-tag-option class tag))
  (when variants
    (setf (schema-class-variants class) (%class-option variants)))
  (when name
    (setf (gethash name *schema-registry*) class)))

(defclass schema-object ()
  ((%extras :initform nil :initarg :extras :wire nil :dump nil :optional t))
  (:metaclass schema-class)
  (:documentation "Mixin. %EXTRAS is the leftover-field bag when :EXTRA :ALLOW."))

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

(defun schema-extra-policy (schema)
  "Resolved :EXTRA policy: :FORBID (default), :IGNORE, or :ALLOW.
   Walks schema superclasses when the class did not set :EXTRA."
  (labels ((walk (class)
             (let ((raw (%class-option (schema-class-extra class))))
               (if raw
                   raw
                   (loop for super in (class-direct-superclasses class)
                         when (schema-class-p super)
                           do (let ((p (walk super)))
                                (when p (return p))))))))
    (or (walk (schema-of schema)) :forbid)))

(defun extras-table (value)
  "Hash-table / alist / plist → equal hash-table with string keys. NIL → empty table."
  (cond
    ((null value)
     (make-hash-table :test #'equal))
    ((hash-table-p value)
     (let ((out (make-hash-table :test #'equal)))
       (maphash (lambda (k v) (setf (gethash (stringify-key k) out) v)) value)
       out))
    ((and (listp value) (consp (first value)))
     (let ((out (make-hash-table :test #'equal)))
       (dolist (pair value out)
         (setf (gethash (stringify-key (car pair)) out) (cdr pair)))))
    ((listp value)
     (unless (evenp (length value))
       (error 'schema-error :message "extras plist has odd length"))
     (let ((out (make-hash-table :test #'equal)))
       (loop for (k v) on value by #'cddr
             do (setf (gethash (stringify-key k) out) v))
       out))
    (t
     (error 'schema-error
            :message (format nil "extras must be a hash-table, alist, or plist, got ~S"
                             (type-of value))))))

(defun extras-alist (table)
  (let ((acc '()))
    (maphash (lambda (k v) (push (cons k v) acc)) table)
    (nreverse acc)))

(defun schema-extras (object &key (as :hash-table))
  "Leftover fields (Pydantic model_extra). :AS is :HASH-TABLE or :ALIST.
   :ALLOW schemas lazily allocate an empty table. :FORBID/:IGNORE return NIL."
  (unless (slot-exists-p object '%extras)
    (return-from schema-extras nil))
  (let ((raw (if (slot-boundp object '%extras)
                 (slot-value object '%extras)
                 nil)))
    (when (and (null raw) (eq (schema-extra-policy object) :allow))
      (setf raw (make-hash-table :test #'equal)
            (slot-value object '%extras) raw))
    (ecase as
      (:hash-table raw)
      (:alist (and raw (extras-alist raw))))))

(defun (setf schema-extras) (new object)
  (unless (slot-exists-p object '%extras)
    (error 'schema-error :message "object has no extras bag"))
  (setf (slot-value object '%extras)
        (if (null new) nil (extras-table new))))

(defun %known-initargs (class)
  (let ((keys '(:extras :allow-other-keys)))
    (dolist (slot (class-slots (finalize-schema class)) keys)
      (dolist (ia (slot-definition-initargs slot))
        (pushnew ia keys)))))

(defmethod make-instance ((class schema-class) &rest initargs)
  (finalize-schema class)
  (let ((policy (schema-extra-policy class)))
    (if (eq policy :forbid)
        (apply #'call-next-method class initargs)
        (let ((known (%known-initargs class))
              (kept '())
              (bag (make-hash-table :test #'equal))
              (explicit nil)
              (explicit-p nil))
          (loop for (k v) on initargs by #'cddr
                do (cond
                     ((eq k :extras)
                      (setf explicit v explicit-p t))
                     ((eq k :allow-other-keys) nil)
                     ((member k known)
                      (setf kept (list* k v kept)))
                     ((eq policy :allow)
                      (setf (gethash (stringify-key k) bag) v))
                     (t nil)))
          (when (or explicit-p (plusp (hash-table-count bag)))
            (let ((merged (extras-table (if explicit-p explicit nil))))
              (maphash (lambda (k v) (setf (gethash k merged) v)) bag)
              (when (eq policy :allow)
                (setf kept (list* :extras merged kept)))))
          (apply #'call-next-method class kept)))))

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
