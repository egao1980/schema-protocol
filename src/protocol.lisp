(in-package #:schema-protocol)

(defvar *schema-issues* nil)
(defvar *schema-path* nil)
(defvar *schema-collecting* nil)

(defun slot-is-required-p (slot)
  (cond
    ((not (slot-wire-p slot)) nil)
    ((slot-optional-p slot) nil)
    ((slot-required-p slot) t)
    ((slot-definition-initfunction slot) nil)
    (t t)))

(defun normalize-path (path)
  (cond
    ((null path) nil)
    ((listp path) path)
    (t (list path))))

(defun record-issue (path message &key value slot)
  (push (make-schema-issue :path (append *schema-path* (normalize-path path))
                           :message message
                           :value value
                           :slot slot)
        *schema-issues*)
  nil)

(defun schema-issue (path message &key value slot)
  "Record a validation issue (collected during PARSE/VALIDATE)."
  (record-issue path message :value value :slot slot))

(defun schema-fail (path message &key value slot)
  "Signal immediately with a single issue."
  (error 'schema-validation-error
         :issues (list (make-schema-issue :path (append *schema-path* (normalize-path path))
                                          :message message
                                          :value value
                                          :slot slot))))

(defun %raise-issues ()
  (when *schema-issues*
    (error 'schema-validation-error :issues (nreverse *schema-issues*))))

(defgeneric validate-field (schema-name slot-name value)
  (:documentation "Hook after type/constraint checks. SCHEMA-NAME and SLOT-NAME are symbols.")
  (:method (schema-name slot-name value)
    (declare (ignore schema-name slot-name))
    value))

(defgeneric coerce-field (schema-name slot-name value)
  (:documentation "Optional per-slot coercion. Default returns VALUE.")
  (:method (schema-name slot-name value)
    (declare (ignore schema-name slot-name))
    value))

(defgeneric validate-object (object)
  (:documentation "Model-level checks. Specialize with :AFTER.")
  (:method ((object schema-object))
    object)
  (:method ((object standard-object))
    object))

(defun stringify-key (key)
  (etypecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))
    (character (string key))))

(defun table-from-source (source)
  (cond
    ((hash-table-p source)
     (let ((out (make-hash-table :test #'equal)))
       (maphash (lambda (k v) (setf (gethash (stringify-key k) out) v)) source)
       out))
    ((null source)
     (make-hash-table :test #'equal))
    ((and (listp source) (consp (first source)))
     (let ((out (make-hash-table :test #'equal)))
       (dolist (pair source out)
         (setf (gethash (stringify-key (car pair)) out) (cdr pair)))))
    ((listp source)
     (unless (evenp (length source))
       (error 'schema-error :message "plist source has odd length"))
     (let ((out (make-hash-table :test #'equal)))
       (loop for (k v) on source by #'cddr
             do (setf (gethash (stringify-key k) out) v))
       out))
    (t
     (error 'schema-error
            :message (format nil "cannot parse ~S as an object" (type-of source))))))

(defun %aliases-list (aliases)
  (cond
    ((null aliases) nil)
    ((and (consp aliases) (eq (first aliases) 'quote))
     (%aliases-list (second aliases)))
    ((and (consp aliases) (or (stringp (first aliases)) (symbolp (first aliases))))
     aliases)
    ((or (stringp aliases) (symbolp aliases))
     (list aliases))
    (t aliases)))

(defun field-lookup-keys (slot schema)
  (remove-duplicates
   (append (when (%slot-key slot) (list (stringify-key (%slot-key slot))))
           (mapcar #'stringify-key (%aliases-list (slot-aliases slot)))
           (list (slot-wire-key slot schema)
                 (string-downcase (symbol-name (slot-definition-name slot)))))
   :test #'equal))

(defun lookup-field (slot table schema)
  (dolist (k (field-lookup-keys slot schema) (values nil nil nil))
    (multiple-value-bind (v ok) (gethash k table)
      (when ok (return (values v t k))))))

(defun parse-or (spec value schema slot path &key coerce)
  (dolist (alt (%type-args spec)
           (progn
             (record-issue path (format nil "expected one of ~S" spec)
                           :value value
                           :slot (and slot (slot-definition-name slot)))
             value))
    (let ((*schema-issues* nil))
      (let ((got (parse-value alt value schema slot path :coerce coerce)))
        (unless *schema-issues*
          (return got))))))

(defun parse-seq (spec value schema slot path &key coerce)
  (let* ((kind (type-kind spec))
         (elt (sequence-element-type spec slot))
         (seq (cond
                ((and (eq kind :list) (listp value)) value)
                ((and (eq kind :vector) (vectorp value) (not (stringp value))) value)
                ((and coerce (eq kind :list) (vectorp value) (not (stringp value)))
                 (coerce value 'list))
                ((and coerce (eq kind :vector) (listp value))
                 (coerce value 'vector))
                ((and (eq kind :sequence) (typep value 'sequence) (not (stringp value)))
                 value)
                (t
                 (record-issue path (format nil "expected ~S" spec)
                               :value value
                               :slot (and slot (slot-definition-name slot)))
                 (return-from parse-seq value)))))
    (when slot
      (dolist (msg (check-constraints slot seq))
        (record-issue path msg :value seq :slot (slot-definition-name slot))))
    (if (null elt)
        seq
        (let ((out (map (if (eq kind :list) 'list 'vector)
                        (lambda (item i)
                          (parse-value elt item schema slot (append path (list i)) :coerce coerce))
                        seq
                        (loop for i from 0 below (length seq) collect i))))
          out))))

(defun parse-nested (spec value path &key coerce extra)
  (cond
    ((json-null-p value)
     (record-issue path (format nil "expected ~S, got null" spec) :value value)
     value)
    ((and (symbolp spec) (typep value spec))
     (validate value)
     value)
    (t
     (let ((*schema-path* (append *schema-path* (normalize-path path))))
       (parse spec value :coerce coerce :extra extra)))))

(defun parse-value (spec value schema slot path &key coerce)
  (let ((kind (type-kind spec)))
    (when (and coerce (member kind '(:integer :number :real :float :string :boolean :keyword
                                     :vector :list)))
      (multiple-value-bind (coerced ok) (coerce-scalar spec value)
        (when ok (setf value coerced))))
    (case kind
      (:or (parse-or spec value schema slot path :coerce coerce))
      ((:vector :list :sequence)
       (parse-seq spec value schema slot path :coerce coerce))
      (:nested
       (parse-nested spec value path :coerce coerce))
      (t
       (unless (schema-typep value spec)
         (record-issue path (format nil "expected ~S" spec)
                       :value value
                       :slot (and slot (slot-definition-name slot)))
         (return-from parse-value value))
       (when slot
         (dolist (msg (check-constraints slot value))
           (record-issue path msg :value value :slot (slot-definition-name slot)))
         (validate-field (class-name (schema-of schema))
                         (slot-definition-name slot)
                         value))
       value))))

(defun %maybe-decode (source format)
  (if (null format)
      source
      (let* ((pkg (find-package "SERDES-PROTOCOL"))
             (fn (and pkg (find-symbol "DECODE" pkg))))
        (unless fn
          (error 'schema-error :message "load serdes-protocol to use :format"))
        (funcall fn source :format format))))

(defun %maybe-encode (value format)
  (let* ((pkg (find-package "SERDES-PROTOCOL"))
         (fn (and pkg (find-symbol "ENCODE" pkg))))
    (unless fn
      (error 'schema-error :message "load serdes-protocol to use :format"))
    (funcall fn value :format format)))

(defun %parse-object (schema source &key coerce extra)
    (let* ((class (schema-of schema))
         (policy (%class-option (or extra (schema-class-extra class))))
         (table (table-from-source source))
         (consumed (make-hash-table :test #'equal))
         (initargs '())
         (extras (make-hash-table :test #'equal)))
    (finalize-schema class)
    (dolist (slot (schema-slots class))
      (when (slot-wire-p slot)
        (let* ((name (slot-definition-name slot))
               (path (list (slot-wire-key slot class))))
          (multiple-value-bind (raw found key) (lookup-field slot table class)
            (when key
              (setf (gethash key consumed) t))
            (restart-case
                (cond
                  ((not found)
                   (when (slot-is-required-p slot)
                     (record-issue path "required" :slot name)))
                  (t
                   (let ((v (if coerce
                                (coerce-field (class-name class) name raw)
                                raw)))
                     (setf v (parse-value (slot-definition-type slot) v class slot path
                                          :coerce coerce))
                     (let ((ia (first (slot-definition-initargs slot))))
                       (if ia
                           (setf initargs (list* ia v initargs))
                           (error 'schema-error
                                  :message (format nil "slot ~S has no initarg" name)))))))
              (use-value (value)
                :report "Use this value for the field"
                (let ((ia (first (slot-definition-initargs slot))))
                  (setf initargs (list* ia value initargs))))
              (skip-field ()
                :report "Leave this field unset")
              (use-default ()
                :report "Use the slot initform"))))))
    (maphash (lambda (k v)
               (unless (gethash k consumed)
                 (ecase policy
                   (:forbid (record-issue (list k) "unexpected field" :value v))
                   (:ignore nil)
                   (:allow (setf (gethash k extras) v)))))
             table)
    (let ((obj (apply #'make-instance class initargs)))
      (when (and (eq policy :allow) (plusp (hash-table-count extras)))
        (setf (schema-extras obj) extras))
      (validate-object obj)
      obj)))

(defun parse (schema source &key (coerce nil) (extra nil extra-p) format)
  "SOURCE (hash-table / plist / alist / instance) → schema instance.
   :COERCE T enables scalar coercions. :FORMAT delegates decode to serdes-protocol."
  (let ((source (%maybe-decode source format)))
    (when (and (typep source 'standard-object)
               (schema-class-p (class-of source)))
      (validate source)
      (return-from parse source))
    (flet ((body ()
             (%parse-object schema source
                            :coerce coerce
                            :extra (and extra-p extra))))
      (if *schema-collecting*
          (body)
          (let ((*schema-collecting* t)
                (*schema-issues* nil)
                (*schema-path* nil))
            (prog1 (body)
              (%raise-issues)))))))

(defun %validate-instance (object)
  (let ((class (schema-of object)))
    (finalize-schema class)
    (dolist (slot (schema-slots class))
      (when (slot-wire-p slot)
        (let ((name (slot-definition-name slot))
              (path (list (slot-wire-key slot class))))
          (cond
            ((not (slot-boundp object name))
             (when (slot-is-required-p slot)
               (record-issue path "required" :slot name)))
            (t
             (let ((v (slot-value object name))
                   (spec (slot-definition-type slot)))
               (case (type-kind spec)
                 (:nested
                  (unless (or (json-null-p v) (typep v spec))
                    (record-issue path (format nil "expected ~S" spec) :value v :slot name))
                  (when (typep v spec)
                    (let ((*schema-path* (append *schema-path* path)))
                      (%validate-instance v))))
                 ((:vector :list :sequence)
                  (unless (schema-typep v spec)
                    (record-issue path (format nil "expected ~S" spec) :value v :slot name))
                  (let ((elt (sequence-element-type spec slot)))
                    (when (and elt (nested-schema-type-p elt))
                      (loop for item across (coerce v 'vector)
                            for i from 0
                            do (when (typep item elt)
                                 (let ((*schema-path* (append *schema-path* path (list i))))
                                   (%validate-instance item)))))))
                 (t
                  (unless (schema-typep v spec)
                    (record-issue path (format nil "expected ~S" spec) :value v :slot name))
                  (dolist (msg (check-constraints slot v))
                    (record-issue path msg :value v :slot name))))))))))
    (validate-object object)
    object))

(defun validate (schema value &key coerce extra format)
  "Validate VALUE. Tables are parsed. Instances are checked in place."
  (cond
    ((and (typep value 'standard-object)
          (schema-class-p (class-of value)))
     (if *schema-collecting*
         (%validate-instance value)
         (let ((*schema-collecting* t)
               (*schema-issues* nil)
               (*schema-path* nil))
           (prog1 (%validate-instance value)
             (%raise-issues)))))
    (t
     (parse schema value :coerce coerce :extra extra :format format))))

(defun validp (schema value &key coerce extra)
  (handler-case
      (progn (validate schema value :coerce coerce :extra extra) t)
    (schema-validation-error () nil)))

(defun dump-value (value)
  (cond
    ((and (typep value 'standard-object) (schema-class-p (class-of value)))
     (dump value :as :hash-table))
    ((and (vectorp value) (not (stringp value)))
     (map 'vector #'dump-value value))
    ((listp value)
     (mapcar #'dump-value value))
    (t value)))

(defun compute-field-key (name schema)
  (style-key name (schema-class-key-style schema)))

(defun dump (object &key (as :hash-table) (include-computed t) format)
  "Instance → hash-table (default), :plist, or :alist. :FORMAT encodes via serdes."
  (let* ((class (schema-of object))
         (out (make-hash-table :test #'equal)))
    (finalize-schema class)
    (dolist (slot (schema-slots class))
      (when (and (slot-wire-p slot) (slot-dump-p slot))
        (let ((name (slot-definition-name slot)))
          (when (slot-boundp object name)
            (setf (gethash (slot-wire-key slot class) out)
                  (dump-value (slot-value object name)))))))
    (when include-computed
      (dolist (cname (schema-class-computes class))
        (when (fboundp cname)
          (setf (gethash (compute-field-key cname class) out)
                (dump-value (funcall cname object))))))
    (when (and (eq (%class-option (schema-class-extra class)) :allow)
               (schema-extras object))
      (maphash (lambda (k v)
                 (unless (nth-value 1 (gethash k out))
                   (setf (gethash k out) v)))
               (schema-extras object)))
    (let ((shaped (ecase as
                    (:hash-table out)
                    (:plist
                     (let ((acc '()))
                       (maphash (lambda (k v)
                                  (setf acc (list* (intern (string-upcase k) :keyword) v acc)))
                                out)
                       (nreverse acc)))
                    (:alist
                     (let ((acc '()))
                       (maphash (lambda (k v) (push (cons k v) acc)) out)
                       (nreverse acc))))))
      (if format
          (%maybe-encode shaped format)
          shaped))))

(defun %json-string-keys (&rest plist)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          when (and v (or (not (eq k :skip)) v))
            do (setf (gethash (if (stringp k) k (string-downcase (symbol-name k))) ht) v))
    ht))

(defun %set-json (table key value)
  (when value
    (setf (gethash key table) value))
  table)

(defun type-json-schema (spec slot defs)
  (let ((kind (type-kind spec)))
    (ecase kind
      (:any (make-hash-table :test #'equal))
      (:null (%json-string-keys "type" "null"))
      (:string
       (let ((ht (%json-string-keys "type" "string")))
         (%set-json ht "minLength" (and slot (slot-min-length slot)))
         (%set-json ht "maxLength" (and slot (slot-max-length slot)))
         (%set-json ht "pattern" (and slot (functionp (slot-pattern slot)) nil))
         (when (and slot (slot-format slot))
           (setf (gethash "format" ht)
                 (string-downcase (symbol-name (slot-format slot)))))
         (when (and slot (slot-description slot))
           (setf (gethash "description" ht) (slot-description slot)))
         ht))
      ((:integer :number :real :float)
       (let ((ht (%json-string-keys "type" (if (eq kind :integer) "integer" "number"))))
         (%set-json ht "minimum" (and slot (slot-minimum slot)))
         (%set-json ht "maximum" (and slot (slot-maximum slot)))
         ht))
      (:boolean (%json-string-keys "type" "boolean"))
      (:keyword (%json-string-keys "type" "string"))
      (:symbol (%json-string-keys "type" "string"))
      (:hash-table (%json-string-keys "type" "object"))
      (:member
       (let ((ht (%json-string-keys "enum" (coerce (%type-args spec) 'vector))))
         ht))
      (:eql (%json-string-keys "const" (second spec)))
      (:satisfies (%json-string-keys "type" "object"))
      (:lisp (make-hash-table :test #'equal))
      (:or
       (let ((ht (make-hash-table :test #'equal)))
         (setf (gethash "anyOf" ht)
               (map 'vector (lambda (s) (type-json-schema s slot defs)) (%type-args spec)))
         ht))
      ((:vector :list :sequence)
       (let ((ht (%json-string-keys "type" "array"))
             (elt (sequence-element-type spec slot)))
         (%set-json ht "minItems" (and slot (slot-min-length slot)))
         (%set-json ht "maxItems" (and slot (slot-max-length slot)))
         (when elt
           (setf (gethash "items" ht) (type-json-schema elt slot defs)))
         ht))
      (:nested
       (let* ((name spec)
              (ref (format nil "#/$defs/~A" (string-downcase (symbol-name name)))))
         (ensure-def name defs)
         (%json-string-keys "$ref" ref))))))

(defun object-json-schema (class defs)
  (finalize-schema class)
  (let ((props (make-hash-table :test #'equal))
        (required '()))
    (dolist (slot (schema-slots class))
      (when (and (slot-wire-p slot) (slot-dump-p slot))
        (let ((key (slot-wire-key slot class)))
          (setf (gethash key props)
                (type-json-schema (slot-definition-type slot) slot defs))
          (when (slot-is-required-p slot)
            (push key required)))))
    (dolist (cname (schema-class-computes class))
      (let ((key (compute-field-key cname class))
            (ht (%json-string-keys "readOnly" t)))
        (setf (gethash key props) ht)))
    (let ((ht (%json-string-keys "type" "object")))
      (setf (gethash "properties" ht) props)
      (when required
        (setf (gethash "required" ht) (coerce (nreverse required) 'vector)))
      (setf (gethash "additionalProperties" ht)
            (eq (%class-option (schema-class-extra class)) :allow))
      (let ((title (class-name class)))
        (setf (gethash "title" ht) (string-downcase (symbol-name title))))
      ht)))

(defun ensure-def (name defs)
  (let ((key (string-downcase (symbol-name name))))
    (unless (gethash key defs)
      (setf (gethash key defs) (make-hash-table :test #'equal))
      (setf (gethash key defs) (object-json-schema (find-schema name) defs)))
    key))

(defun json-schema (schema &key (draft :draft-07))
  "Emit a JSON Schema object (hash-table, string keys) for SCHEMA."
  (let* ((class (schema-of schema))
         (defs (make-hash-table :test #'equal))
         (root (object-json-schema class defs)))
    (setf (gethash "$schema" root)
          (ecase draft
            ((:draft-07 :draft7)
             "http://json-schema.org/draft-07/schema#")
            ((:draft-2020-12 :2020-12)
             "https://json-schema.org/draft/2020-12/schema")))
    (when (plusp (hash-table-count defs))
      (setf (gethash "$defs" root) defs))
    root))
