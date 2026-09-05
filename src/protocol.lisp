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

(defun schema-tag (class)
  (let ((class (schema-of class)))
    (or (schema-class-tag class)
        (loop for super in (class-direct-superclasses class)
              when (schema-class-p super)
                do (let ((tag (schema-tag super)))
                     (when tag (return tag)))))))

(defun discover-schema-subclasses (class)
  (let ((acc '()))
    (labels ((walk (c)
               (dolist (child (class-direct-subclasses c))
                 (when (schema-class-p child)
                   (walk child)
                   (push child acc)))))
      (walk (finalize-schema class)))
    (nreverse acc)))

(defun schema-variants (class)
  "Tagged variants: explicit :VARIANTS or discovered schema subclasses (deepest first)."
  (let* ((class (schema-of class))
         (explicit (schema-class-variants class)))
    (if explicit
        (mapcar #'find-schema explicit)
        (discover-schema-subclasses class))))

(defun variant-tag-values (variant tag-name)
  (let ((slot (schema-slot variant tag-name)))
    (unless slot
      (return-from variant-tag-values nil))
    (let ((spec (normalize-type-spec (slot-definition-type slot))))
      (case (type-kind spec)
        (:eql (list (second spec)))
        (:member (type-args spec))
        (:enum (enum-members spec))
        (t
         (when (slot-definition-initfunction slot)
           (list (funcall (slot-definition-initfunction slot)))))))))

(defun variant-accepts-tag-p (variant tag-name value)
  (let ((slot (schema-slot variant tag-name)))
    (when slot
      (let ((spec (slot-definition-type slot)))
        (or (schema-typep value spec)
            (find value (variant-tag-values variant tag-name) :test #'tag-matches-p))))))

(defun schema-variant (schema tag-value)
  (let* ((class (schema-of schema))
         (tag (schema-tag class)))
    (and tag
         (find-if (lambda (v) (variant-accepts-tag-p v tag tag-value))
                  (schema-variants class)))))

(defun canonicalize-tag (value spec)
  (let ((spec (normalize-type-spec spec)))
    (case (type-kind spec)
    (:enum (or (enum-canonical spec value) value))
    (:member (or (member-canonical spec value) value))
    (:eql (if (tag-matches-p (second spec) value) (second spec) value))
    (:keyword
     (cond
       ((keywordp value) value)
       ((stringp value) (intern (string-upcase value) :keyword))
       ((symbolp value) (intern (symbol-name value) :keyword))
       (t value)))
    (t value))))

(defun resolve-tagged-class (class table)
  (let ((tag-name (schema-tag class)))
    (unless tag-name
      (return-from resolve-tagged-class (values class nil)))
    (let ((variants (schema-variants class)))
      (unless variants
        (return-from resolve-tagged-class (values class nil)))
      (let ((slot (schema-slot class tag-name)))
        (unless slot
          (error 'schema-error
                 :message (format nil "tag slot ~S missing on ~S" tag-name (class-name class))))
        (multiple-value-bind (raw found) (lookup-field slot table class)
          (unless found
            (record-issue (list (slot-wire-key slot class)) "missing tag"
                          :slot tag-name)
            (return-from resolve-tagged-class (values nil t)))
          (let ((canonical (canonicalize-tag raw (slot-definition-type slot))))
            (let ((matched (find-if (lambda (v)
                                      (variant-accepts-tag-p v tag-name canonical))
                                    variants)))
              (unless matched
                (record-issue (list (slot-wire-key slot class)) "unknown tag"
                              :value raw :slot tag-name)
                (return-from resolve-tagged-class (values nil t)))
              (values matched nil))))))))

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

(defun %enum-conjunct (spec)
  (cond
    ((eq (type-kind spec) :enum) spec)
    ((and (consp spec) (eq (first spec) 'and))
     (find-if (lambda (s) (eq (type-kind s) :enum)) (rest spec)))
    (t nil)))

(defun inherited-enum-type (class slot-name)
  "Direct slot type on CLASS or a schema superclass that is an enum."
  (labels ((walk (c)
             (let ((d (find slot-name (class-direct-slots c)
                            :key #'slot-definition-name)))
               (when (and d (eq (type-kind (slot-definition-type d)) :enum))
                 (return-from inherited-enum-type (slot-definition-type d))))
             (dolist (s (class-direct-superclasses c))
               (when (schema-class-p s)
                 (walk s)))))
    (when (and class slot-name)
      (walk (schema-of class)))
    nil))

(defun parse-value (spec value schema slot path &key coerce)
  (let* ((enum-part (or (%enum-conjunct spec)
                        (and schema slot
                             (inherited-enum-type schema
                                                  (slot-definition-name slot)))))
         (spec (normalize-type-spec spec))
         (kind (type-kind spec)))
    (when enum-part
      (let ((canon (enum-canonical enum-part value)))
        (when canon (setf value canon))))
    (when (and coerce (member kind '(:integer :number :real :float :string :boolean :keyword
                                     :vector :list)))
      (multiple-value-bind (coerced ok) (coerce-scalar spec value)
        (when ok (setf value coerced))))
    (case kind
      (:or (return-from parse-value
             (parse-or spec value schema slot path :coerce coerce)))
      ((:vector :list :sequence)
       (return-from parse-value
         (parse-seq spec value schema slot path :coerce coerce)))
      (:nested
       (return-from parse-value
         (parse-nested spec value path :coerce coerce)))
      (:enum
       (let ((canon (enum-canonical spec value)))
         (unless canon
           (record-issue path (format nil "expected ~S" spec)
                         :value value
                         :slot (and slot (slot-definition-name slot)))
           (return-from parse-value value))
         (setf value canon)))
      (:member
       (let ((canon (member-canonical spec value)))
         (unless canon
           (record-issue path (format nil "expected ~S" spec)
                         :value value
                         :slot (and slot (slot-definition-name slot)))
           (return-from parse-value value))
         (setf value canon)))
      (:eql
       (unless (tag-matches-p (second spec) value)
         (record-issue path (format nil "expected ~S" spec)
                       :value value
                       :slot (and slot (slot-definition-name slot)))
         (return-from parse-value value))
       (setf value (second spec))))
    (when (and (not (member kind '(:or :vector :list :sequence :nested :enum :member :eql)))
               (not (schema-typep value spec)))
      (record-issue path (format nil "expected ~S" spec)
                    :value value
                    :slot (and slot (slot-definition-name slot)))
      (return-from parse-value value))
    (when (and slot (not (member kind '(:or :vector :list :sequence :nested))))
      (dolist (msg (check-constraints slot value))
        (record-issue path msg :value value :slot (slot-definition-name slot)))
      (validate-field (class-name (schema-of schema))
                      (slot-definition-name slot)
                      value))
    value))

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
         (policy (%class-option (or extra (schema-extra-policy class))))
         (table (table-from-source source))
         (consumed (make-hash-table :test #'equal))
         (initargs '())
         (extras (make-hash-table :test #'equal)))
    (finalize-schema class)
    (multiple-value-bind (resolved tag-error)
        (resolve-tagged-class class table)
      (when tag-error
        (return-from %parse-object (make-instance class)))
      (when (and resolved (not (eq resolved class)))
        (return-from %parse-object
          (%parse-object resolved source :coerce coerce :extra extra)))
      (when resolved
        (setf class resolved)))
    (dolist (slot (schema-slots class))
      (when (slot-wire-p slot)
        (let* ((name (slot-definition-name slot))
               (path (list (slot-wire-key slot class))))
          (multiple-value-bind (raw found key) (lookup-field slot table class)
            (declare (ignore key))
            (dolist (k (field-lookup-keys slot class))
              (when (nth-value 1 (gethash k table))
                (setf (gethash k consumed) t)))
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
      (when (eq policy :allow)
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
    (when (eq (schema-extra-policy class) :allow)
      (let ((bag (schema-extras object)))
        (when bag
          (maphash (lambda (k v)
                     (unless (nth-value 1 (gethash k out))
                       (setf (gethash k out) v)))
                   bag))))
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

(defgeneric json-schema (schema &key draft)
  (:documentation "Emit a JSON Schema document. Wrapper for (EMIT-SCHEMA SCHEMA :FORMAT :JSON).")
  (:method (schema &key (draft :draft-07))
    (emit-schema schema :format :json :draft draft)))

(defgeneric xsd-schema (schema &key version)
  (:documentation "Emit an XSD document. Wrapper for (EMIT-SCHEMA SCHEMA :FORMAT :XSD).")
  (:method (schema &key (version :1.0))
    (emit-schema schema :format :xsd :version version)))

(defgeneric arrow-schema (schema &key)
  (:documentation "Emit an Arrow schema. Wrapper for (EMIT-SCHEMA SCHEMA :FORMAT :ARROW).")
  (:method (schema &key)
    (emit-schema schema :format :arrow)))

(defgeneric avro-schema (schema &key)
  (:documentation "Emit an Avro schema. Wrapper for (EMIT-SCHEMA SCHEMA :FORMAT :AVRO).")
  (:method (schema &key)
    (emit-schema schema :format :avro)))

(defgeneric toml-schema (schema &key version)
  (:documentation "Emit a TOML Schema (.tosd) document. Wrapper for (EMIT-SCHEMA SCHEMA :FORMAT :TOML).")
  (:method (schema &key (version "1.0.0"))
    (emit-schema schema :format :toml :version version)))
