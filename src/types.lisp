(in-package #:schema-protocol)

;;; Schema types are Lisp type specifiers plus a small interchange set:
;;;   string integer number real float boolean keyword symbol hash-table t :any :null
;;;   (integer lo hi) (or ...) (member ...) (eql ...) (satisfies pred)
;;;   (vector element-type) (list element-type)
;;;   class-name → nested schema
;;;
;;; `null` is treated as JSON null (:null), not the Lisp NIL type.
;;; JSON false is Lisp NIL and uses BOOLEAN.

(defun %type-head (spec)
  (if (consp spec) (first spec) spec))

(defun %type-args (spec)
  (if (consp spec) (rest spec) nil))

(defun nested-schema-type-p (spec)
  (let ((name (if (consp spec) nil spec)))
    (and (symbolp name) (schema-name-p name))))

(defun type-kind (spec)
  (let ((head (%type-head spec)))
    (cond
      ((member head '(t :any)) :any)
      ((member head '(:null null)) :null)
      ((eq head 'string) :string)
      ((eq head 'integer) :integer)
      ((eq head 'number) :number)
      ((eq head 'real) :real)
      ((eq head 'float) :float)
      ((eq head 'boolean) :boolean)
      ((eq head 'keyword) :keyword)
      ((eq head 'symbol) :symbol)
      ((eq head 'hash-table) :hash-table)
      ((eq head 'or) :or)
      ((eq head 'member) :member)
      ((eq head 'eql) :eql)
      ((eq head 'vector) :vector)
      ((eq head 'list) :list)
      ((eq head 'sequence) :sequence)
      ((eq head 'satisfies) :satisfies)
      ((and (symbolp head) (schema-name-p head)) :nested)
      (t :lisp))))

(defun json-null-p (value)
  (eq value :null))

(defun schema-boolean-p (value)
  (or (eq value t) (null value)))

(defun %in-range (value minimum maximum)
  (and (or (null minimum) (>= value minimum))
       (or (null maximum) (<= value maximum))))

(defun %length-ok (value min-length max-length)
  (let ((n (length value)))
    (and (or (null min-length) (>= n min-length))
         (or (null max-length) (<= n max-length)))))

(defun %valid-email-p (string)
  (let ((at (position #\@ string)))
    (and at
         (plusp at)
         (< at (1- (length string)))
         (position #\. string :start (1+ at)))))

(defun %valid-uri-p (string)
  (let ((colon (position #\: string)))
    (and colon (plusp colon) (< colon (1- (length string))))))

(defun %valid-uuid-p (string)
  (and (= 36 (length string))
       (char= #\- (char string 8))
       (char= #\- (char string 13))
       (char= #\- (char string 18))
       (char= #\- (char string 23))
       (every (lambda (c)
                (or (digit-char-p c 16) (char= c #\-)))
              string)))

(defun %valid-date-time-p (string)
  (and (>= (length string) 19)
       (digit-char-p (char string 0))
       (char= #\- (char string 4))
       (or (find #\T string) (find #\Space string))))

(defun check-format (format value)
  (if (null format)
      t
      (and (stringp value)
           (ecase format
             (:email (%valid-email-p value))
             (:uri (%valid-uri-p value))
             (:uuid (%valid-uuid-p value))
             ((:date-time :datetime) (%valid-date-time-p value))))))

(defun check-pattern (pattern value)
  (cond
    ((null pattern) t)
    ((functionp pattern) (not (null (funcall pattern value))))
    ((and (symbolp pattern) (fboundp pattern))
     (not (null (funcall pattern value))))
    (t (error 'schema-error
              :message (format nil ":pattern must be a function designator, got ~S" pattern)))))

(defun check-constraints (slot value)
  (let ((issues '()))
    (flet ((fail (msg)
             (push msg issues)))
      (when (stringp value)
        (unless (%length-ok value (slot-min-length slot) (slot-max-length slot))
          (fail (format nil "string length not in [~A,~A]"
                        (slot-min-length slot) (slot-max-length slot))))
        (unless (check-format (slot-format slot) value)
          (fail (format nil "failed format ~S" (slot-format slot))))
        (unless (check-pattern (slot-pattern slot) value)
          (fail "failed pattern")))
      (when (or (vectorp value) (listp value))
        (unless (stringp value)
          (unless (%length-ok value (slot-min-length slot) (slot-max-length slot))
            (fail (format nil "sequence length not in [~A,~A]"
                          (slot-min-length slot) (slot-max-length slot))))))
      (when (realp value)
        (unless (%in-range value (slot-minimum slot) (slot-maximum slot))
          (fail (format nil "number not in [~A,~A]"
                        (slot-minimum slot) (slot-maximum slot)))))
      (nreverse issues))))

(defun lisp-type-for (spec)
  "Best-effort CL type for TYPEP of non-nested scalars."
  (let ((kind (type-kind spec)))
    (case kind
      (:any t)
      (:null '(eql :null))
      (:string 'string)
      (:integer 'integer)
      (:number 'number)
      (:real 'real)
      (:float 'float)
      (:boolean t) ; checked separately
      (:keyword 'keyword)
      (:symbol 'symbol)
      (:hash-table 'hash-table)
      (:member spec)
      (:eql spec)
      (:satisfies spec)
      (:lisp spec)
      (t t))))

(defun schema-typep (value spec)
  (let ((kind (type-kind spec)))
    (ecase kind
      (:any t)
      (:null (json-null-p value))
      (:string (stringp value))
      (:integer (and (integerp value) (or (atom spec) (typep value spec))))
      (:number (and (numberp value) (or (atom spec) (typep value spec))))
      (:real (and (realp value) (or (atom spec) (typep value spec))))
      (:float (and (floatp value) (or (atom spec) (typep value spec))))
      (:boolean (schema-boolean-p value))
      (:keyword (keywordp value))
      (:symbol (symbolp value))
      (:hash-table (hash-table-p value))
      (:member (typep value spec))
      (:eql (typep value spec))
      (:satisfies (typep value spec))
      (:lisp (typep value spec))
      (:or (some (lambda (s) (schema-typep value s)) (%type-args spec)))
      (:vector (and (vectorp value) (not (stringp value))))
      (:list (listp value))
      (:sequence (typep value 'sequence))
      (:nested (or (json-null-p value)
                   (typep value spec)
                   (hash-table-p value)
                   (and (listp value) (or (null value) (consp (first value)) (keywordp (first value)))))))))

(defun coerce-scalar (spec value)
  "Return (values new-value t) or (values value nil)."
  (let ((kind (type-kind spec)))
    (cond
      ((schema-typep value spec) (values value t))
      ((eq kind :integer)
       (cond
         ((stringp value)
          (let ((n (ignore-errors (parse-integer value :junk-allowed nil))))
            (if n (values n t) (values value nil))))
         ((and (realp value) (= value (truncate value)))
          (values (truncate value) t))
         (t (values value nil))))
      ((member kind '(:number :real :float))
       (if (stringp value)
           (let ((n (ignore-errors
                     (let ((*read-eval* nil))
                       (let ((v (read-from-string value)))
                         (and (numberp v) v))))))
             (if n (values n t) (values value nil)))
           (values value nil)))
      ((eq kind :string)
       (cond
         ((symbolp value) (values (string-downcase (symbol-name value)) t))
         ((numberp value) (values (princ-to-string value) t))
         (t (values value nil))))
      ((eq kind :boolean)
       (cond
         ((stringp value)
          (let ((x (string-downcase value)))
            (cond
              ((member x '("true" "1" "yes" "on") :test #'string=) (values t t))
              ((member x '("false" "0" "no" "off") :test #'string=) (values nil t))
              (t (values value nil)))))
         ((integerp value) (values (not (zerop value)) t))
         (t (values value nil))))
      ((eq kind :keyword)
       (cond
         ((stringp value) (values (intern (string-upcase value) :keyword) t))
         ((and (symbolp value) (not (keywordp value)))
          (values (intern (symbol-name value) :keyword) t))
         (t (values value nil))))
      ((eq kind :vector)
       (if (listp value)
           (values (coerce value 'vector) t)
           (values value nil)))
      ((eq kind :list)
       (if (and (vectorp value) (not (stringp value)))
           (values (coerce value 'list) t)
           (values value nil)))
      (t (values value nil)))))

(defun sequence-element-type (spec slot)
  (or (and slot (slot-element-type slot))
      (let ((args (%type-args spec)))
        (when args (first args)))))
