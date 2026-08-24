(in-package #:schema-protocol)

;;; Named interchange enums. Wire accepts the canonical keyword, its
;;; symbol-name (any case), a proto-style PREFIX_MEMBER alias, an
;;; optional integer, and explicit :aliases.
;;;
;;; Two members may share a number (proto allow_alias). The first keeps
;;; the number → canonical mapping; each name still parses to itself.

(defvar *enum-registry* (make-hash-table :test #'eq))

(defstruct (schema-enum (:constructor %make-schema-enum)
                        (:copier nil)
                        (:predicate schema-enum-p))
  name
  members
  numbers
  aliases)

(defun find-enum (name &optional (errorp t))
  (let ((e (and (symbolp name) (gethash name *enum-registry*))))
    (cond
      (e e)
      (errorp (error 'schema-unknown :name name :message "not a schema-enum"))
      (t nil))))

(defun enum-of (spec)
  (cond
    ((schema-enum-p spec) spec)
    ((and (consp spec) (eq (first spec) 'enum) (second spec))
     (find-enum (second spec) nil))
    ((symbolp spec) (find-enum spec nil))
    (t nil)))

(defun enum-members (spec)
  (let ((e (enum-of spec)))
    (and e (schema-enum-members e))))

(defun enum-number (spec member)
  (let* ((e (enum-of spec))
         (kw (and e (enum-canonical e member))))
    (and e kw (cdr (assoc kw (schema-enum-numbers e) :test #'eq)))))

(defun proto-enum-alias (enum-name member)
  (format nil "~A_~A"
          (substitute #\_ #\- (string-upcase (string enum-name)))
          (substitute #\_ #\- (string-upcase (string member)))))

(defun %enum-alias-key (value)
  (typecase value
    (integer value)
    (keyword value)
    (symbol (string-upcase (symbol-name value)))
    (string (string-upcase value))
    (t value)))

(defun %enum-add-alias (enum key canonical)
  (setf (gethash (%enum-alias-key key) (schema-enum-aliases enum)) canonical))

(defun %parse-enum-member (form)
  (cond
    ((keywordp form)
     (values form nil nil))
    ((and (consp form) (keywordp (first form)))
     (let ((name (first form))
           (rest (rest form))
           (number nil)
           (aliases nil))
       (when (integerp (first rest))
         (setf number (pop rest)))
       (let ((raw (getf rest :aliases)))
         (setf aliases (cond
                         ((null raw) nil)
                         ((and (consp raw) (eq (first raw) 'quote))
                          (let ((x (second raw)))
                            (if (listp x) x (list x))))
                         ((listp raw) raw)
                         (t (list raw)))))
       (values name number aliases)))
    (t
     (error 'schema-error
            :message (format nil "defenum: bad member ~S" form)))))

(defun register-enum (name members)
  (let ((enum (%make-schema-enum :name name
                                 :members '()
                                 :numbers '()
                                 :aliases (make-hash-table :test #'equal)))
        (seen-numbers (make-hash-table :test #'eql)))
    (dolist (form members)
      (multiple-value-bind (kw number aliases)
          (%parse-enum-member form)
        (push kw (schema-enum-members enum))
        (%enum-add-alias enum kw kw)
        (%enum-add-alias enum (symbol-name kw) kw)
        (%enum-add-alias enum (proto-enum-alias name kw) kw)
        (when (integerp number)
          (push (cons kw number) (schema-enum-numbers enum))
          (unless (gethash number seen-numbers)
            (setf (gethash number seen-numbers) kw)
            (%enum-add-alias enum number kw)))
        (dolist (alias aliases)
          (%enum-add-alias enum alias kw))))
    (setf (schema-enum-members enum) (nreverse (schema-enum-members enum)))
    (setf (schema-enum-numbers enum) (nreverse (schema-enum-numbers enum)))
    (setf (gethash name *enum-registry*) enum)
    enum))

(defun enum-canonical (spec value)
  "VALUE → canonical keyword, or NIL."
  (let ((enum (enum-of spec)))
    (unless enum
      (return-from enum-canonical nil))
    (or (gethash (%enum-alias-key value) (schema-enum-aliases enum))
        (and (or (stringp value) (symbolp value))
             (find value (schema-enum-members enum) :test #'string-equal)))))

(defmacro defenum (name &body body)
  "Define a named interchange enum (type specifier = NAME).

   (defenum color
     (:red 1)
     (:blue 2 :aliases (\"BLUE\" :b))
     (:azure 2))            ; proto allow_alias — 2 stays :blue

   Accepted on the wire: keyword, symbol-name (any case), proto
   NAME_MEMBER, the integer, and :aliases."
  (let ((doc (when (stringp (first body)) (first body)))
        (members (if (stringp (first body)) (rest body) body)))
    (declare (ignore doc))
    `(progn
       (register-enum ',name ',members)
       (deftype ,name ()
         (list* 'member (enum-members ',name)))
       ',name)))
