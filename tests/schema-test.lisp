(in-package #:schema-protocol/tests)

(defun %ht (&rest plist)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash k ht) v))
    ht))

(defun %issues (thunk)
  (handler-case (progn (funcall thunk) nil)
    (schema-validation-error (e)
      (schema-validation-error-issues e))))

(deftest parse-required-and-optional
  (defschema %person-1 ()
    (name string)
    (age integer :optional t))
  (let ((p (parse '%person-1 (%ht "name" "ada"))))
    (ok (equal "ada" (slot-value p 'name)))
    (ng (slot-boundp p 'age)))
  (ok (signals (parse '%person-1 (%ht)) 'schema-validation-error))
  (let ((issues (%issues (lambda () (parse '%person-1 (%ht "age" 1))))))
    (ok (equal '("name") (schema-issue-path (first issues))))
    (ok (equal "required" (schema-issue-message (first issues))))))

(deftest parse-plist-and-defaults
  (defschema %item-1 ()
    (sku string)
    (qty integer :default 1))
  (let ((i (parse '%item-1 '(:sku "abc"))))
    (ok (equal "abc" (slot-value i 'sku)))
    (ok (= 1 (slot-value i 'qty)))))

(deftest extra-forbid-ignore-allow
  (defschema %strict-1 ()
    (name string)
    (:extra :forbid))
  (ok (signals (parse '%strict-1 (%ht "name" "a" "x" 1)) 'schema-validation-error))
  (defschema %lax-1 ()
    (name string)
    (:extra :ignore))
  (ok (equal "a" (slot-value (parse '%lax-1 (%ht "name" "a" "x" 1)) 'name)))
  (defschema %bag-1 ()
    (name string)
    (:extra :allow))
  (let ((o (parse '%bag-1 (%ht "name" "a" "x" 1))))
    (ok (equal 1 (gethash "x" (schema-extras o))))))

(deftest nested-and-vector
  (defschema %addr-1 ()
    (city string))
  (defschema %user-1 ()
    (name string)
    (address %addr-1)
    (tags (vector string)))
  (let* ((u (parse '%user-1
                   (%ht "name" "ada"
                        "address" (%ht "city" "London")
                        "tags" #("lisp" "clos"))))
         (d (dump u)))
    (ok (equal "ada" (slot-value u 'name)))
    (ok (equal "London" (slot-value (slot-value u 'address) 'city)))
    (ok (equalp #("lisp" "clos") (slot-value u 'tags)))
    (ok (equal "London" (gethash "city" (gethash "address" d))))
    (ok (equalp #("lisp" "clos") (gethash "tags" d)))))

(deftest union-null
  (defschema %note-1 ()
    (title string)
    (body (or :null string) :optional t))
  (let ((n (parse '%note-1 (%ht "title" "t" "body" :null))))
    (ok (eq :null (slot-value n 'body))))
  (let ((n (parse '%note-1 (%ht "title" "t" "body" "hi"))))
    (ok (equal "hi" (slot-value n 'body)))))

(deftest constraints-and-format
  (defschema %acct-1 ()
    (email string :format :email)
    (age (integer 0 120)))
  (ok (parse '%acct-1 (%ht "email" "ada@ex.com" "age" 36)))
  (ok (signals (parse '%acct-1 (%ht "email" "nope" "age" 36))
               'schema-validation-error))
  (ok (signals (parse '%acct-1 (%ht "email" "ada@ex.com" "age" -1))
               'schema-validation-error)))

(deftest member-enum
  (defschema %flag-1 ()
    (color (member :red :blue)))
  (let ((o (parse '%flag-1 (%ht "color" :red))))
    (ok (eq :red (slot-value o 'color))))
  (ok (signals (parse '%flag-1 (%ht "color" :green))
               'schema-validation-error)))

(deftest coerce-scalars
  (defschema %co-1 ()
    (n integer)
    (ok? boolean)
    (kw keyword))
  (let ((o (parse '%co-1 (%ht "n" "12" "ok?" "true" "kw" "foo") :coerce t)))
    (ok (= 12 (slot-value o 'n)))
    (ok (eq t (slot-value o 'ok?)))
    (ok (eq :foo (slot-value o 'kw))))
  (ok (signals (parse '%co-1 (%ht "n" "12" "ok?" "true" "kw" "foo"))
               'schema-validation-error)))

(deftest computed-and-validate-hook
  (defschema %comp-1 ()
    (first-name string)
    (last-name string)
    (:compute display-name (self)
      (format nil "~A ~A" (slot-value self 'first-name) (slot-value self 'last-name)))
    (:validate (self)
      (when (equal (slot-value self 'first-name) (slot-value self 'last-name))
        (schema-issue 'first-name "names must differ"))))
  (let ((o (parse '%comp-1 (%ht "first-name" "Ada" "last-name" "Lovelace"))))
    (ok (equal "Ada Lovelace" (display-name o)))
    (ok (equal "Ada Lovelace" (gethash "display-name" (dump o)))))
  (ok (signals (parse '%comp-1 (%ht "first-name" "X" "last-name" "X"))
               'schema-validation-error)))

(deftest key-alias-and-style
  (defschema %alias-1 ()
    (display-name string :key "displayName" :aliases ("name"))
    (:key-style :camel))
  (ok (equal "z" (slot-value (parse '%alias-1 (%ht "displayName" "z")) 'display-name)))
  (ok (equal "z" (slot-value (parse '%alias-1 (%ht "name" "z")) 'display-name)))
  (ok (equal "z" (gethash "displayName" (dump (parse '%alias-1 (%ht "name" "z")))))))

(deftest inheritance
  (defschema %animal-1 ()
    (name string))
  (defschema %dog-1 (%animal-1)
    (breed string))
  (let ((d (parse '%dog-1 (%ht "name" "fido" "breed" "whippet"))))
    (ok (equal "fido" (slot-value d 'name)))
    (ok (equal "whippet" (slot-value d 'breed))))
  (ok (signals (parse '%dog-1 (%ht "breed" "whippet"))
               'schema-validation-error)))

(deftest validate-instance-and-validp
  (defschema %v-1 ()
    (n integer :minimum 0))
  (let ((o (make-instance '%v-1 :n 3)))
    (ok (eq o (validate '%v-1 o)))
    (ok (validp '%v-1 o)))
  (let ((o (make-instance '%v-1 :n -2)))
    (ok (not (validp '%v-1 o)))
    (ok (signals (validate '%v-1 o) 'schema-validation-error))))

(deftest restart-use-value
  (defschema %r-1 ()
    (name string)
    (:validate-field name (value)
      (if (equal value "bad")
          (schema-fail 'name "bad")
          value)))
  (let ((got (handler-bind ((schema-validation-error
                             (lambda (c)
                               (declare (ignore c))
                               (invoke-restart 'use-value "ada"))))
               (parse '%r-1 (%ht "name" "bad")))))
    (ok (equal "ada" (slot-value got 'name)))))

(deftest field-validator-method
  (defschema %fv-1 ()
    (email string)
    (:validate-field email (value)
      (unless (find #\@ value)
        (schema-issue 'email "need @"))
      value))
  (ok (parse '%fv-1 (%ht "email" "a@b.c")))
  (ok (signals (parse '%fv-1 (%ht "email" "nope"))
               'schema-validation-error)))
