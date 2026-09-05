(in-package #:schema-protocol)

(defvar *schema-format* :json
  "Default schema-document format keyword.")

(defvar *schema-backend* nil
  "Optional current backend object for *SCHEMA-FORMAT*.")

(defvar *schema-formats* (make-hash-table :test #'eq)
  "Map normalized format keywords to backend objects.")

(defclass schema-format-backend () ()
  (:documentation "Base class for schema-document format backends."))

(defgeneric backend-emit-schema (backend schema &key)
  (:documentation "Emit SCHEMA as this backend's schema document."))

(defgeneric backend-parse-schema (backend source &key)
  (:documentation "Parse SOURCE (JSON Schema / XSD / …) into a schema-class."))

(defun %normalize-schema-format (format)
  (etypecase format
    (keyword format)
    (symbol (intern (symbol-name format) :keyword))
    (string (intern (string-upcase format) :keyword))))

(defun register-schema-format (format backend)
  "Register BACKEND for FORMAT and return BACKEND."
  (check-type backend schema-format-backend)
  (let ((normalized (%normalize-schema-format format)))
    (setf (gethash normalized *schema-formats*) backend)
    backend))

(defun find-schema-backend (format &optional (errorp t))
  "Return the backend registered for FORMAT.
When ERRORP is true, signal SCHEMA-UNKNOWN-FORMAT when the registry has no backend."
  (let* ((normalized (%normalize-schema-format format))
         (backend (gethash normalized *schema-formats*)))
    (cond
      (backend backend)
      (errorp
       (error 'schema-unknown-format
              :format normalized
              :message "load or register a backend for this format"))
      (t nil))))

(defun %schema-backend-for (format)
  (let ((normalized (%normalize-schema-format format)))
    (or (and *schema-backend*
             (eq normalized (%normalize-schema-format *schema-format*))
             *schema-backend*)
        (find-schema-backend normalized))))

(defun emit-schema (schema &rest args &key (format *schema-format*) &allow-other-keys)
  "Emit SCHEMA as a schema document using the backend registered for FORMAT."
  (let ((keys (copy-list args)))
    (remf keys :format)
    (apply #'backend-emit-schema (%schema-backend-for format) schema keys)))

(defun parse-schema (source &rest args &key (format *schema-format*) &allow-other-keys)
  "Parse a schema document (JSON Schema / XSD / …) into a schema-class.
   Distinct from PARSE (instance values) and from avro-protocol:PARSE-SCHEMA
   (Avro writer schema for the binary codec)."
  (let ((keys (copy-list args)))
    (remf keys :format)
    (apply #'backend-parse-schema (%schema-backend-for format) source keys)))
