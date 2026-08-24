(defsystem "schema-protocol"
  :version "0.1.0"
  :description "CLOS schema protocol for cl-stack — interchange models, validation, nested schemas, JSON Schema emit"
  :author "egao1980"
  :license "MIT"
  :depends-on ("closer-mop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "mop")
               (:file "types")
               (:file "protocol")
               (:file "defschema"))
  :in-order-to ((test-op (test-op "schema-protocol/tests"))))

(defsystem "schema-protocol/tests"
  :depends-on ("schema-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "schema-test")
               (:file "json-schema-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
