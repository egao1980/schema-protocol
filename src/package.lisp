(defpackage #:schema-protocol
  (:use #:cl)
  (:nicknames #:stack-schema)
  (:import-from #:closer-mop
                #:class-slots
                #:class-direct-subclasses
                #:class-direct-superclasses
                #:class-direct-slots
                #:slot-definition-name
                #:slot-definition-type
                #:slot-definition-initargs
                #:slot-definition-initform
                #:slot-definition-initfunction
                #:class-finalized-p
                #:finalize-inheritance
                #:validate-superclass
                #:direct-slot-definition-class
                #:effective-slot-definition-class
                #:compute-effective-slot-definition
                #:standard-direct-slot-definition
                #:standard-effective-slot-definition)
  (:export
   ;; metaclass / objects
   #:schema-class
   #:schema-object
   #:schema-class-p
   #:schema-class-extra
   #:schema-extra-policy
   #:schema-class-key-style
   #:schema-class-computes
   #:schema-class-tag
   #:schema-class-variants
   #:schema-tag
   #:schema-variants
   #:schema-variant
   #:variant-tag-values
   #:schema-extras
   #:schema-of
   #:find-schema
   #:list-schemas
   #:schema-slots
   #:schema-slot
   #:finalize-schema
   #:type-kind
   #:type-args
   #:normalize-type-spec
   #:sequence-element-type
   #:nested-schema-type-p
   #:slot-is-required-p
   #:style-key
   #:slot-required-p
   #:slot-optional-p
   #:slot-wire-p
   #:slot-dump-p
   #:slot-wire-key
   #:slot-aliases
   #:slot-format
   #:slot-pattern
   #:slot-min-length
   #:slot-max-length
   #:slot-minimum
   #:slot-maximum
   #:slot-description
   #:slot-element-type

   ;; definition
   #:defschema
   #:defenum
   #:find-enum
   #:schema-enum
   #:schema-enum-p
   #:enum-of
   #:enum-members
   #:enum-canonical
   #:enum-number
   #:proto-enum-alias
   #:member-canonical
   #:tag-matches-p

   ;; protocol
   #:validate
   #:validp
   #:parse
   #:dump
   #:json-schema
   #:xsd-schema
   #:validate-object
   #:validate-field
   #:coerce-field
   #:schema-issue
   #:schema-fail

   ;; conditions
   #:schema-error
   #:schema-error-message
   #:schema-validation-error
   #:schema-validation-error-issues
   #:schema-unknown
   #:schema-unknown-name
   #:schema-issue-path
   #:schema-issue-message
   #:schema-issue-value
   #:schema-issue-slot
   #:make-schema-issue))

(in-package #:schema-protocol)
