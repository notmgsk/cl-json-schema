;;;; package.lisp

(defpackage #:cl-json-schema
  (:use #:cl)
  (:nicknames #:json-schema)
  (:import-from #:alexandria
                #:alist-hash-table
                #:assoc-value
                #:when-let*
                #:if-let)
  (:import-from #:trivial-do
                #:dohash)
  (:export #:json-schema-error
           #:json-schema-additional-property-error
           #:json-schema-error-schema
           #:json-schema-error-property-name
           #:json-schema-error-invalid-type
           #:json-schema-invalid-type-error
           #:json-schema-error-datum
           #:json-schema-error-expected-type
           #:json-schema-properties-size-error
           #:json-schema-error-minimum-properties
           #:json-schema-error-maximum-properties
           #:json-schema-error-provided-properties
           #:json-schema-pattern-error
           #:json-schema-error-pattern
           #:json-schema-length-error
           #:json-schema-error-min-length
           #:json-schema-error-max-length
           #:json-schema-multipleof-error
           #:json-schema-error-multiple
           #:json-schema-range-error
           #:json-schema-error-minimum
           #:json-schema-error-exclusive-minimum
           #:json-schema-error-maximum
           #:json-schema-error-exclusive-maximum
           #:json-schema-invalid-schema-type-error
           #:json-schema-error-schema-type
           #:validate
           #:validate-integer
           #:validate-number
           #:validate-boolean
           #:validate-string
           #:validate-object))
