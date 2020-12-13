;;;; package.lisp

(defpackage #:cl-json-schema
  (:use #:cl)
  (:import-from #:alexandria
                #:alist-hash-table
                #:assoc-value
                #:when-let*)
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
           #:validate))
