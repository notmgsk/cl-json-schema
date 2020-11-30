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
           #:validate))
