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
           #:validate))
