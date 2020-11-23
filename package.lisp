;;;; package.lisp

(defpackage #:cl-json-schema
  (:use #:cl)
  (:import-from #:alexandria
                #:alist-hash-table
                #:assoc-value
                #:when-let*)
  (:import-from #:trivial-do
                #:dohash))
