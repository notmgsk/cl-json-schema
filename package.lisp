;;;; package.lisp

(defpackage #:cl-json-schema
  (:use #:cl)
  (:import-from #:alexandria
                #:alist-hash-table
                #:assoc-value)
  (:import-from #:trivial-do
                #:dohash))
