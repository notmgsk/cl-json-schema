;;;; cl-json-schema.asd

(asdf:defsystem #:cl-json-schema
  :description "Describe cl-json-schema here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-json-schema-tests)))
  :serial t
  :components ((:file "package")
               (:file "errors")
               (:file "cl-json-schema"))
  :depends-on (#:alexandria
               #:yason
               #:trivial-do
               #:cl-ppcre))
