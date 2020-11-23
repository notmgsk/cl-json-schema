;;;; cl-json-schema.asd

(asdf:defsystem #:cl-json-schema
  :description "Describe cl-json-schema here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "cl-json-schema"))
  :depends-on (#:alexandria
               #:yason
               #:trivial-do
               #:cl-ppcre))
