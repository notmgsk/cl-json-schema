(asdf:defsystem #:cl-json-schema-tests
  :description ""
  :author ""
  :license ""
  :depends-on (#:cl-json-schema
               #:fiasco)
  :perform (asdf:test-op
            (o s)
            (uiop:symbol-call
             ':cl-json-schema-tests '#:run-cl-json-schema-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "tests")
               (:file "test-objects")))
