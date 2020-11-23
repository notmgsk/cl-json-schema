(in-package #:cl-json-schema-tests)

(deftest test-boolean ()
  (let ((schema "{ \"type\": \"boolean\" }"))
    (not-signals json-schema-error
      (validate (yason:parse "true")
                (yason:parse schema)))
    (not-signals json-schema-error
      (validate (yason:parse "false")
                (yason:parse schema)))
    (let* ((datum "\"true\"")
           (condition (signals json-schema-invalid-type-error
                        (validate (yason:parse datum)
                                  (yason:parse schema)))))
      (is (string= (json-schema-error-expected-type condition) "boolean"))
      (is (string= (json-schema-error-invalid-type condition) "string")))
    (let* ((datum "0")
           (condition (signals json-schema-invalid-type-error
                        (validate (yason:parse datum)
                                  (yason:parse schema)))))
      (is (string= (json-schema-error-expected-type condition) "boolean"))
      (is (string= (json-schema-error-invalid-type condition) "number")))))
