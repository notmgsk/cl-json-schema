(in-package #:cl-json-schema-tests)

(deftest test-string ()
  (let ((schema (yason:parse "{\"type\": \"string\"}")))
    (not-signals json-schema-error (validate (yason:parse "\"This is a string\"") schema))
    (not-signals json-schema-error (validate (yason:parse "\"Déjà vu\"") schema))
    (not-signals json-schema-error (validate (yason:parse "\"42\"") schema))
    (not-signals json-schema-error (validate (yason:parse "\"\"") schema))
    (let ((condition (signals json-schema-invalid-type-error
                       (validate 42 schema))))
      (is (= (json-schema-error-datum condition) 42))
      (is (string= (json-schema-error-expected-type condition) "string"))
      (is (string= (json-schema-error-invalid-type condition) "number")))))

(deftest test-string-length ()
  (let ((schema (yason:parse "{
  \"type\": \"string\",
  \"minLength\": 2,
  \"maxLength\": 3
}")))
    (signals json-schema-length-error (validate (yason:parse "\"A\"") schema))
    (not-signals json-schema-error (validate (yason:parse "\"AB\"") schema))
    (not-signals json-schema-error (validate (yason:parse "\"ABC\"") schema))
    (signals json-schema-length-error (validate (yason:parse "\"ABCD\"") schema))))

(deftest test-string-pattern ()
  (let ((schema (yason:parse "{
   \"type\": \"string\",
   \"pattern\": \"^(\\\\([0-9]{3}\\\\))?[0-9]{3}-[0-9]{4}$\"
}")))
    (not-signals json-schema-error
      (validate (yason:parse "\"555-1212\"") schema))
    (not-signals json-schema-error
      (validate (yason:parse "\"(888)555-1212\"") schema))
    (signals json-schema-pattern-error
      (validate (yason:parse "\"(888)555-1212 ext. 532\"") schema))
    (signals json-schema-pattern-error
      (validate (yason:parse "\"(800)FLOWERS\"") schema))))
