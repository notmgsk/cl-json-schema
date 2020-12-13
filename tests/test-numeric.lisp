(in-package #:cl-json-schema-tests)

(deftest test-integer ()
  (let ((schema (yason:parse "{\"type\": \"integer\"}")))
    (not-signals json-schema-error (validate 42 schema))
    (not-signals json-schema-error (validate -1 schema))
    (signals json-schema-invalid-type-error
      (validate 3.141 schema))
    (signals json-schema-invalid-type-error
      (validate (yason:parse "\"42\"") schema))))

(deftest test-number ()
  (let ((schema (yason:parse "{\"type\": \"number\"}")))
    (not-signals json-schema-error (validate 42 schema))
    (not-signals json-schema-error (validate -1 schema))
    (not-signals json-schema-error (validate (yason:parse "2.99e8") schema))
    (not-signals json-schema-error (validate 3.141 schema))
    (signals json-schema-invalid-type-error
      (validate (yason:parse "\"42\"") schema))))

(deftest test-multiple-of ()
  (let ((schema (yason:parse "{\"type\": \"number\", \"multipleOf\": 1.0}")))
    (not-signals json-schema-error (validate 42 schema))
    (not-signals json-schema-error (validate 42.0 schema))
    (signals json-schema-multipleof-error
      (validate 3.141 schema)))
  (let ((schema (yason:parse "{\"type\": \"number\", \"multipleOf\": 10}")))
    (not-signals json-schema-error (validate 10 schema))
    (not-signals json-schema-error (validate 20.0 schema))
    (signals json-schema-multipleof-error
      (validate 23 schema))))

(deftest test-range ()
  (let ((schema (yason:parse "{
  \"type\": \"number\",
  \"minimum\": 0,
  \"exclusiveMaximum\": 100
}")))
    (signals json-schema-range-error
      (validate -1 schema))
    (not-signals json-schema-error
      (validate 0 schema))
    (not-signals json-schema-error
      (validate 10 schema))
    (not-signals json-schema-error
      (validate 99 schema))
    (signals json-schema-range-error
      (validate 100 schema))
    (signals json-schema-range-error
      (validate 101 schema)))
  (let ((schema (yason:parse "{
  \"type\": \"number\",
  \"exclusiveMinimum\": 0,
  \"maximum\": 100
}")))
    (signals json-schema-range-error
      (validate -1 schema))
    (signals json-schema-range-error
      (validate 0 schema))
    (not-signals json-schema-error
      (validate 10 schema))
    (not-signals json-schema-error
      (validate 99 schema))
    (not-signals json-schema-error
      (validate 100 schema))
    (signals json-schema-range-error
      (validate 101 schema))))

(deftest test-numeric-invalid-type ()
  (let ((schema (yason:parse "{\"type\": \"number\"}")))
    (signals json-schema-invalid-type-error (validate (make-hash-table :test #'equal)
                                                      schema))
    (signals json-schema-invalid-type-error (validate t
                                                      schema))))
