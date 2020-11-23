(in-package #:cl-json-schema-tests)

(deftest test-empty-schema ()
  (let ((schema "{ }"))
    (not-signals json-schema-error
      (validate (yason:parse "42")
                (yason:parse schema)))
    (not-signals json-schema-error
      (validate (yason:parse "\"I'm a string\"")
                (yason:parse schema)))
    (not-signals json-schema-error
      (validate (yason:parse "{ \"an\": [ \"arbitrarily\", \"nested\" ], \"data\": \"structure\" }")
                (yason:parse schema)))))

(deftest test-true ()
  (let ((schema "true"))
    (not-signals json-schema-error
      (validate (yason:parse "42")
                (yason:parse schema)))
    (not-signals json-schema-error
      (validate (yason:parse "\"I'm a string\"")
                (yason:parse schema)))
    (not-signals json-schema-error
      (validate (yason:parse "{ \"an\": [ \"arbitrarily\", \"nested\" ], \"data\": \"structure\" }")
                (yason:parse schema)))))

(deftest test-false ()
  (let ((schema "false"))
    (signals json-schema-error
      (validate (yason:parse "42")
                (yason:parse schema)))
    (signals json-schema-error
      (validate (yason:parse "\"I'm a string\"")
                (yason:parse schema)))
    (signals json-schema-error
      (validate (yason:parse "{ \"an\": [ \"arbitrarily\", \"nested\" ], \"data\": \"structure\" }")
                (yason:parse schema)))))

(deftest test-invalid-schema-type ()
  (let ((schema (yason:parse "{\"type\": \"z0mgbad!\"}")))
    (signals json-schema-invalid-schema-type-error
      (validate nil schema))))

(deftest test-invalid-lisp->json ()
  (signals error (cl-json-schema::lisp->json 'symbol)))
