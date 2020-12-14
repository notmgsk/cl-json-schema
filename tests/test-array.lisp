(in-package #:cl-json-schema-tests)

(deftest test-array ()
  (let ((schema (yason:parse "{\"type\": \"array\"}")))
    (not-signals json-schema-error (validate (yason:parse "[1, 2, 3, 4, 5]")
                                             schema))
    (not-signals json-schema-error (validate (yason:parse "[3, \"different\", { \"types\" : \"of values\" }]")
                                             schema))
    (matches^ (json-schema-error "non-array datum")
      (validate (yason:parse "{\"Not\": \"an array\"}")
                schema))))

(deftest test-array-items ()
  (let ((schema (yason:parse "{\"type\": \"array\", \"items\": {\"type\": \"number\"}}")))
    (not-signals json-schema-error (validate (yason:parse "[1, 2, 3, 4, 5]") schema))
    (signals json-schema-invalid-type-error (validate (yason:parse "[3, \"different\", { \"types\" : \"of values\" }]") schema))
    (not-signals json-schema-invalid-type-error (validate (yason:parse "[]") schema))))

(deftest test-array-items-tuple ()
  (let ((schema (yason:parse "{
  \"type\": \"array\",
  \"items\": [
    {
      \"type\": \"number\"
    },
    {
      \"type\": \"string\"
    },
    {
      \"type\": \"string\",
      \"enum\": [\"Street\", \"Avenue\", \"Boulevard\"]
    },
    {
      \"type\": \"string\",
      \"enum\": [\"NW\", \"NE\", \"SW\", \"SE\"]
    }
  ]
}")))
    (not-signals json-schema-error
      (validate (yason:parse "[1600, \"Pennsylvania\", \"Avenue\", \"NW\"]")
                schema))
    (not-signals json-schema-error
      (validate (yason:parse "[10, \"Downing\", \"Street\"]")
                schema))
    (validate (yason:parse "[1600, \"Pennsylvania\", \"Avenue\", \"NW\", \"Washington\"]")
                schema)
    (not-signals json-schema-error
      (validate (yason:parse "[1600, \"Pennsylvania\", \"Avenue\", \"NW\", \"Washington\"]")
                schema))
    (signals json-schema-error
      (validate (yason:parse "[24, \"Sussex\", \"Drive\"]")
                schema))
    (signals json-schema-error
      (validate (yason:parse "[\"Palais de l'Élysée\"]")
                schema))))

(deftest test-array-items-bad-enum ()
  (let ((schema (yason:parse "{
  \"type\": \"array\",
  \"items\": [
    {
      \"type\": \"string\",
      \"enum\": 1,
    },
  ],
}")))
    (matches^ (json-schema-error "expected a list for enum")
      (validate (yason:parse "[\"\"]") schema))))

(deftest test-array-items-tuple-additional-items-false ()
  (let ((schema (yason:parse "{
  \"type\": \"array\",
  \"items\": [
    {
      \"type\": \"number\"
    },
    {
      \"type\": \"string\"
    },
    {
      \"type\": \"string\",
      \"enum\": [\"Street\", \"Avenue\", \"Boulevard\"]
    },
    {
      \"type\": \"string\",
      \"enum\": [\"NW\", \"NE\", \"SW\", \"SE\"]
    }
  ],
  \"additionalItems\": false,
}")))
    (not-signals json-schema-error
      (validate (yason:parse "[1600, \"Pennsylvania\", \"Avenue\", \"NW\"]")
                schema))
    (not-signals json-schema-error
      (validate (yason:parse "[1600, \"Pennsylvania\", \"Avenue\"]")
                schema))
    (signals json-schema-error
      (validate (yason:parse "[1600, \"Pennsylvania\", \"Avenue\", \"NW\", \"Washington\"]")
                schema))))

(deftest test-array-items-tuple-additional-items ()
  (let ((schema (yason:parse "{
  \"type\": \"array\",
  \"items\": [
    {
      \"type\": \"number\"
    },
    {
      \"type\": \"string\"
    },
    {
      \"type\": \"string\",
      \"enum\": [\"Street\", \"Avenue\", \"Boulevard\"]
    },
    {
      \"type\": \"string\",
      \"enum\": [\"NW\", \"NE\", \"SW\", \"SE\"]
    }
  ],
  \"additionalItems\": {\"type\": \"string\"},
}")))
    (not-signals json-schema-error
      (validate (yason:parse "[1600, \"Pennsylvania\", \"Avenue\", \"NW\", \"Washington\"]")
                schema))
    (matches (json-schema-error "non-string datum 20500")
      (validate
       (yason:parse "[1600, \"Pennsylvania\", \"Avenue\", \"NW\", 20500]")
       schema))))

(deftest test-array-contains ()
  (let ((schema (yason:parse "{
   \"type\": \"array\",
   \"contains\": {
     \"type\": \"number\"
   }
}")))
    (not-signals json-schema-error (validate (yason:parse "[\"life\", \"universe\", \"everything\", 42]") schema))
    (not-signals json-schema-error (validate (yason:parse "[1, 2, 3, 4, 5]") schema))
    (matches (json-schema-error "array does not validate the contains schema")
      (validate (yason:parse "[\"life\", \"universe\", \"everything\", \"forty-two\"]") schema))))

(deftest test-array-length ()
  (let ((schema (yason:parse "{
  \"type\": \"array\",
  \"minItems\": 2,
  \"maxItems\": 3
}")))
    (matches (json-schema-error "array length (0) smaller than required (2)")
      (validate (yason:parse "[]") schema))
    (matches (json-schema-error "array length (1) smaller than required (2)")
      (validate (yason:parse "[1]") schema))
    (not-signals json-schema-error
      (validate (yason:parse "[1, 2]") schema))
    (not-signals json-schema-error
      (validate (yason:parse "[1, 2, 3]") schema))
    (matches (json-schema-error "array length (4) greater than required (3)")
      (validate (yason:parse "[1, 2, 3, 4]") schema))))

(deftest test-array-unique ()
  (let ((schema (yason:parse "{
  \"type\": \"array\",
  \"uniqueItems\": true,
}")))
    (matches (json-schema-error "non-unique array element: 4")
      (validate (yason:parse "[1, 2, 3, 4, 4]") schema))
    (not-signals json-schema-error
      (validate (yason:parse "[1, 2, 3, 4, 5]") schema))
    (not-signals json-schema-error
      (validate (yason:parse "[]") schema))))
