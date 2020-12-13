(in-package #:cl-json-schema-tests)

(deftest test-object ()
  (let ((schema
          (yason:parse "{ \"type\": \"object\" }")))
    (not-signals json-schema-error
      (validate (yason:parse "{
   \"key\"         : \"value\",
   \"another_key\" : \"another_value\"
}")
                schema))
    (not-signals json-schema-error
      (validate (yason:parse "{
    \"Sun\"     : 1.9891e30,
    \"Jupiter\" : 1.8986e27,
    \"Saturn\"  : 5.6846e26,
    \"Neptune\" : 10.243e25,
    \"Uranus\"  : 8.6810e25,
    \"Earth\"   : 5.9736e24,
    \"Venus\"   : 4.8685e24,
    \"Mars\"    : 6.4185e23,
    \"Mercury\" : 3.3022e23,
    \"Moon\"    : 7.349e22,
    \"Pluto\"   : 1.25e22
}")
                schema))
    (signals json-schema-invalid-type-error
      (validate (yason:parse "\"not an object\"")
                schema))
    (signals json-schema-invalid-type-error
      (validate (yason:parse "[\"an\", \"array\"]")
                schema))))

(deftest test-object-properties ()
  ;; See for source
  ;; https://json-schema.org/understanding-json-schema/reference/object.html
  (let ((schema
          (yason:parse "
{
  \"type\": \"object\",
  \"properties\": {
    \"number\":      { \"type\": \"number\" },
    \"street_name\": { \"type\": \"string\" },
    \"street_type\": { \"type\": \"string\",
                     \"enum\": [\"Street\", \"Avenue\", \"Boulevard\"]
                   }
  }
}
"))
        (data
          (list
           (yason:parse
            "{ \"number\": 1600, \"street_name\": \"Pennsylvania\", \"street_type\": \"Avenue\" }")
           (yason:parse
            "{ \"number\": 1600, \"street_name\": \"Pennsylvania\" }")
           (yason:parse
            "{ }")
           (yason:parse
            "{ \"number\": 1600, \"street_name\": \"Pennsylvania\", \"street_type\": \"Avenue\", \"direction\": \"NW\" }"))))
    (loop :for datum :in data :do
      (is (validate datum schema)))))

(deftest test-object-properties-invalid-type ()
  (let ((schema
          (yason:parse "
{
  \"type\": \"object\",
  \"properties\": {
    \"number\":      { \"type\": \"number\" },
    \"street_name\": { \"type\": \"string\" },
    \"street_type\": { \"type\": \"string\",
                     \"enum\": [\"Street\", \"Avenue\", \"Boulevard\"]
                   }
  }
}
")))
    (let* ((datum
             (yason:parse
              "{ \"number\": \"1600\", \"street_name\": \"Pennsylvania\", \"street_type\": \"Avenue\" }"))
           (condition (signals json-schema-invalid-type-error
                        (validate datum schema))))
      (is (string= (json-schema-error-datum condition) "1600"))
      (is (string= (json-schema-error-invalid-type condition) "string"))
      (is (string= (json-schema-error-expected-type condition) "number")))))

(deftest test-required-properties ()
  (let ((schema (yason:parse "
{
  \"type\": \"object\",
  \"properties\": {
    \"name\":      { \"type\": \"string\" },
    \"email\":     { \"type\": \"string\" },
    \"address\":   { \"type\": \"string\" },
    \"telephone\": { \"type\": \"string\" }
  },
  \"required\": [\"name\", \"email\"]
}")))
    (let ((datum
            (yason:parse "
{
  \"name\": \"William Shakespeare\",
  \"email\": \"bill@stratford-upon-avon.co.uk\"
}")))
      (not-signals json-schema-error
        (validate datum schema)))
    (let ((datum
            (yason:parse "
{
  \"name\": \"William Shakespeare\",
  \"email\": \"bill@stratford-upon-avon.co.uk\",
  \"address\": \"Henley Street, Stratford-upon-Avon, Warwickshire, England\",
  \"authorship\": \"in question\"
}
")))
      (not-signals json-schema-error
        (validate datum schema)))
    (let* ((datum
             (yason:parse "
{
  \"name\": \"William Shakespeare\",
  \"address\": \"Henley Street, Stratford-upon-Avon, Warwickshire, England\",
}"))
           (condition
             (signals json-schema-error
               (validate datum schema))))
      (is (string= (json-schema-error-property-name condition) "email")))))

(deftest test-additional-properties ()
  (let ((schema "
{
  \"$id\": \"https://example.com/person.schema.json\",
  \"$schema\": \"http://json-schema.org/draft-07/schema#\",
  \"title\": \"Person\",
  \"type\": \"object\",
  \"properties\": {
    \"firstName\": {
      \"type\": \"string\",
      \"pattern\": \"^John\",
      \"description\": \"The person's first name.\"
    },
  },
  \"required\": [\"firstName\"],
  \"additionalProperties\": false,
}
")
        (data "
{
  \"firstName\": \"John\",
  \"lastName\": \"Doe\",
}
"))
    (let ((condition (signals json-schema-additional-property-error
                       (validate (yason:parse data)
                                 (yason:parse schema)))))
      (is (string= (json-schema-error-property-name condition) "lastName"))))
  (let ((schema (yason:parse "{
  \"type\": \"object\",
  \"properties\": {
    \"number\":      { \"type\": \"number\" },
    \"street_name\": { \"type\": \"string\" },
    \"street_type\": { \"type\": \"string\",
                     \"enum\": [\"Street\", \"Avenue\", \"Boulevard\"]
                   }
  },
  \"additionalProperties\": { \"type\": \"string\" }
}")))
    (let ((data (yason:parse "{ \"number\": 1600, \"street_name\": \"Pennsylvania\", \"street_type\": \"Avenue\" }")))
      (not-signals json-schema-error
        (validate data schema)))
    (let ((data (yason:parse "{ \"number\": 1600, \"street_name\": \"Pennsylvania\", \"street_type\": \"Avenue\", \"direction\": \"NW\" }")))
      (not-signals json-schema-error
        (validate data schema)))
    (let* ((data (yason:parse "{ \"number\": 1600, \"street_name\": \"Pennsylvania\", \"street_type\": \"Avenue\", \"office_number\": 201 }"))
           (condition (signals json-schema-invalid-type-error
                        (validate data schema))))
      (is (= (json-schema-error-datum condition) 201))
      (is (string= (json-schema-error-expected-type condition) "string"))
      (is (string= (json-schema-error-invalid-type condition) "number")))))



(deftest test-min-properties ()
  (let ((schema "{
  \"type\": \"object\",
  \"minProperties\": 2,
}"))
    (let* ((data "{ \"a\": 0, \"b\": 1 }"))
      (not-signals json-schema-error
        (validate (yason:parse data)
                  (yason:parse schema))))
    (let* ((data "{ \"a\": 0, \"b\": 1, \"c\": 2 }"))
      (not-signals json-schema-error
        (validate (yason:parse data)
                  (yason:parse schema))))
    (let* ((data "{}")
           (condition (signals json-schema-properties-size-error
                        (validate (yason:parse data)
                                  (yason:parse schema)))))
      (is (= (json-schema-error-provided-properties condition) 0))
      (is (= (json-schema-error-minimum-properties condition) 2)))))

(deftest test-max-properties ()
  (let ((schema "{
  \"type\": \"object\",
  \"maxProperties\": 3,
}"))
    (let* ((data "{ \"a\": 0, \"b\": 1, \"c\": 2 }"))
      (not-signals json-schema-error
        (validate (yason:parse data)
                  (yason:parse schema))))
    (let* ((data "{ \"a\": 0, \"b\": 1, \"c\": 2, \"d\": 3 }")
           (condition (signals json-schema-properties-size-error
                        (validate (yason:parse data)
                                  (yason:parse schema)))))
      (is (= (json-schema-error-provided-properties condition) 4))
      (is (= (json-schema-error-maximum-properties condition) 3)))))

(deftest test-pattern-properties ()
  (let ((schema "{
  \"type\": \"object\",
  \"patternProperties\": {
    \"^S_\": { \"type\": \"string\" },
    \"^I_\": { \"type\": \"integer\" }
  },
  \"additionalProperties\": false
}"))
    (let ((data "{ \"S_25\": \"This is a string\" }"))
      (not-signals json-schema-error
        (validate (yason:parse data)
                  (yason:parse schema))))
    (let ((data "{ \"I_0\": 42 }"))
      (not-signals json-schema-error
        (validate (yason:parse data)
                  (yason:parse schema))))
    (let* ((data "{ \"S_0\": 42 }")
           (condition (signals json-schema-invalid-type-error
                        (validate (yason:parse data)
                                  (yason:parse schema)))))
      (is (string= (json-schema-error-expected-type condition) "string"))
      (is (string= (json-schema-error-invalid-type condition) "number"))
      (is (= (json-schema-error-datum condition) 42)))
    (let* ((data "{ \"I_42\": \"This is a string\" }")
           (condition (signals json-schema-invalid-type-error
                        (validate (yason:parse data)
                                  (yason:parse schema)))))
      (is (string= (json-schema-error-expected-type condition) "integer"))
      (is (string= (json-schema-error-invalid-type condition) "string"))
      (is (string= (json-schema-error-datum condition) "This is a string")))
    (let* ((data "{ \"keyword\": \"value\" }")
           (condition (signals json-schema-additional-property-error
                        (validate (yason:parse data)
                                  (yason:parse schema)))))
      (is (string= (json-schema-error-property-name condition) "keyword"))))
  (let ((schema "{
  \"type\": \"object\",
  \"properties\": {
    \"builtin\": { \"type\": \"number\" }
  },
  \"patternProperties\": {
    \"^S_\": { \"type\": \"string\" },
    \"^I_\": { \"type\": \"integer\" }
  },
  \"additionalProperties\": { \"type\": \"string\" }
}"))
    (let* ((data "{ \"builtin\": 42 }"))
      (not-signals json-schema-error
        (validate (yason:parse data)
                  (yason:parse schema))))
    (let* ((data "{ \"keyword\": \"value\" }"))
      (not-signals json-schema-error
        (validate (yason:parse data)
                  (yason:parse schema))))
    (let* ((data "{ \"keyword\": 42 }")
           (condition (signals json-schema-invalid-type-error
                        (validate (yason:parse data)
                                  (yason:parse schema)))))
      (is (string= (json-schema-error-expected-type condition) "string"))
      (is (string= (json-schema-error-invalid-type condition) "number"))
      (is (= (json-schema-error-datum condition) 42)))))

(deftest test-property-names ()
  (let ((schema (yason:parse "{
  \"type\": \"object\",
  \"propertyNames\": {
    \"pattern\": \"^[A-Za-z_][A-Za-z0-9_]*$\"
  }
}
")))
    (let ((datum (yason:parse "{
  \"_a_proper_token_001\": \"value\"
}")))
      (not-signals json-schema-error
        (validate datum schema)))
    (let* ((datum (yason:parse "{
  \"001 invalid\": \"value\"
}"))
           (condition (signals json-schema-pattern-error
                        (validate datum schema))))
      (is (string= (json-schema-error-datum condition) "001 invalid")))
    (let* ((datum (make-hash-table :test 'equal)))
      ;; NOTE(notmgsk): This is unfortunate. JSON keys are required to
      ;; be strings and so yason makes the (questionable?) decision to
      ;; parse keys as strings. The numeric key in {0: 1} is then
      ;; parsed as a string "0". In a more rigid interpretation it
      ;; would instead throw an error. :shrug:
      (setf (gethash 0 datum) "value")
      (let ((condition (signals json-schema-invalid-type-error
                         (validate datum schema))))
        (is (= (json-schema-error-datum condition) 0))
        (is (string= (json-schema-error-expected-type condition) "string"))
        (is (string= (json-schema-error-invalid-type condition) "number"))))))

(deftest test-invalid-object ()
  (signals json-schema-invalid-type-error
    (json-schema::validate-object nil (yason:parse "{\"type\": \"string\"}")))
  (signals error
    (json-schema::validate-object (make-hash-table :test 'eql)
                                  (yason:parse "{\"type\": \"string\"}"))))
