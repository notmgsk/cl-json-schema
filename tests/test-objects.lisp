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
      (is (string= (json-schema-error-property-name condition) "lastName")))))
