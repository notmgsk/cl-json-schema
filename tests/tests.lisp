(in-package #:cl-json-schema-tests)

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
