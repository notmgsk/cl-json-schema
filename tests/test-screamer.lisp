(in-package :cl-json-schema)

(defun test-basic ()
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
    \"lastName\": {
      \"type\": \"string\",
      \"description\": \"The person's last name.\"
    },
    \"age\": {
      \"description\": \"Age in years which must be equal to or greater than zero.\",
      \"type\": \"integer\",
      \"minimum\": 0
    }
  },
  \"required\": [\"firstName\"],
  \"additionalProperties\": {\"type\": \"string\"},
}
")
        (data "
{
  \"firstName\": \"John\",
  \"lastName\": \"Doe\",
  \"age\": 38,
  \"boop\": 1,
}
"))
    (validate (yason:parse data)
              (yason:parse schema))))

(defun test-propertyNames ()
  (let ((schema "
{
  \"$id\": \"https://example.com/person.schema.json\",
  \"$schema\": \"http://json-schema.org/draft-07/schema#\",
  \"title\": \"Person\",
  \"type\": \"object\",
  \"minProperties\": 1,
  \"maxProperties\": 2,
  \"propertyNames\": {
      \"pattern\": \"^[A-Za-z_][A-Za-z0-9_]*$\"
  },
}
")
        (data "
{
  \"firstName\": \"John\",
  \"lastName\": \"Doe\",
  \"age\": 38,
}
"))
    (validate (yason:parse data)
              (yason:parse schema))))

(defun test-pattern-properties ()
  (let ((schema "
{
  \"type\": \"object\",
  \"patternProperties\": {
    \"^S_\": { \"type\": \"string\" },
    \"^I_\": { \"type\": \"integer\" }
  },
  \"additionalProperties\": false
}")
        (data "
{
  \"S_25\": \"This is a string\",
  \"I_0\": \"hi\",
}
"))
    (validate (yason:parse data)
              (yason:parse schema))))
