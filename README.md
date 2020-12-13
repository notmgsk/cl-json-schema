# cl-json-schema
### _Mark Skilbeck <markskilbeck@gmail.com>_

A Common-Lisp library to validate your [JSON schema](https://json-schema.org/ "JSON schema").

## Documentation

The main entrypoint is `(cl-json-schema:validate thing schema)` where `thing` is a JSON-compatible
value, and `schema` is a `hash-table`. (Alternatively, if you prefer, `(json-schema:validate thing
schema)`.

For example
```common-lisp
(let ((schema (yason:parse "{
  \"type\": \"object\",
  \"propertyNames\": {
    \"pattern\": \"^[A-Za-z_][A-Za-z0-9_]*$\"
  }
}
")))
  ;; NEAT!
  (json-schema:validate (yason:parse "{\"_a_proper_token_001\": \"value\"}")
                        schema)
  ;; NO BUENO! Key does not match the required pattern
  (json-schema:validate (yason:parse "{\"001 invalid\": \"value\"}")
                        schema))
```

## License

MIT. See `LICENSE`.

