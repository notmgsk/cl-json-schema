(require :sb-cover)

(declaim (optimize sb-cover:store-coverage-data))

(asdf:oos 'asdf:load-op :cl-json-schema-tests :force t)
(asdf:oos 'asdf:load-op :cl-json-schema :force t)

(cl-json-schema-tests:run-cl-json-schema-tests)

(sb-cover:report "/tmp/report/")

(declaim (optimize (sb-cover:store-coverage-data 0)))
