(in-package #:cl-json-schema)

(define-condition json-schema-error ()
  ())

(define-condition json-schema-additional-property-error (json-schema-error)
  ((schema        :initarg :schema        :reader json-schema-error-schema)
   (property-name :initarg :property-name :reader json-schema-error-property-name))
  (:report
   (lambda (condition stream)
     (format stream "property ~a is invalid for schema ~a"
             (json-schema-error-property-name condition)
             (json-schema-error-schema condition)))))
