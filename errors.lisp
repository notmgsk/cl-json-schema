(in-package #:cl-json-schema)

(define-condition json-schema-error ()
  ((schema :initarg :schema :reader json-schema-error-schema)
   (datum :initarg :datum :reader json-schema-error-datum)))

(define-condition json-schema-invalid-type-error (json-schema-error)
  ((expected-type :initarg :expected-type :reader json-schema-error-expected-type)
   (invalid-type :initarg :invalid-type :reader json-schema-error-invalid-type))
  (:report
   (lambda (condition stream)
     (format stream "type ~s (for datum ~a) does not satisfy type ~s specified by schema ~a"
             (json-schema-error-invalid-type condition)
             (json-schema-error-datum condition)
             (json-schema-error-expected-type condition)
             (json-schema-error-schema condition)))))

(define-condition json-schema-required-property-error (json-schema-error)
  ((property-name :initarg :property-name :reader json-schema-error-property-name)))

(define-condition json-schema-required-property-missing-error (json-schema-required-property-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "required property ~a is missing in the datum ~a but required by the schema ~a"
             (json-schema-error-property-name condition)
             (json-schema-error-datum condition)
             (json-schema-error-schema condition)))))

(define-condition json-schema-additional-property-error (json-schema-error)
  ;; TODO(notmgsk): Should property errors have a common ancestor with
  ;; this slot?
  ((property-name :initarg :property-name :reader json-schema-error-property-name))
  (:report
   (lambda (condition stream)
     (format stream "property ~a is invalid for schema ~a"
             (json-schema-error-property-name condition)
             (json-schema-error-schema condition)))))
