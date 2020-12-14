(in-package #:cl-json-schema)

(define-condition json-schema-error ()
  ((schema :initarg :schema :reader json-schema-error-schema :initform nil)
   (datum :initarg :datum :reader json-schema-error-datum :initform nil)
   (message :initarg :message :reader json-schema-error-message :initform nil))
  (:report
   (lambda (condition stream)
     (with-slots (schema datum message) condition
       (format stream "~a" message)))))

(defun json-schema-error (message &rest rest)
  (let ((condition (apply #'make-condition 'json-schema-error :message message rest)))
    (error condition)))

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

(define-condition json-schema-properties-size-error (json-schema-error)
  ((minimum-properties :initarg :minimum-properties :reader json-schema-error-minimum-properties)
   (maximum-properties :initarg :maximum-properties :reader json-schema-error-maximum-properties)
   (provided-properties :initarg :provided-properties :reader json-schema-error-provided-properties))
  (:report
   (lambda (condition stream)
     (with-slots (json-schema-error-minimum-properties
                  json-schema-error-maximum-properties
                  json-schema-error-provided-properties)
         condition
       (cond ((and json-schema-error-minimum-properties json-schema-error-maximum-properties)
              (format stream "expected at least ~a and at most ~a properties but got ~a"
                      json-schema-error-minimum-properties
                      json-schema-error-maximum-properties
                      json-schema-error-provided-properties))
             (json-schema-error-minimum-properties
              (format stream "expected at least ~a properties but got ~a"
                      json-schema-error-minimum-properties
                      json-schema-error-maximum-properties))
             (json-schema-error-maximum-properties
              (format stream "expected at most ~a properties but got ~a"
                      json-schema-error-maximum-properties
                      json-schema-error-maximum-properties)))))))

(define-condition json-schema-pattern-error (json-schema-error)
  ((pattern :initarg :pattern :reader json-schema-error-pattern))
  (:report
   (lambda (condition stream)
     (with-slots (json-schema-error-datum json-schema-error-pattern)
         condition
       (format stream "Datum ~s does not match the required pattern ~s"
               json-schema-error-datum json-schema-error-pattern)))))

(define-condition json-schema-length-error (json-schema-error)
  ((min-length :initarg :min-length :reader json-schema-error-min-length)
   (max-length :initarg :max-length :reader json-schema-error-max-length))
  (:report
   (lambda (condition stream)
     (with-slots (json-schema-error-datum
                  json-schema-error-min-length
                  json-schema-error-max-length)
         condition
       (if (and json-schema-error-min-length
                (> json-schema-error-min-length (length json-schema-error-datum)))
           (format stream "string ~a has length ~a less than the required minimum length ~a"
                   json-schema-error-datum
                   (length json-schema-error-datum)
                   json-schema-error-min-length)
           (format stream "string ~a has length ~a greather than the required maximum length ~a"
                   json-schema-error-datum
                   (length json-schema-error-datum)
                   json-schema-error-max-length))))))

(define-condition json-schema-multipleof-error (json-schema-error)
  ((multiple :initarg :multiple :reader json-schema-error-multiple))
  (:report
   (lambda (condition stream)
     (with-slots (json-schema-error-datum json-schema-error-multiple)
         condition
       (format stream "Datum ~a is not a multiple of ~a"
               json-schema-error-datum
               json-schema-error-multiple)))))

(define-condition json-schema-range-error (json-schema-error)
  ((minimum :initarg :minimum :reader json-schema-error-minimum)
   (exclusive-minimum :initarg :exclusive-minimum :reader json-schema-error-exclusive-minimum)
   (maximum :initarg :maximum :reader json-schema-error-maximum)
   (exclusive-maximum :initarg :exclusive-maximum :reader json-schema-error-exclusive-maximum))
  (:report
   (lambda (condition stream)
     (with-slots (json-schema-error-datum
                  json-schema-error-minimum
                  json-schema-error-exclusive-minimum
                  json-schema-error-maximum
                  json-schema-error-exclusive-maximum)
         condition
       (cond (json-schema-error-minimum
              (format stream "Datum ~a is less than the inclusive minimum ~a"
                      json-schema-error-datum json-schema-error-minimum))
             (json-schema-error-exclusive-minimum
              (format stream "Datum ~a is less than or equal to the exclusive minimum ~a"
                      json-schema-error-datum json-schema-error-exclusive-minimum))
             (json-schema-error-maximum
              (format stream "Datum ~a is greater than the inclusive maximum ~a"
                      json-schema-error-datum json-schema-error-maximum))
             (json-schema-error-exclusive-maximum
              (format stream "Datum ~a is greater than or equal to the exclusive minimum ~a"
                      json-schema-error-datum json-schema-error-exclusive-maximum)))))))

(define-condition json-schema-invalid-schema-type-error (json-schema-error)
  ((schema-type :initarg :schema-type :reader json-schema-error-schema-type))
  (:report
   (lambda (condition stream)
     (format stream "Unsupported type ~s in schema"
             (json-schema-error-schema-type condition)))))
