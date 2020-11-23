;;;; cl-json-schema.lisp

(in-package #:cl-json-schema)

(deftype json-type ()
  '(or string number hash-table array boolean))

(defvar *schema-reserved-keywords*
  '("type" "title" "description" "default" "examples"
    "$id" "$schema"))

(defun valid-json-type-p (thing)
  (typep thing 'json-type))

(defun validate-type (thing json-type)
  (cond
    ((string= json-type "string")
     (typep thing 'string))
    ((string= json-type "object")
     (typep thing 'hash-table))
    (t
     (error "invalid json type ~a" json-type))))

(defun %validate-number-properties (number schema)
  (let ((multiples (gethash "multiples" schema))
        (minimum (gethash "minimum" schema))
        (exclusive-minimum (gethash "exclusiveMinimum" schema))
        (maximum (gethash "maximum" schema))
        (exclusive-maximum (gethash "exclusiveMaximum" schema)))
    (when (and multiples
               (not (zerop (mod number multiples))))
      (error "number ~a is not a multiple of ~a" number multiples))
    (when (and
           exclusive-minimum
           (not minimum)
           (<= number exclusive-minimum))
      (error "number ~a does not satisfy exclusive minimum requirement ~a"
             number exclusive-minimum))
    (when (and minimum
               (< number minimum))
      (error "number ~a does not satisfy minimum requirement ~a"
             number minimum))
    (when (and
           exclusive-maximum
           (not maximum)
           (>= number exclusive-maximum))
      (error "number ~a does not satisfy exclusive maximum requirement ~a"
             number exclusive-maximum))
    (when (and
           maximum
           (> number maximum))
      (error "number ~a does not satisfy maximum requirement ~a"
             number maximum))))

(defun validate-integer (integer schema)
  (unless (integerp integer)
    (error "~a has type ~a, not integer" integer (type-of integer)))
  (%validate-number-properties integer schema))

(defun validate-number (number schema)
  (unless (numberp number)
    (error "~a has type ~a, not number" number (type-of number)))
  (%validate-number-properties number schema))

(defun validate-string (string schema)
  (unless (typep string 'string)
    (error "value ~a is not a string" string))
  (let ((min-length (gethash "minLength" schema))
        (max-length (gethash "maxLength" schema))
        (pattern (gethash "pattern" schema))
        ;; TODO(notmgsk): Implement formats?
        ;; (format (gethash "format" schema))
        )
    (when (and min-length (< (length string) min-length))
      (error "string ~s (length ~a) does not satisfy minimum length requirement ~a"
             string (length string) min-length))
    (when (and max-length (> (length string) max-length))
      (error "string ~s (length ~a) does not satisfy maximum length requirement ~a"
             string (length string) max-length))
    (when (and
           pattern
           ;; TODO(notmgsk): Catch ppcre errors?
           (not (cl-ppcre:scan pattern string)))
      (error "string ~s does not match the required pattern ~s" string pattern))))

(defun validate-object (object schema)
  (let ((properties (gethash "properties" schema))
        (required-properties (gethash "required" schema))
        ;; TODO(notmgsk): Implement
        ;; (property-names (gethash "propertyNames" schema))
        ;; (min-properties (gethash "minProperties" schema))
        ;; (max-properties (gethash "maxProperties" schema))
        ;; (dependencies (gethash "dependencies" schema))
        )
    (when properties
      (dohash (property-name property-schema properties t)
        (multiple-value-bind (value present-p)
            (gethash property-name object)
          (when (and (find property-name required-properties :test #'string=)
                     (not present-p))
            (error "property ~a is required by the schema but not present in the object ~a"
                   property-name schema))
          (validate value property-schema))))
    (multiple-value-bind (additional-properties-schema present-p)
        (gethash "additionalProperties" schema)
      (dohash (key value object t)
        (unless (or (gethash key properties)
                    (find key *schema-reserved-keywords* :test #'string=))
          (cond ((and present-p additional-properties-schema)
                 (validate value additional-properties-schema))
                ((and present-p (not additional-properties-schema))
                 (error "property ~a is not permitted as an additional property in schema ~a"
                        key schema))))))))

(defun validate (thing schema)
  (check-type schema (or boolean hash-table))
  (cond ((and (typep schema 'boolean) schema)
         t)
        ((and (typep schema 'boolean) (not schema))
         nil)
        ((typep schema 'hash-table)
         (let ((type (gethash "type" schema)))
           (cond ((string= type "object")
                  (validate-object thing schema))
                 ((string= type "string")
                  (validate-string thing schema))
                 ((string= type "integer")
                  (validate-integer thing schema))
                 ((string= type "number")
                  (validate-number thing schema))
                 (t
                  (error "definitely u wot ~a" type)))))
        (t
         (error "u wot"))))
