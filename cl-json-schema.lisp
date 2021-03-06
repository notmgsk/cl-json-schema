;;;; cl-json-schema.lisp

(in-package #:cl-json-schema)

(deftype json-type ()
  '(or string number hash-table array boolean))

(defvar *schema-reserved-keywords*
  '("type" "title" "description" "default" "examples"
    "$id" "$schema"))

(defun validate-number-properties (number schema)
  (let ((multiples (gethash "multipleOf" schema))
        (minimum (gethash "minimum" schema))
        (exclusive-minimum (gethash "exclusiveMinimum" schema))
        (maximum (gethash "maximum" schema))
        (exclusive-maximum (gethash "exclusiveMaximum" schema)))
    (when (and multiples
               (not (zerop (mod number multiples))))
      (error 'json-schema-multipleof-error :datum number :multiple multiples))
    (when (and
           exclusive-minimum
           (not minimum)
           (<= number exclusive-minimum))
      (error 'json-schema-range-error :datum number :exclusive-minimum exclusive-minimum))
    (when (and minimum
               (< number minimum))
      (error 'json-schema-range-error :datum number :minimum minimum))
    (when (and
           exclusive-maximum
           (not maximum)
           (>= number exclusive-maximum))
      (error 'json-schema-range-error :datum number :exclusive-maximum exclusive-maximum))
    (when (and
           maximum
           (> number maximum))
      (error 'json-schema-range-error :datum number :maximum maximum)))
  t)

(defun validate-integer (integer schema)
  (unless (integerp integer)
    (error 'json-schema-invalid-type-error
           :datum integer :schema schema :invalid-type (lisp->json integer) :expected-type "integer"))
  (validate-number-properties integer schema)
  t)

(defun validate-number (number schema)
  (unless (numberp number)
    (error 'json-schema-invalid-type-error
           :datum number :schema schema :invalid-type (lisp->json number) :expected-type "number"))
  (validate-number-properties number schema)
  t)

(defun validate-boolean (boolean schema)
  (unless (typep boolean 'boolean)
    (error 'json-schema-invalid-type-error
           :datum boolean :schema schema :invalid-type (lisp->json boolean) :expected-type "boolean"))
  t)

(defun validate-string (string schema)
  (unless (typep string 'string)
    (json-schema-error (format nil "non-string datum ~a" string) :datum string :schema schema))
  (if-let ((enum (gethash "enum" schema)))
    (progn
      (unless (listp enum)
        (json-schema-error (format nil "expected a list for enum but found ~a" (type-of enum))))
      (unless (find string enum :test #'string=)
        (json-schema-error (format nil "invalid value ~s for enum" string))))
    (let ((min-length (gethash "minLength" schema))
          (max-length (gethash "maxLength" schema))
          (pattern (gethash "pattern" schema))
          ;; TODO(notmgsk): Implement formats?
          ;; (format (gethash "format" schema))
          )
      (when (and min-length (< (length string) min-length))
        (error 'json-schema-length-error
               :schema schema :datum string :min-length min-length))
      (when (and max-length (> (length string) max-length))
        (error 'json-schema-length-error
               :schema schema :datum string :max-length max-length))
      (when (and
             pattern
             ;; TODO(notmgsk): Catch ppcre errors?
             (not (cl-ppcre:scan pattern string)))
        (error 'json-schema-pattern-error :datum string :pattern pattern :schema schema))))
  t)

(defun matching-key (value regex-keyed-table)
  (dohash (regex schema regex-keyed-table nil)
    ;; TODO(notmgsk): import SCAN
    (when (cl-ppcre:scan regex value)
      (return-from matching-key schema))))

(defun validate-object (object schema)
  (unless (typep object 'hash-table)
    (error 'json-schema-invalid-type-error
           :datum object :schema schema :invalid-type (lisp->json object) :expected-type "hash-table"))
  (unless (eql (hash-table-test object) 'equal)
    (error "HASH-TABLEs must use EQUAL for their test"))
  (let ((properties (gethash "properties" schema))
        (required-properties (gethash "required" schema))
        ;; TODO(notmgsk): Implement
        (property-names (gethash "propertyNames" schema))
        ;; (dependencies (gethash "dependencies" schema))
        )
    (when properties
      (dohash (property-name property-schema properties t)
        (multiple-value-bind (value present-p)
            (gethash property-name object)
          (when (and (find property-name required-properties :test #'string=)
                     (not present-p))
            (error 'json-schema-required-property-missing-error
                   :schema schema :datum object :property-name property-name))
          (when present-p
            (validate value property-schema)))))
    ;; TODO(notmgsk): Should minProperties and maxProperties apply if
    ;; properties is provided?
    (when-let* ((min-properties (gethash "minProperties" schema)))
      (let* ((defined-properties (alexandria:hash-table-keys object))
             (n-defined-properties (length defined-properties)))
        (when (> min-properties n-defined-properties)
          (error 'json-schema-properties-size-error
                 :minimum-properties min-properties
                 :provided-properties n-defined-properties))))
    (when-let* ((max-properties (gethash "maxProperties" schema))
                (defined-properties (alexandria:hash-table-keys object))
                (n-defined-properties (length defined-properties)))
      (when (< max-properties n-defined-properties)
        (error 'json-schema-properties-size-error
               :maximum-properties max-properties
               :provided-properties n-defined-properties)))
    (multiple-value-bind (additional-properties-schema additional-properties-p)
        (gethash "additionalProperties" schema)
      (multiple-value-bind (pattern-properties-schema pattern-properties-p)
          (gethash "patternProperties" schema)
        (dohash (key value object t)
          ;; All property names must be strings (i.e. JSON keys are strings).
          (when property-names
            (validate-string key property-names))
          ;; Don't need to validate if this key exists in the properties schema, or if
          ;; it's a reserved keyword.
          (unless (or (and properties (gethash key properties))
                      (find key *schema-reserved-keywords* :test #'string=))
            (cond
              ;; 1. No additional properties entry, or additional properies = true -> all
              ;; additional properties are valid.
              ((or (not additional-properties-p)
                   (and (typep additional-properties-schema 'boolean)
                        additional-properties-schema))
               t)
              ;; 2. Additional properties entry, no pattern properties entry -> match all
              ;; additional properties against an entry in additional properties
              ((and (not pattern-properties-p)
                    additional-properties-schema)
               (validate value additional-properties-schema))
              ;; 3. Additional properties entry, pattern properties entry -> match all
              ;; additional properties against an entry in additional properties OR match
              ;; them against a property pattern
              ((and pattern-properties-p (matching-key key pattern-properties-schema))
               (let ((matching-key-schema (matching-key key pattern-properties-schema)))
                 (validate value matching-key-schema)))
              ((and additional-properties-p additional-properties-schema)
               (validate value additional-properties-schema))
              (t
               (error 'json-schema-additional-property-error
                      :schema schema
                      :property-name key))))))))
  t)

(defun validate-array (array schema)
  (unless (typep array '(or array sequence)) ; TODO(notmgsk): array type?
    (json-schema-error (format nil "non-array datum ~a" array) :datum array :schema schema))
  (when-let* ((items-schema (gethash "items" schema)))
    (multiple-value-bind (additional-items additional-items-provided)
        (gethash "additionalItems" schema)
      (cond
        ;; Validating a tuple, additionalItems is provided explicity
        ;; and is false, and there are more array items than schemas
        ((and (listp items-schema)
              additional-items-provided
              (not additional-items)
              (> (length array) (length items-schema)))
         (json-schema-error (format nil "additional items not supported by enum")))
        ;; Validating a tuple with possible additional items.
        ((listp items-schema)
         (let ((required (subseq array 0 (min (length items-schema) (length array))))
               (additional (subseq array (min (length items-schema) (length array)))))
           (loop :for item-schema :in items-schema
                 :for item :in required :do
                   (validate item item-schema))
           (when additional-items
             (loop :for item :in additional :do
               (validate item additional-items)))))
        ;; Validating each array element against a single schema.
        (t
         (dolist (item array)
           (validate item items-schema))))))
  ;; what am i even doing
  (when-let* ((contains-schema (gethash "contains" schema)))
    (loop :for item :in array
          :collect (handler-case (validate item contains-schema)
                     (json-schema-error () nil)) :into errors
          :finally
             (when (= (count nil errors)
                      (length array))
               (json-schema-error "array does not validate the contains schema"
                                  :datum array :schema contains-schema))))
  (when-let* ((min-items (gethash "minItems" schema)))
    (when (< (length array) min-items)
      (json-schema-error (format nil "array length (~a) smaller than required (~a)"
                                 (length array) min-items))))
  (when-let* ((max-items (gethash "maxItems" schema)))
    (when (> (length array) max-items)
      (json-schema-error (format nil "array length (~a) greater than required (~a)"
                                 (length array) max-items))))
  (when-let* ((unique-items (gethash "uniqueItems" schema)))
    (loop :for i :below (length array)
          :for elt := (nth i array)
          :for rest := (subseq array (1+ i)) :do
            (when (find elt rest)
              (json-schema-error (format nil "non-unique array element: ~a" elt)))))
  t)

(defun validate (thing schema)
  (check-type schema (or boolean hash-table))
  (cond ((and (typep schema 'boolean) schema)
         t)
        ((and (typep schema 'boolean) (not schema))
         (error 'json-schema-error :datum thing :schema schema))
        (t
         (when-let* ((type (gethash "type" schema)))
           (cond ((string= type "object")
                  (validate-object thing schema))
                 ((string= type "array")
                  (validate-array thing schema))
                 ((string= type "string")
                  (validate-string thing schema))
                 ((string= type "integer")
                  (validate-integer thing schema))
                 ((string= type "number")
                  (validate-number thing schema))
                 ((string= type "boolean")
                  (validate-boolean thing schema))
                 (t
                  (error 'json-schema-invalid-schema-type-error
                         :schema schema :schema-type type))))))
  t)

(defun lisp->json (thing)
  (typecase thing
    (string "string")
    (number "number")
    (hash-table "hash-table")
    ((or array sequence) "array")
    (boolean "boolean")
    (t (error "Cannot convert ~a (of type ~a) to a JSON type"
              thing (type-of thing)))))
