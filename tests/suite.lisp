(in-package #:cl-json-schema-tests)

(defmacro with-signaled ((condition-type condition-var) expression &body body)
  "During evaluation of the BODY forms, the symbol CONDITION-VAR is bound to a condition of type CONDITION-TYPE if, when evaluating the EXPRESSION form, such a condition is raised. "
  ;; TODO(notmgsk): Emacs doesn't indent this properly. Compare with
  ;; DESTRUCTURING-BIND.
  (let ((did-error-p (gensym)))
    `(let* ((,did-error-p nil)
            (,condition-var (handler-case ,expression
                              (,condition-type (c)
                                (setf ,did-error-p t)
                                (is (typep c ',condition-type))
                                c))))
       (if ,did-error-p
           (progn ,@body)
           (error "expected a condition of type ~a, but got ~a"
                  ',condition-type (type-of ,condition-var))))))

(defmacro matches ((condition-type match-expression) &body expression)
  `(with-signaled (,condition-type c) (progn ,@expression)
     (if (string= ,match-expression (format nil "~a" c))
         (is t)
         (error "~s does not match the error: ~a" ,match-expression c))))

(defmacro matches-regexp ((condition-type match-expression) &body expression)
  `(with-signaled (,condition-type c) (progn ,@expression)
     (if (ppcre:scan ,match-expression (format nil "~a" c))
         (is t)
         (error "~s does not match the error: ~a" ,match-expression c))))

(defmacro matches^ ((condition-type match-string) &body expression)
  `(matches-regexp (,condition-type ,(concatenate 'string "^" match-string))
     ,@expression))

(defmacro matches$ ((condition-type match-string) &body expression)
  `(matches-regexp (,condition-type ,(concatenate 'string match-string "$"))
     ,@expression))

(defun run-cl-json-schema-tests (&key (verbose nil) (headless nil))
  (cond
    ((null headless)
     (run-package-tests :package ':cl-json-schema-tests
                        :verbose verbose
                        :describe-failures t
                        :interactive t))
    (t
     (let ((successp (run-package-tests :package ':cl-json-schema-tests
                                        :verbose t
                                        :describe-failures t
                                        :interactive nil)))
       (uiop:quit (if successp 0 1))))))
