(in-package #:cl-json-schema-tests)

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
