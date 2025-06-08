(define-syntax my-let
  (syntax-rules ()
    ((my-let ((variable value) ...) expression ...)
     ((lambda (variable ...) expression ...) value ...))))
(define-syntax my-let*
  (syntax-rules ()
    ((my-let* ((first-variable first-value)
               (second-variable second-value) ...)
              expression ...)
     (my-let ((first-variable first-value))
             (my-let* ((second-variable second-value) ...) expression ...)
             ))
    ((my-let* () expression ...)
     (my-let () expression ...))))