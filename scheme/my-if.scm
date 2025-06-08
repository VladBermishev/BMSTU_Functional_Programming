(define exit #f)
(define (use-assertions)
  (call-with-current-continuation
   (lambda (cc)
     (set! exit cc))))
(use-assertions)

(define-syntax my-if
  (syntax-rules ()
    ((my-if condition true-expression false-expression)
     (let ((true-promise (delay 'true-expression))
           (false-promise (delay 'false-expression)))
       (begin
         (or condition (exit (force false-promise)))
         (exit (force true-promise)))))
    ((my-if condition true-expression)
     (let ((true-promise (delay 'true-expression)))
       (begin
         (or condition (exit))
         (exit (force true-promise)))))))