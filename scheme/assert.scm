(define exit #f)
(define (use-assertions)
  (call-with-current-continuation
   (lambda (cc)
     (set! exit cc))))
(use-assertions)
(define-syntax assert
  (syntax-rules ()
    ((_ expression)
     (if (not expression)
         (begin
           (display "FAILED: ")
           (display 'expression)
           (newline)
           (exit))))))
(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))
(map 1/x '(1 2 3 4 5))
(map 1/x '(-2 -1 0 1 2))


