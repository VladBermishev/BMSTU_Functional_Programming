(define-syntax trace-ex
  (syntax-rules ()
    ((_ x)
     (let vars ((var x)) (begin (display 'x) (display " => ") (display var) (newline) x)))))