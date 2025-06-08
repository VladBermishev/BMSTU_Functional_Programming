(define cppmap '())
(define (map-at index)
  (define (_iter collectable)
    (if (null? collectable)
        #f
        (if (equal? (car (car collectable)) index)
            (cadr (car collectable))
            (_iter (cdr collectable)))))
  (_iter cppmap))

(define (map-set index value)
  (set! cppmap (cons (list index value) cppmap)))

(define (slow-tribonachi n)
  (cond
    ((<= n 1) 0)
    ((equal? n 2) 1)
    (else (+ (slow-tribonachi (- n 1))
             (slow-tribonachi (- n 2))
             (slow-tribonachi (- n 3))))))
(define (fast-tribonachi n)
  (define (search n)
    (let ((res (map-at n)))
      (if (equal? res #f)
          (let ((first (fast-tribonachi (- n 1)))
                (second (fast-tribonachi (- n 2)))
                (third (fast-tribonachi (- n 3))))
            (begin
              (map-set n (+ first second third))
              (+ first second third)))
          res)))
  (cond
    ((<= n 1) 0)
    ((equal? n 2) 1)
    (else (search n))))

  
(define (func 1 2 3) (define (f1) (+ 3 2)) (- 10 (f1)))
(func)


