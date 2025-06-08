(define memoized-factorial
  (let ((map '()))
    (lambda (n)
      (let ((pair (assq n map)))
        (if pair
            (cadr pair)
            (let ((res (if (< n 2)
                           1
                           (* (memoized-factorial (- n 1)) n))))
              (set! map (cons (list n res) map)) res))))))