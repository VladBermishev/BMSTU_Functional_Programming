(define (cat . args)
  (define (_iter collectable res)
    (if (null? collectable)
        res
        (if (symbol? (car collectable))
            (_iter (cdr collectable) (string-append res (symbol->string (car collectable))))
            (_iter (cdr collectable) (string-append res (car collectable))))))
  (string->symbol (_iter args "")))
(define-syntax data-constructor
  (syntax-rules ()
    ((data-constructor data-type data-name data-args ...)
     (eval (list 'define (list 'data-name 'data-args ...)
                 (list 'list ''data-type ''data-name (list 'list 'data-args ...)))
           (interaction-environment)))))

(define-syntax data-predicate-constructor
  (syntax-rules ()
    ((data-predicate-constructor data-type)
     (eval (list 'define (list (cat 'data-type "?") 'value)
                 '(and (list? value) (equal? (car value) 'data-type)))
           (interaction-environment)))))

(define-syntax define-data
  (syntax-rules ()
    ((define-data data-type ((data-name data-args ...) ...))
     (begin
       (data-predicate-constructor data-type)
       (data-constructor data-type data-name data-args ...) ...))))

(define (match-helper elem collectable)
  (if (null? collectable)
      #f
      (let ((name (caar collectable)) (func (cadar collectable)))
        (if (equal? (cadr elem) (car name))
            (eval (append (list (list 'lambda (cdr name) func)) (caddr elem)) (interaction-environment))
            (match-helper elem (cdr collectable))))))


(define-syntax match
  (syntax-rules ()
    ((match value (name func) ...) (match-helper value '((name func) ...)))))

(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))
(define s (square 10))
(define r (rectangle 10 20))
(define t (triangle 10 20 30))
(define c (circle 10))
(and (figure? s)
     (figure? r)
     (figure? t)
     (figure? c))

(define (perim f)
  (match f 
    ((square a)       (* 4 a))
    ((rectangle a b)  (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r)       (* 2 pi r))))
(define pi (acos -1))
(perim s)
(perim r)
(perim t)
