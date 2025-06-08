(define (cat . args)
  (define (_iter collectable res)
    (if (null? collectable)
        res
        (if (symbol? (car collectable))
            (_iter (cdr collectable) (string-append res (symbol->string (car collectable))))
            (_iter (cdr collectable) (string-append res (car collectable))))))
  (string->symbol (_iter args "")))

(define-syntax struct-predicate-constructor
  (syntax-rules ()
    ((struct-pred-constructor struct-name)
     (eval (list 'define (list (cat 'struct-name "?") 'struct)
                 '(and (list? struct) (equal? (car struct) 'struct-name)))
           (interaction-environment)))))

(define-syntax struct-reference-constructor
  (syntax-rules ()
    ((struct-reference-constructor struct-name struct-field)
     (eval (list 'define (list (cat 'struct-name "-" 'struct-field) 'struct)
                 '(cadr (assq 'struct-field (cadr struct))))
           (interaction-environment)))))
(define-syntax struct-references-constructor
  (syntax-rules ()
    ((struct-references-constructor struct-name (struct-field))
     (struct-reference-constructor struct-name struct-field))
    ((struct-references-constructor struct-name (struct-field . struct-fields))
     (begin (struct-reference-constructor struct-name struct-field) (struct-references-constructor struct-name struct-fields)))))

(define-syntax struct-setter-constructor
  (syntax-rules ()
    ((struct-setter-constructor struct-name struct-field)
     (eval (list 'define (list (cat "set-" 'struct-name "-" 'struct-field "!") 'struct 'value)
                 '(set-car! (cdr (assq 'struct-field (cadr struct))) value))
           (interaction-environment)))))
(define-syntax struct-setters-constructor
  (syntax-rules ()
    ((struct-setters-constructor struct-name (struct-field))
     (struct-setter-constructor struct-name struct-field))
    ((struct-setters-constructor struct-name (struct-field . struct-fields))
     (begin (struct-setter-constructor struct-name struct-field) (struct-setters-constructor struct-name struct-fields)))))

(define-syntax struct-fields-constructor
  (syntax-rules ()
    ((struct-fields-constructor (struct-field))
     (list (list 'list ''struct-field (cat 'struct-field "_arg"))))
    ((struct-fields-constructor (struct-field struct-fields ...))
     (list (append '(list)  (struct-fields-constructor (struct-field)) (struct-fields-constructor (struct-fields ...)))) )
    ((struct-fields-constructor struct-name (struct-fields ...))
     (append '('struct-name) (struct-fields-constructor (struct-fields ...))))))

(define-syntax struct-constructor
  (syntax-rules ()
    ((struct-constructor struct-name (struct-fields ...))
     (eval (list 'define (list (cat "make-" 'struct-name) (cat 'struct-fields "_arg") ...)
                 (append '(list) (struct-fields-constructor struct-name (struct-fields ...))))
           (interaction-environment)))))

(define-syntax define-struct
  (syntax-rules ()
    ((_ struct-name (struct-fields ...))
     (begin
       (struct-predicate-constructor struct-name)
       (struct-references-constructor struct-name (struct-fields ...))
       (struct-setters-constructor struct-name (struct-fields ...))
       (struct-constructor struct-name (struct-fields ...))))))
        
(define-struct pos (row col))
(define p (make-pos 1 2))
(pos? p) 
(pos-row p)
(pos-col p)
(set-pos-row! p 3)
(set-pos-col! p 4) 
(pos-row p)
(pos-col p)