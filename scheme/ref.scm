(define-syntax if-type-of
  (syntax-rules (list string vector :)
    ((_ collectable : list action1 string action2 vector action3)
     (cond
       ((list? collectable) action1)
       ((string? collectable) action2)
       ((vector? collectable) action3)))))
(define (ref collectable index . value)
  (define (list-insert lst index value)
    (define (_iter _lst i result)
      (if (null? _lst)
          (if (= i index)
              (cons value result)
              result)
          (if (= i index)
              (_iter _lst (+ i 1) (cons value result))
              (_iter (cdr _lst) (+ i 1) (cons (car _lst) result)))))
    (if (> index (length lst))
        #f
        (reverse (_iter lst 0 '()))))
  (define (vector-insert vec index value)
    (list->vector (list-insert (vector->list vec) index value)))
  (define (string-insert str index value)
    (if (char? value)
        (let var ((result (list-insert (string->list str) index value)))
          (if result
              (list->string result)
              #f))
        #f))
  (if (null? value)
      (if (< index ((if-type-of collectable :
                                list length
                                string string-length
                                vector vector-length) collectable))
          ((if-type-of collectable :
                       list list-ref
                       string string-ref
                       vector vector-ref) collectable index)
          #f)
      ((if-type-of collectable :
                   list list-insert
                   string string-insert
                   vector vector-insert) collectable index (car value))))
(load "unit-test.scm")
(define the-tests (list
                   (test (ref '(1 2 3) 1) 2)
                   (test (ref #(1 2 3) 1) 2)
                   (test (ref "123" 1) #\2)
                   (test (ref "123" 3) #f)
                   (test (ref '(1 2 3) 1 0) '(1 0 2 3))
                   (test (ref #(1 2 3) 1 0) #(1 0 2 3))
                   (test (ref #(1 2 3) 1 #\0) #(1 #\0 2 3))
                   (test (ref "123" 1 #\0) "1023")
                   (test (ref "123" 1 0) #f)
                   (test (ref "123" 3 #\4) "1234")
                   (test (ref "123" 5 #\4) #f)))


                   
                   
                    