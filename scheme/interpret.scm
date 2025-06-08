(define exit #f)
(define (use-assertions)
  (call-with-current-continuation
   (lambda (cc)
     (set! exit cc))))
(use-assertions)
(define-syntax assert-with-message
  (syntax-rules ()
    ((_ expression message)
     (if (not expression)
         (begin
           (display message)
           (newline)
           (exit))))))
(define (interpret commands stack)
  (define exit-continuations '())
  (define commands-length (vector-length commands))
  ;returns index of end for current function/if
  (define (find-end pos)
    (define (find-pos plus_elem minus_elem counter index)
      (if (and (not (= index pos)) (= counter 0))
          index
          (if (= index commands-length)
              0
              (if (equal? (vector-ref commands index) plus_elem)
                  (find-pos plus_elem minus_elem (+ counter 1) (+ index 1))
                  (if (equal? (vector-ref commands index) minus_elem)
                      (find-pos plus_elem minus_elem (- counter 1) (+ index 1))
                      (find-pos plus_elem minus_elem counter (+ index 1)))))))
    (if (equal? (vector-ref commands pos) 'define)
        (- (find-pos 'define 'end  0 pos) 1)
        (if (equal? (vector-ref commands pos) 'if)
            (- (find-pos 'if 'endif 0 pos) 1)
            -1)))
  ;returns index of else for current if
  (define (find-else pos)
    (define (find-pos counter index end-index)
      (if (and (not (= index pos)) (= counter 0))
          index
          (if (= index end-index)
              -1
              (let ((current-command (vector-ref commands index)))
                (cond
                  ((equal? current-command 'if) (find-pos (+ counter 1) (+ index 1) end-index))
                  ((equal? current-command 'endif) (find-pos (- counter 1) (+ index 1) end-index))
                  ((equal? current-command 'else) (if (= counter 1) index (find-pos counter (+ index 1) end-index)))
                  (else (find-pos counter (+ index 1) end-index)))))))
    (if (equal? (vector-ref commands pos) 'if)
        (find-pos 0 pos (find-end pos))
        -1))
  ;returns dictionary of functions
  (define (parse-scope begin_pos end_pos)
    (define (_iter index result)
      (if (= index end_pos)
          result
          (let ((current-command (vector-ref commands index)))
            (cond
              ((equal? current-command 'define) (let ((end-index (find-end index)))
                                                  (if (= end-index -1)
                                                      (assert-with-message #f "cant find <end> for <define>")
                                                      (_iter (+ end-index 1) (cons (list (vector-ref commands (+ index 1)) (list (+ index 2) end-index)) result)))))
              ((equal? current-command 'if) (let ((end-index (find-end index)))
                                              (if (= end-index -1)
                                                  (assert-with-message #f "can't find <endif> for <if>")
                                                  (_iter (+ end-index 1) result))))
              (else (_iter (+ index 1) result))))))
    (_iter begin_pos '()))
  ;merges scopes, if element found in both scopes rhs_scope overrides lhs_scope
  (define (merge-scopes lhs_scope rhs_scope)
    (define (_iter collectable result)
      (if (null? collectable)
          result
          (if (equal? (assq (caar collectable) result) #f)
              (_iter (cdr collectable) (cons (car collectable) result))
              (_iter (cdr collectable) result))))
    (_iter lhs_scope rhs_scope))
  ;do stuff:)
  (define (main index end-index stack scope)
    (if (= index end-index)
        stack
        (let ((current-command (vector-ref commands index)))
          (cond
            ((number? current-command) (main (+ index 1) end-index (cons current-command stack) scope ))
            ((equal? current-command '+) (let ((stack-length (length stack)))
                                           (if (< stack-length 2)
                                               (assert-with-message #f "+ cant be applied, cause stack length < 2")
                                               (main (+ index 1) end-index (cons (+ (car stack) (cadr stack)) (cddr stack)) scope ))))
            ((equal? current-command '-) (let ((stack-length (length stack)))
                                           (if (< stack-length 2)
                                               (assert-with-message #f "- cant be applied, cause stack length < 2")
                                               (main (+ index 1) end-index (cons (- (cadr stack) (car stack)) (cddr stack)) scope ))))
            ((equal? current-command '*) (let ((stack-length (length stack)))
                                           (if (< stack-length 2)
                                               (assert-with-message #f "* cant be applied, cause stack length < 2")
                                               (main (+ index 1) end-index (cons (* (car stack) (cadr stack)) (cddr stack)) scope ))))
            ((equal? current-command '/) (let ((stack-length (length stack)))
                                           (if (< stack-length 2)
                                               (assert-with-message #f "/ cant be applied, cause stack length < 2")
                                               (main (+ index 1) end-index (cons (/ (cadr stack) (car stack)) (cddr stack)) scope ))))
            ((equal? current-command 'mod) (let ((stack-length (length stack)))
                                             (if (< stack-length 2)
                                                 (assert-with-message #f "mod cant be applied, cause stack length < 2")
                                                 (main (+ index 1) end-index (cons (remainder (cadr stack) (car stack)) (cddr stack)) scope ))))
            ((equal? current-command 'and) (let ((stack-length (length stack)))
                                             (if (< stack-length 2)
                                                 (assert-with-message #f "and cant be applied, cause stack length < 2")
                                                 (main (+ index 1) end-index (cons (and (car stack) (cadr stack)) (cddr stack)) scope))))
            ((equal? current-command 'or) (let ((stack-length (length stack)))
                                            (if (< stack-length 2)
                                                (assert-with-message #f "or cant be applied, cause stack length < 2")
                                                (main (+ index 1) end-index (cons (- (car stack) (cadr stack)) (cddr stack)) scope))))
            ((equal? current-command '>) (let ((stack-length (length stack)))
                                           (if (< stack-length 2)
                                               (assert-with-message #f "> cant be applied, cause stack length < 2")
                                               (main (+ index 1) end-index (cons (> (cadr stack) (car stack)) (cddr stack)) scope))))
            ((equal? current-command '<) (let ((stack-length (length stack)))
                                           (if (< stack-length 2)
                                               (assert-with-message #f "< cant be applied, cause stack length < 2")
                                               (main (+ index 1) end-index (cons (< (cadr stack) (car stack)) (cddr stack)) scope))))
            ((equal? current-command '=) (let ((stack-length (length stack)))
                                           (if (< stack-length 2)
                                               (assert-with-message #f "= cant be applied, cause stack length < 2")
                                               (main (+ index 1) end-index (cons (= (car stack) (cadr stack)) (cddr stack)) scope))))
            ((equal? current-command 'swap) (let ((stack-length (length stack)))
                                              (if (< stack-length 2)
                                                  (assert-with-message #f "swap cant be applied, cause stack length < 2")
                                                  (main (+ index 1) end-index (append (list (cadr stack) (car stack)) (cddr stack)) scope))))
            ((equal? current-command 'over) (let ((stack-length (length stack)))
                                              (if (< stack-length 2)
                                                  (assert-with-message #f "over cant be applied, cause stack length < 2")
                                                  (main (+ index 1) end-index (cons (cadr stack) stack) scope))))
            ((equal? current-command 'rot) (let ((stack-length (length stack)))
                                             (if (< stack-length 3)
                                                 (assert-with-message #f "rot cant be applied, cause stack length < 3")
                                                 (main (+ index 1) end-index (append (list (caddr stack) (cadr stack) (car stack)) (cdddr stack)) scope))))
            ((equal? current-command 'depth) (let ((stack-length (length stack)))
                                               (main (+ index 1) end-index (cons stack-length stack) scope)))
            ((equal? current-command 'drop) (let ((stack-length (length stack)))
                                              (if (< stack-length 1)
                                                  (assert-with-message #f "drop cant be applied, cause stack length < 1")
                                                  (main (+ index 1) end-index (cdr stack) scope))))
            ((equal? current-command 'dup) (let ((stack-length (length stack)))
                                             (if (< stack-length 1)
                                                 (assert-with-message #f "dup cant be applied, cause stack length < 1")
                                                 (main (+ index 1) end-index (append (list (car stack) (car stack)) (cdr stack)) scope))))
            ((equal? current-command 'not) (let ((stack-length (length stack)))
                                             (if (< stack-length 1)
                                                 (assert-with-message #f "not cant be applied, cause stack length < 1")
                                                 (main (+ index 1) end-index (cons (not (car stack)) (cdr stack)) scope))))
            ((equal? current-command 'neg) (let ((stack-length (length stack)))
                                             (if (< stack-length 1)
                                                 (assert-with-message #f "neg cant be applied, cause stack length < 1")
                                                 (main (+ index 1) end-index (cons (- (car stack)) (cdr stack)) scope))))
            ((equal? current-command 'define) (let ((end-pos (find-end index)))
                                                (if (= end-pos -1)
                                                    (assert-with-message #f "funciton hasn't got an <end>")
                                                    (main (+ end-pos 1) end-index stack scope))))
            ((equal? current-command 'if) (let ((end-pos (find-end index)) (else-index (find-else index)))
                                            (if (= end-pos -1)
                                                (assert-with-message #f "<if> hasn't got an <end>")
                                                (if (car stack)
                                                    (main (+ end-pos 1) end-index
                                                          (main (+ 1 index) end-pos (cdr stack)
                                                                (merge-scopes scope (parse-scope (+ index 1) (if (= else-index -1) end-index else-index))))
                                                          scope)
                                                    (if (= else-index -1)
                                                        (main (+ end-pos 1) end-index (cdr stack) scope)
                                                        (main (+ end-pos 1) end-index
                                                              (main (+ else-index 1) end-pos
                                                                    (cdr stack)
                                                                    (merge-scopes scope (parse-scope (+ index 1) end-pos)))
                                                              scope))))))
            ;exit on top context in stack
            ((equal? current-command 'exit) ((car exit-continuations) stack))
            ;function call will remembering of current context for possible exit
            ((not (equal? (assq current-command scope) #f))
             (let* ((pair (cadr (assq current-command scope)))
                    (result (call-with-current-continuation (lambda (cc) (begin
                                                                           (set! exit-continuations (cons cc exit-continuations))
                                                                           (main (car pair) (cadr pair) stack (merge-scopes scope (parse-scope (car pair) (cadr pair)))))))))
               (begin (set! exit-continuations (cdr exit-continuations)) (main (+ index 1) end-index result scope))))
            (else (assert-with-message #f (string-append "cant interpret <" (symbol->string current-command) ">")))))))
  (main 0  commands-length stack (parse-scope 0 commands-length )))
(interpret #(2 3 * 4 5 * +) '())
(interpret #(   define -- 1 - end
                5 -- --      ) '())
(interpret #(   define abs 
                    dup 0 < 
                    if neg endif 
                end 
                 9 abs 
                -9 abs      ) (quote ()))
(interpret #(   define =0? dup 0 = end 
                define <0? dup 0 < end 
                define signum 
                    =0? if exit endif 
                    <0? if drop -1 exit endif 
                    drop 
                    1 
                end 
                 0 signum 
                -5 signum 
                10 signum       ) (quote ()))
(interpret #(   define -- 1 - end 
                define =0? dup 0 = end 
                define =1? dup 1 = end 
                define factorial 
                    =0? if drop 1 exit endif 
                    =1? if drop 1 exit endif 
                    dup -- 
                    factorial 
                    * 
                end 
                0 factorial 
                1 factorial 
                2 factorial 
                3 factorial 
                4 factorial     ) (quote ()))
(interpret #(   define =0? dup 0 = end 
                define =1? dup 1 = end 
                define -- 1 - end 
                define fib 
                    =0? if drop 0 exit endif 
                    =1? if drop 1 exit endif 
                    -- dup 
                    -- fib 
                    swap fib 
                    + 
                end 
                define make-fib 
                    dup 0 < if drop exit endif 
                    dup fib 
                    swap -- 
                    make-fib 
                end 
                10 make-fib     ) (quote ()))
(interpret #(   define =0? dup 0 = end 
                define gcd 
                    =0? if drop exit endif 
                    swap over mod 
                    gcd 
                end 
                90 99 gcd 
                234 8100 gcd    ) '())
  
    
