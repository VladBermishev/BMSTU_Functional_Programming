(define (factorize expression)
  (let vars ((x (cadr (cadr expression)))
             (y (cadr (caddr expression)))
             (sign (car expression))
             (x_power (caddr (cadr expression)))
             (y_power (caddr ( caddr expression))))
    (if (and (= x_power y_power) (symbol? sign))
        (if (= x_power 2)
            (if (equal? sign '-)
                (list '* (list '- x y) (list '+ x y))
                #f)
            (if (= x_power 3)
                (if (equal? sign '-)
                    (list '* (list '- x y) (list '+ (list 'expt x 2) (list '* x y) (list 'expt y 2)))
                    (list '* (list '+ x y) (list '+ (list 'expt x 2) (list '- (list '* x y)) (list 'expt y 2))))
                #f))
        #f)))
(load "unit-test.scm")
(define the-tests (list
                   (test (factorize '(- (expt x 2) (expt y 2)))
                         '(* (- x y) (+ x y)))
                   (test (eval (list (list 'lambda 
                                            '(x y) 
                                            (factorize '(- (expt x 2) (expt y 2))))
                                      1 2)
                                (interaction-environment)) -3)
                   (test (factorize '(+ (expt x 2) (expt y 2))) #f)
                   (test (factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))
                         '(* (- (+ first 1) (- second 1)) (+ (+ first 1) (- second 1))))
                   (test (eval (list (list 'lambda 
                                            '(first second) 
                                            (factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2))))
                                      1 2)
                                (interaction-environment)) 3)
                   (test (factorize '(+ (expt (+ x 1) 2) (expt (- y 1) 2))) #f)
                   (test (factorize '(- (expt x 3) (expt y 3)))
                         '(* (- x y) (+ (expt x 2) (* x y) (expt y 2))))
                   (test (factorize '(+ (expt x 3) (expt y 3)))
                         '(* (+ x y) (+ (expt x 2) (- (* x y)) (expt y 2))))
                   (test (eval (list (list 'lambda 
                                            '(x y) 
                                            (factorize '(- (expt x 3) (expt y 3))))
                                      1 2)
                                (interaction-environment)) -7)
                   (test (eval (list (list 'lambda 
                                            '(x y) 
                                            (factorize '(+ (expt x 3) (expt y 3))))
                                      1 2)
                                (interaction-environment)) 9)
                   (test (factorize '(- (expt x 2) (expt y 3))) #f)
                   (test (factorize '(- (expt x 3) (expt y 2))) #f)
                   (test (factorize '(+ (expt x 3) (expt y 2))) #f)
                   (test (factorize '(+ (expt x 2) (expt y 3))) #f)
                   (test (factorize '(+ (expt x 2) (expt y 3))) #f)
                   (test (factorize '(+ (expt x 2) (expt y 4))) #f)
                   (test (factorize '(+ (expt x 4) (expt y 4))) #f)
                   (test (factorize '(+ (expt x 0) (expt y 1))) #f)
                   (test (factorize '(- (expt x 3) (expt y 4))) #f)
                   (test (factorize '(- (expt x 4) (expt y 4))) #f)
                   (test (factorize '(- (expt x 4) (expt y 3))) #f)))