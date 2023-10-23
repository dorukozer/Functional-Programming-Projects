(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LETREC language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")
  (require "globalstack.scm")
  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-translation value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define value-of-translation
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-env))))))

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))
  
  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 83
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of letrec-body
            (extend-env-rec p-name b-var p-body env)))
        
        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### You need to implement the value-of methods of
        ; ###### proc-nested-exp, call-nested-exp, and 
        ; ###### letrec-nested-exp. They should be similar to 
        ; ###### the proc-exp, letrec-exp and call-exp versions.
        ; ###### However, please keep in mind the nested-procedure
        ; ###### type and extend-env-rec-nested procedure in other
        ; ###### files while implementing this structure. 
        ; #####################################################
          (proc-nested-exp (var n name body)
                       (proc-val (nested-procedure var n name body (extend-env 'n (num-val 0) env))))

     


        (call-nested-exp (rator rand count)
          (let ((proc (expval->proc (value-of rator env)))
                (n (expval->num (value-of count env)))
               
                (arg (value-of rand env)))
             (let (( env-cont (extend-env  'm  (num-val (+ (expval->num(value-of  (var-exp 'm) env)) 1)) env)))
               (let (( m ( expval->num (value-of (var-exp 'm) env-cont))))
             ;   (extend-env  'result (apply-procedure proc arg) env)
                 (recursive-displayer "anonym" m)  
                 (if (equal? 0 n) (value-of (var-exp 'result) (extend-env  'result  (apply-procedure proc arg) env-cont))
                    (value-of (call-nested-exp rator (const-exp(expval->num(apply-procedure proc arg)))  (const-exp(- n 1 ))) (extend-env  'result (apply-procedure proc arg) env-cont))
                            )))))
                    ;   (value-of (call-nested-exp rator rand (const-exp(- n 1 ))) (extend-env  'result (apply-procedure proc arg) env)))))
                   ; (apply-procedure proc arg)
;(value-of 'result (extend-env  'result  (apply-procedure proc arg) env))
               ; (value-of (call-nested-exp rator rand count) env))))
              ; (apply-procedure proc arg)))

        ; #####################################################
      

(letrec-nested-exp (b-var b-count p-name p-body letrec-body)
          (value-of letrec-body
            (extend-env-rec-nested b-var 0 p-name p-body env)))



        (else 1)
        )



      ))


  ;; apply-procedure : Proc * ExpVal -> ExpVal

  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of body (extend-env var arg saved-env)))
        
        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### Here, you need to handle the case where proc 
        ; ###### is not a procedure but a nested-procedure. 
        ; ###### Since we also keep the count as a parameter, 
        ; ###### we also need to extend the environment with
        ; ###### the count value.
        ; ###### Additionally, please not that no matter how the
        ; ###### procedure is defined, all procedures are applied
        ; ###### by using this part. This information may be
        ; ###### helpful for your implementation.
        ; #####################################################

        (nested-procedure (var count name body saved-env)
                          (let (( env-cont (extend-env  'n  (num-val (+ (expval->num(value-of  (var-exp 'n) saved-env)) 1)) (extend-env var arg saved-env))))

                            (let (( n ( expval->num (value-of (var-exp 'n) env-cont))))
                            
                       ;   (recursive-displayer name n)
                          ;  (if (equal? n 0) (value-of 'result saved-env)
                   ; (value-of body (extend-env  'result (value-of body env-cont))))))
                     (value-of body (extend-env var arg env-cont)))))       
(else 1)

        ; #####################################################
      
      )))
  
    (define recursive-displayer
      (lambda (name num)
        (if (zero? num) (begin
          (display name)
          (display " --> ")
          (display num)
          (newline)
        )
        (begin
          (recursive-displayer-helper name (- num 1))
          (display num)
          (newline))
    )))

    (define recursive-displayer-helper
      (lambda (name num)
        (if (zero? num) 
          (begin
            (display name)
            (display " --> ")
          )

        (begin
          (display "....")
          (recursive-displayer-helper name (- num 1))
        )
    ))
    )
  )
  


  
