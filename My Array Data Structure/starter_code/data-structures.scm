(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (ref-val
      (ref reference?))
    ; #####################################################
    ; ###### ENTER YOUR CODE HERE
    ; ###### add a new value type for your arrays
    ; #####################################################
    (arr-val
      (list-ref my-array?)))
 ;(arr-val
  ;    (list-ref reference?)))
    ; #####################################################
    

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  


  (define expval->arr
    (lambda (arr)
    (cases expval arr
     (arr-val (list-ref) list-ref)
	(else 0))))


  (define (read-array-n arr index)
 (let((length (length-array arr)))
  (let((actual-index  (- length index)))
  
  (read-array-actual-n arr actual-index )
)))

  
(define read-array-actual-n
  (lambda (arr index)
  (if (zero? index)
      (cases my-array arr
    (m-array-one-val (array-member)  (m-array-one-val array-member))
     (m-array (array-member rest) (m-array array-member rest))
              ;(deref array-member ))
        (else 0))
(cases my-array arr
    (m-array-one-val (array-member) 0)
     (m-array (array-member rest) (read-array-actual-n rest (- index 1)))
  (else 0)
  ))))


(define expval->arr-ref
    (lambda (arr)
    (cases expval arr
      
     (arr-val (list-ref)
              (cases my-array list-ref
    (m-array-one-val (array-member) array-member)
     (m-array (array-member rest) array-member)
  (else 0)
  ))
	(else 0))))

      ; (m-array-one-val (array-member)(m-array-one-val array-member))
    ; (m-array (array-member rest)  (m-array array-member rest)))))
    


  
  (define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

  ;; HINT if you need extractors, add them here

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ; #####################################################
  ; ###### ENTER YOUR CODE HERE
  ; ###### you might want to add a new datatype for arrays here similar 
  ; ###### to mutable pairs.
  ; #####################################################


(define-datatype my-array my-array? 
   (m-array-one-val (array-member reference?))
(m-array (array-member reference?)
         (rest my-array?))
  )

  ; #####################################################

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of symbol?))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym (expval->printable val))
	    (env->list saved-env)))
	(extend-env-rec* (p-names b-vars p-bodies saved-env)
	  (cons
	    (list 'letrec p-names '...)
	    (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	(proc-val (p)
	  (cases proc p
	    (procedure (var body saved-env)
	      (list 'procedure var '... (env->list saved-env)))))
	(else val))))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE


(define newarray 
(lambda (length value)
  (if (zero? length) '()
         (newarray-helper length value 3)
          )))



;(define (returnval arr)
 ; (let((length (length-array arr)))
     
  ;  (read-array-new arr (- length 1))))


  
  (define (read-array-new arr index)
 (let((length (length-array arr)))
   
  (let((actual-index  (+ 0(- length index))))
      
 
  (read-array-actual arr actual-index )
)))
  
  (define read-array-actual-new
  (lambda (arr index)
  (if (zero? index)
      (cases my-array arr
    (m-array-one-val (array-member)  array-member)
     (m-array (array-member rest) (deref array-member )))
(cases my-array arr
    (m-array-one-val (array-member) 0)
     (m-array (array-member rest) (read-array-actual-new rest (- index 1)))
  ))))
  
(define (newarray-helper length value result)
(if (zero? length) result 
 (if (number? result)
(newarray-helper (- length 1) value (m-array-one-val (newref value)))
(newarray-helper (- length 1) value (m-array  (newref value) result))  )))


(define update-array
  (lambda (arr index value) 
    (let((actual-index (length-array arr)
        ))
     (let((search-index (+ 0 (- actual-index index))))
      
 
  (update-array-actual arr search-index value)
    82
))))
;(define update-array-actual
 ; (lambda (arr index value)
;(if (zero? (- arr index))
 ;    (setref! arr value)
  ;   (update-array-actual (+ 1 arr) index value))))
(define update-array-actual
  (lambda (arr index value)
  (if (zero? index)
      (cases my-array arr
    (m-array-one-val (array-member) (setref! array-member value))
     (m-array (array-member rest) (setref! array-member value))
        )(cases my-array arr
    (m-array-one-val (array-member) (setref! array-member value))
     (m-array (array-member rest) (update-array-actual rest (- index 1) value))
        )) ))


  (define delete-array
  (lambda (arr index value) ;updates the value of the array arr at index index by value value
    (let((actual-index (length-array arr)
        ))
     (let((search-index (+ 0 (- actual-index index))))
       (if (zero? actual-index) -1
 
  (delete-array-actual arr search-index value))
    82
))))
  
   (define delete-array-actual
  (lambda (arr index value)
  (if (zero? index)
      (cases my-array arr
    (m-array-one-val (array-member)
(let ((return (deref array-member)))
              (setref! array-member -1)
  return
  )        )
     (m-array (array-member rest)

(let ((return (deref array-member)))
              (setref! array-member -1)
  return
  )
        ))(cases my-array arr
    (m-array-one-val (array-member) (setref! array-member value))
     (m-array (array-member rest) (delete-array-actual rest (- index 1) value))
        )) ))




(define top-array
  (lambda (arr index value) ;updates the value of the array arr at index index by value value
    (let((actual-index (length-array arr)
        ))
     (let((search-index (+ 0 (- actual-index index))))
       
 
  (top-array-actual arr search-index value)
    
))))
  
   (define top-array-actual
  (lambda (arr index value)
  (if (zero? index)
      (cases my-array arr
    (m-array-one-val (array-member)
(let ((return (deref array-member)))
              
  return
  )            )
     (m-array (array-member rest)

(let ((return (deref array-member)))
              
  return
  )
        ))(cases my-array arr
    (m-array-one-val (array-member) (setref! array-member value))
     (m-array (array-member rest) (top-array-actual rest (- index 1) value))
        )) ))




  

(define (read-array arr index)
  (let((actual-index (length-array arr)
        ))
     (let((search-index (- actual-index index)))
  (read-array-actual actual-index search-index)
)))

  
(define read-array-actual
  (lambda (arr index)
  (if (zero? index)
      (cases my-array arr
    (m-array-one-val (array-member) (deref array-member))
     (m-array (array-member rest) (deref array-member ))
        )
(cases my-array arr
    (m-array-one-val (array-member) 0)
     (m-array (array-member rest) (read-array-actual rest (- index 1)))
  (else 0)
  ))))



  



   
(define print-stack
   (lambda (arr)
(display"[")
     (print-stack-helper arr )
(display" ]")))
     
    (define print-stack-helper
  (lambda (arr)
      (cases my-array arr
      (m-array-one-val (array-member) (if (or (> 0 (deref array-member))(< 10000 (deref array-member)  )) 0  (display (deref array-member))))
     (m-array (array-member rest) (print-stack-helper rest) (if (or (> 0 (deref array-member))(< 10000 (deref array-member)  )) 0  (begin (display " ")(display ( deref array-member))))
        ))))    
  

 (define print-array
   (lambda (arr)
(display"[")
     (print-array-helper arr )
(display"]")))
     
    (define print-array-helper
  (lambda (arr)
      (cases my-array arr
      (m-array-one-val (array-member) (display(deref array-member)))
     (m-array (array-member rest) (print-array-helper rest)  (display", ")(display (deref array-member)))
        (else 0))))    

(define (length-array arr )
(length-array-helper arr 0)
   )
(define (length-array-helper arr result)
  (cases my-array arr
    (m-array-one-val (array-member)  result   )
(m-array (array-member rest) (length-array-helper rest (+ 1 result)))))


  (define (length-stack arr )
(length-stack-helper arr 0)
   )



  (define (length-stack-helper arr result)
  (cases my-array arr
    (m-array-one-val (array-member)  result   )
 
(m-array (array-member rest)

   (if  (or (> 0 (deref array-member))(< 10000 (deref array-member)  ))   result
         (length-stack-helper rest (+ 1 result))))))



  )