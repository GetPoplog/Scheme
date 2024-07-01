
; EXTRAS. These are some functions NOT in the Scheme Standard.


(define (delete obj list)
  (cond ((null? list) '())
        ((equal? obj (car list)) (delete obj (cdr list)))
        (else (cons (car list) (delete obj (cdr list))))
   )
)

(example '(delete 3 '(1 2 3 4 5 3 2)) '(1 2 4 5 2))


(example '(alphaless? 'ab 'b) #t)

(example '(alphaless? 'b 'b) #f)


; The following cases assume NO conversion to upper case.

;(trace explode)
;(trace equal?)
;(explode 'gleep35)
(example '(explode 'gleep35) '(g l e e p 3 5))


(example '(implode '(g l e e p 3 5)) 'gleep35)

(example '(char 65) 'A)

(example '(ascii 'A) 65)

;(example '(generate-uninterned-symbol 'fact) 'fact1)


(example '(number? 23) #t)
(example '(number? 'a) #f)


(example '(integer? 23) #t)

;(define f (open-input-file "initial.scm"))
;(read f)

(example '(symbol->string 'FRED) "FRED")
