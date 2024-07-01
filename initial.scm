

; initial.scm                          Robin Popplestone, May 1995

; The initial bindings for pop-scheme.

(set! level_of_user 10)

(define for-each
    (lambda (proc ls)
        (if (not (null? ls))
            (begin
                (proc (car ls))
                (for-each proc (cdr ls))))))

(define writeln
  ;; Writes out a list of arguments followed by a new line
  (lambda args
    (for-each display args)
    (newline)))

(define error
    (lambda args
        (newline)
        (display "Error [call of error function in Scheme code]:")
        (for-each (lambda (value) (display " ") (display value)) args)
        (newline)
        (reset)
))
(define (example expr val)
    ;; Allows you to run and check examples of code in-line
    (let (
         (val-actual (eval expr (nearest-repl/environment)))
         )
        (newline)
        (display "example: ")
        (write expr)
        (display " = ")
        (write val)
        (if (equal? val-actual val)
            (begin (display ",  ok!") (newline) #t)
            (begin
                (display "
                 example failed, evaluating: ")
                (write expr)
                (display
                    "
                 value returned: ")
                (write val-actual)
                (display
                    "
                 value expected: ")
                (write val)
                (error)
                ))))


(define (member x list)
   (if (null? list) #f
      (if (equal? x (car list)) list
          (member x (cdr list)))))


(define (boolean? obj) (or (eq? obj #f) (eq? obj #t)))


;(define map
;    (lambda (proc ls)
;        (if (null? ls) '()
;            (cons (proc (car ls)) (map proc (cdr ls))))))


(define error
    (lambda args
        (newline)
        (display "Error:")
        (for-each (lambda (value) (display " ") (display value)) args)
        (newline)
        (reset)
))

; c...r variants are nowcreated on the fly by the compiler

;(define (cddr l) (cdr (cdr l)))
;(define (cdddr l) (cdr (cdr (cdr l))))
;(define (caar l) (car (car l)))

(define first car)
(define second cadr)
(define third  caddr)


; does not work and is not required.
;(define list*
;    (lambda args
;       (cond
;             ((atom?
;             ((atom? (cdr args)) (car args))
;             ((atom? (cddr args)) (cons (car args) (cadr args)))
;             (else (cons (car args) (list* (cdr args)) ))
;       )
;     )
;)



(define (nth-cdr i l)
    (if (zero? i) l
        (nth-cdr (- i 1) (cdr l))
    )
)







; ?? Tests of eq? omitted.

; ?? tests of equal? omitted

; 6.3 Pairs and Lists.

(define (list? x)
  (letrec
    ((l?
      (lambda (x x0)

        (cond
          ((eq? x x0) #f)
          ((eq? x '()) #t)
          ((pair? x) (l? (cdr x) x0))
          (else #f)
        ) ; end cond
      )   ; end lambda
     ))   ; end let-binding
     (cond ((pair? x) (l? (cdr x) x))
           ((eq? x '()) #t)
           (else #f)
     )    ; end cond
  )       ; end letrec
)         ; end list?




;(set-cdr! x x)
;?? infinite print out
; ???  we get true here .... (list? x)


(define list (lambda x x))

(define list-tail
   (lambda (x k)
      (if (zero? k)
         x
         (list-tail (cdr x) (- k 1)))))


(define (list-ref list k)
   (car (list-tail list k))
)



(define (memq obj list)
    (cond
        ((null? list) #f)
        ((eq? obj (car list)) list)
        (else (memq obj (cdr list)))
))

(define (memv obj list)
    (cond
        ((null? list) #f)
        ((eqv? obj (car list)) list)
        (else (memv obj (cdr list)))
))





(define (assoc obj alist)
    (cond
        ((null? alist) #f)
        ((equal? obj (caar alist)) (car alist))
        (else (assoc obj (cdr alist)))
))


;(let ((ee '((a 1) (b 2) (c 3)) ))
;(example '(assoc 'a ee) '(a 1))
;)


(define (assq obj alist)
    (cond
        ((null? alist) #f)
        ((eq? obj (caar alist)) (car alist))
        (else (assq obj (cdr alist)))
))


(define (assv obj alist)
    (cond
        ((null? alist) #f)
        ((eqv? obj (caar alist)) (car alist))
        (else (assv obj (cdr alist)))
))



;6.4 Symbols


; 6.5 Numbers

; ??? mluch to be done here

(zero? 0)

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))




; EXTRAS.


(define (delete obj list)
  (cond ((null? list) '())
        ((equal? obj (car list)) (delete obj (cdr list)))
        (else (cons (car list) (delete obj (cdr list))))
   )
)


; The following cases assume NO conversion to upper case.

;(trace explode)
;(trace equal?)
;(explode 'gleep35)


;(example '(generate-uninterned-symbol 'fact) 'fact1)


;(define f (open-input-file "initial.scm"))
;(read f)

; Symbol comparison - non-standard


(define symbol<? string<?)
(define symbol>? string>?)
(define symbol<= string<=?)
(define symbol>= string>=?)

(set! level_of_user 0)
