


(define (eval_if b x y)
    (cond ((eq? b #t) x)
        ((eq? b #f) y)
        (else (list 'if b x y))
        )
    )

(apply null? '(()))
(eval_eager '(null? '()))
(trace apply_eager)
(example '(eval_eager ''(1 2 3)) '(1 2 3))
(example '(eval_eager  '((lambda (x) (+ x 3)) 4)) 7)

(eval_eager '(null? '()))

(eval_eager
    '(
     (Y-
         (lambda (sum)
             (lambda(l)
                 (if (null? l) 0 (+ (car l) (sum (cdr l)))
                     )
                 )     ;end lambda
             )  ; end lambda
         )      ; end Y-
     '()
     )
    )


(eval_eager
    '(
     (Y-
         (lambda (sum)
             (lambda(l)
                 (if (null? l) 0 (+ (car l) (sum (cdr l)))
                     )
                 )     ;end lambda
             )  ; end lambda
         )      ; end Y-
     '(33)
     )
    )

(eval_eager ''(33))
