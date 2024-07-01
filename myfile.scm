(define (member? x list)
    (if (null? list) #f                                ;(1)
        (if (equal? x (car list)) #t                   ;(2)
            (member? x (cdr list)))))                  ;(3)
