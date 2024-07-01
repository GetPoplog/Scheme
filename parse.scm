
(define (p_const n)
   (lambda (l)
       (if (eq? (car l) n)
            (cons n (cdr l))
            #f )))

(example '((p_const 'the) '(the fat cat)) '(the fat cat))
(example '((p_const 'the) '(a fat cat)) #f)

(define (parse_oneof tokens)
   (lambda (l)
     (if (member (car l) tokens)
         l
         #f))
)

(define noun (parse_oneof '(cat dog cow pencil)))

(example '(noun '(cat eats cake)) '(cat eats cake))
(example '(noun '(eats cake)) #f)
(define det (parse_oneof '(a the)))

(define (parse_seq parser_1 parser_2 f)
    (lambda (l)
        (let
            ((parse_1 (parser_1 l)))
            (if parse_1
                (let
                    ((parse_2 (parser_2 (cdr parse_1))))
                    (if parse_2
                        (f (car parse_1) (car parse_2))
                        #f
                        ) ;end if
                    ) ;end let
                ) ;end if
            ) ;end let
        ) ;end lambda
    ) ;end define
