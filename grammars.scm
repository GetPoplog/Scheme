;letrec
;string->symbol takes a string and converts it to a scheme symbol. It is
;useful for making parenthesis symbols.

;Parentheses will print out preceded by "##". (string->symbol "(")

;string-> token-list-pop11 takes a string and converts it to a list of tokens.

;Pascal Style
;-------------

;Keywords: if then else begin end procedure + - * /

;An identifier is any Scheme symbol except a keyword.
;A constant is any Scheme atom except a symbol.

;<procedure> ->
;    procedure <identifier> <args> <compound_statement>

;<args> -> ( <identifier_list>)

;<identifier_list> ->
;   <identifier>,<identifier_list> | <identifier>


;<conditional_expr> ->
;    if <expr> then <expr>
;    if <expr> then <expr> else <expr>

;<prim> -> <identifier> | <constant>

;<compound_statement> ->
;    begin <statement_seq>
;    end

(debug 'no-code) ;make it impossible to have a break inside member?

(define (member? x list)
   (if (null? list) #f
     (or (equal? x (car list)) (member? x (cdr list))))
)

(debug 'code)
(example '(member? '2 '(3 4 5)) #f)
(example '(member? '4 '(3 4 5)) #t)


(define (mk_parser_singleton list_of_objects)
    (lambda (list_of_tokens)
        (cond
            (( null? list_of_tokens) #f)
            ((member (car list_of_tokens) list_of_objects) list_of_tokens)
            (else #f ) ))
    )


(define (mk_parse_seq parser_1 parser_2 build_parse)
    (lambda(list_of_tokens)
        (let ((p1 (parser_1 list_of_tokens)))
            (if p1
                (let ((p2 (parser_2 (cdr p1))))
                    (if p2
                        (cons
                            (build_parse (car p1) (car p2))
                            (cdr p2)
                            )
                        #f)
                    ) ;end let
                #f)
            );end let
        ); end lambda
    ); end def. mk_parse_seq

(define (mk_parse_sandwich symbol_1 parser symbol_2 build_parse)
    (lambda(l_tok)
        (if (eq? (car l_tok) symbol_1)
            (let
                ((p1 (parser (cdr l_tok))))
                (if p1
                    (if (and
                            (pair? (cdr p1))
                            (eq? (cadr p1) symbol_2))
                        (cons (build_parse (car p1)) (cddr p1))
                        #f ;  symbol_2 not found
                        )
                    #f  ; parser did not recognise "filling" of sandwich
                    ) ; end if
                ) ; end let
            #f ; did not begin with symbol_1
            )  ;end if
        ); end lambda
    )

;(mk_parse_alternate parser1 parser2 builder_one builder_ext)
;makes a parser for the syntactic form <r> whose grammar is
; <r> -> <r1>
; <r> -> <r1> <r2> <r>
; where <r1> is recognised by parser1 and <r2> is recognised by parser2

(define (mk_parse_alternate parser1 parser2 builder_one builder_ext)
    ;generates parser for form <p1> <p2> <p1> <p2> ... <p1>
    (letrec
        ((parser
             (lambda (l_tok)
                 (let ((p1 (parser1 l_tok)))
                     (if p1
                         (let ((p2 (parser2 (cdr p1))))
                             (if p2
                                 (let ((p3 (parser (cdr p2))))
                                     (if p3 (cons
                                             (builder_ext
                                                 (car p1) (car p2) (car p3))
                                             (cdr p3)
                                             )
                                         #f)
                                     ) ; end let
                                 (cons (builder_one (car p1)) (cdr p1))
                                 )
                             ); end let
                         #f
                         ); end if
                     ); end let
                 ); end lambda
             )) ; end letrec binding
        parser
        ); end letrec
    ) ; end mk_parse_alternate

    ; Now the specific Pascal-style grammar.
(define parens
    (list (string->symbol "(") (string->symbol ")")))
(define comma (list (string->symbol ",")))

(define keywords
    (append parens  comma
        '(procedure begin end + - * / if then else )))


(define (identifier l_tok)
    (if (and
            (pair? l_tok)
            (symbol? (car l_tok))
            (not (member? (car l_tok) keywords) )
            )
        l_tok
        #f
        )
    )

    (example '(identifier '(x + 4)) '(x + 4))
    (example '(identifier '(+ 4)) #f)

(define arglist
    (mk_parse_alternate
        identifier
        (mk_parse_singleton comma)
        (lambda (p)
            (cons p '())
            )
        (lambda (p1 p2 p3)
            (cons p1 p3)
            )
        )
    )

(define toks string->token-list-pop11)
(example '(arglist (toks "x,y,z)")) (cons '(x y z) (toks")")) )


(define addop (mk_parse_singleton '(+ -)))
(debug #t)
(addop '(+ 2))
