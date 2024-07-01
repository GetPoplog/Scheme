; examples.scm                          Robin Popplestone, May 1995

; This file contains examples of the use of many constructions in Scheme.
; It is used for checking out new versions of the Scheme compiler,
; but may be of interest to general users.
; Most of the examples are taken from the Scheme definition.

; We use the "example" function. (example EXPR VAL) evaluated EXPR and checks
; to see that the result is equal to VAL.

(define old_level_of_user level_of_user)
(define level_of_user 10) ; we are experts...

(define (freddy y)
    (let ((x 23)) (+ y x))
    )
(freddy 34)


;4.1.5 Conditionals

(example '(if (> 3 2) 'yes 'no) 'yes)
(example '(if (> 2 3) 'yes 'no) 'no)
(example '(if (> 3 2) (- 3 2) (/ 1 0)) 1)
(example '(if (> 3 2) 'yes) 'yes)



;4.2.1 Conditionals

(example
    '(case (* 2 3)
       ((2 3 5 7) 'prime)
       ((1 4 6 8 9) 'composite))
   'composite)



(example
    '(case 3
       ((2 3 5 7) 'prime)
       ((1 4 6 8 9) 'composite))
    'prime)



(example
    '(case (car '(c d))
       ((a) 'a)
       ((b) 'b))
    (display 'ok))


(example
    '(case (car '(c d))
       ((a e i o u) 'vowel)
       ((w y) 'semivowel)
       (else 'consonant))
        'consonant)

(example
    '(case 'a
       ((a e i o u) 'vowel)
       ((w y) 'semivowel)
       (else 'consonant))
        'vowel)


(example
    '(case 'w
       ((a e i o u) 'vowel)
       ((w y) 'semivowel)
       (else 'consonant))
        'semivowel)

(define (or_test b1 b2)
  (or b1 b2))

(example '(or_test #f #f) #f)
(example '(or_test #f #t) #t)
(example '(or_test #t #f) #t)
(example '(or_test #t #t) #t)
(example '(or #f #f #f) #f)
(example '(or #f #f #t) #t)
(example '(or #f #t #f) #t)
(example '(or #f #t #t) #t)
(example '(or #t #f #f) #t)
(example '(or #t #f #t) #t)
(example '(or #t #t #f) #t)
(example '(or #t #t #t) #t)


(example '(and #f #f #f) #f)
(example '(and #f #f #t) #f)
(example '(and #f #t #f) #f)
(example '(and #f #t #t) #f)
(example '(and #t #f #f) #f)
(example '(and #t #f #t) #f)
(example '(and #t #t #f) #f)
(example '(and #t #t #t) #t)


(define freddy (let ((x 2)) (+ x 5)))
(let ((x 2)) (+ x 5)) 7)


;Section 6 of the Scheme Definition.

;6.1 Booleans


;not returns #t if obj is false, and returns #f otherwise.


(example '(not #t) #f)
(example '(not 3) #f)
(example '(not #f) #t)
(example '(not '()) #f)
(example '(not (list)) #f)
(example '(not 'nil) #f)


(example '(boolean? #f) #t)
(example '(boolean? 0) #f)
(example '(boolean? '()) #f)


(define (and-test b1 b2)
    (and b1 b2))


(example '(and-test #f #f) #f)
(example '(and-test #t #f) #f)
(example '(and-test #f #t) #f)
(example '(and-test #t #t) #t)




; some cadr... variants are defined in compile_Scheme

(example '(cdar '((1 2)(3 4))) '(2))
(example '(caddr '(1 2 3 4))  3)

(example '(list 22 33 44) '(22 33 44))
;list* is not required by IEEE Scheme
;(example '(list* 11 23 33) '(11 23 . 33))


(example '(append '(x) '(y)) '(x y))
(example '(append '(x) '(y) '(22 33))   '(x y 22 33))

(example '(+ 2 3 4) 9)
(example '(- 3 4 5) (- 6))
(example '(- 3 4)  (- 1))

(example '(length '(22 33 44)) 3)


(example '(reverse '(a b c)) '(c b a))

(example '(last '(33 4 45)) 45)


(example '(nth-cdr 2 '(a b c d)) '(c d))



(example '(eqv? 23 23) #t)
(example '(eqv? '(a b c) '(a b c)) #f)

(example '(eqv? 'a 'a) #t)
(example '(eqv? 'a 'b) #f)
(example '(eqv? 2 2) #t)
(example '(eqv? '() '()) #t)
(example '(eqv? 100000000000 100000000000) #t)
(example '(eqv? (cons 1 2) (cons 1 2)) #f)
(example '(eqv? (lambda () 1) (lambda () 2)) #f)
(example '(eqv? #f 'nil) #f)
(example '(let ((p (lambda (x) x))) (eqv? p p)) #t)

;The following choices are optional.

(example '(eqv? "" "") #t)
(example '(eqv? (lambda (x) x) (lambda (x) x)) #f)
(example '(eqv? (lambda (x) x) (lambda (y) y)) #f)


(define gen-counter
  (lambda ()
   (let ((n 0))
      (lambda () (begin(set! n (+ n 1)) n)))))

(define l1 '(2 3 4))
(set! (car l1) 22)
(example '(car l1) 22)


(set! (cadr l1) 99)
(example '(cadr l1) 99)


(example '(let ((g (gen-counter)))
   (eqv? g g))  #t)

(example '(eqv? (gen-counter) (gen-counter)) #f)

(define gen-loser
  (lambda ()
    (let (( n 0))
        (lambda () (begin (set! n (+ n 1)) 27)))))

(example
  '(let ((g (gen-loser)))
       (eqv? g g))
#t)

(example '(eqv? (gen-loser) (gen-loser)) #f)  ; can be either.

(example
'(letrec
  ((f (lambda () (if (eqv? f g) 'f 'both)))
  (g (lambda () (if (eqv? f g) 'both 'g))))
(eqv? f g)) #f)

;(example
;    '(do (( vec (make-vector 5))
;         (i 0 (+ i 1)))
;        ((= i 5) vec)
;        (vector-set! vec i i ))
;    #(0 1 2 3 4)
;    )


(example
    '(let ((x '(1 3 5 7 9)))
        (do ((x x (cdr x))
             (sum 0 (+ sum (car x))))
            ((null? x) sum)))
25
)

; ?? Tests of eq? omitted.

; ?? tests of equal? omitted

(example '(equal? '(1 2 3) '(1 2 3)) #t)

; 6.3 Pairs and Lists.


(define x (list 'a 'b 'c))
(define y x)
(example 'y '(a b c))
(example '(list? y) #t)
(example '(set-cdr! x 4)  4)
(example 'x  '(a . 4))
(example '(eqv? x y) #t)
(example 'y '(a . 4))
(example '(list? y) #f)



;(set-cdr! x x)
;?? infinite print out
; ???  we get true here .... (list? x)


(example '(pair? '(a . b)) #t)
(example '(pair? '(a b c)) #t)
(example '(pair? '()) #f)
(example '(pair? '#(a b)) #f)


(example '(cons 'a '()) '(a))
(example '(cons '(a) '(b c d)) '((a) b c d))
(example '(cons "a" '(b c)) '("a" b c))
(example '(cons 'a 3) '(a . 3))
(example '(cons '(a b) 'c) '((a b) . c))

(example '(car '(a b c)) 'a)
(example '(car '((a) b c d)) '(a))
(example '(car '(1 . 2)) 1)
;(car '()) - error


(example '(cdr '((a) b c d)) '(b c d))
(example '(cdr '(1 . 2)) 2)
;(cdr '()) - error

; Note - we allow a constant list to be changed.


(example '(null? '()) #t)
(example '(null? '(a . b)) #f)

(example (list? '(a b c)) #t)
(example (list? '(a . b)) #f)
;??? list?
(example '(let ((x (list 'a))) (set-cdr! x x) (list? x)) #f)


(example '(list 'a (+ 3 4) 'c) '(a 7 c))

(example '(list)  '())

(example '(length '(a b c)) 3)
(example '(length '(a (b) (c d e))) 3)
(example '(length '()) 0)


(example '(append '(x) '(y)) '(x y))
(example '(append '(a) '(b c d)) '(a b c d))
(example '(append '(a (b)) '((c))) '(a (b) (c)))
(example '(append '(a b) '(c . d)) '(a b c . d))
(example '(append '() 'a) 'a)
(example '(append '(a) '(b) '(c d)) '(a b c d))

(example '(reverse '(a b c)) '(c b a))
(example '(reverse '(a (b c) d (e (f))) ) '((e (f)) d (b c) a) )



(example '(list-tail  '(a b c d) 2) '(c d))

(define (list-ref list k)
   (car (list-tail list k))
)

(example '(list-ref '(a b c d) 2) 'c)



(example '(memq 'a '(a b c)) '(a b c))
(example '(memq 'b '(a b c)) '(b c))
(example '(memq 'a '(b c d)) #f)
(example '(memq (list 'a) '(b (a) c)) #f)
(example '(member (list 'a) '(b (a) c)) '((a) c))
(example '(memq 101 '(100 101 102)) '(101 102))           ; unspecified
(example '(memv 101 '(100 101 102)) '(101 102))



;(let ((ee '((a 1) (b 2) (c 3)) ))
;(example '(assoc 'a ee) '(a 1))
;)




(define e '((a 1) (b 2) (c 3)))
(example '(assq 'a e) '(a 1))
(example '(assq 'b e) '(b 2))
(example '(assq 'd e) #f)
(example '(assq (list 'a) '(((a)) ((b)) ((c)))) #f)
(example '(assoc (list 'a) '(((a)) ((b)) ((c)))) '((a)))
(example '(assq 5 '((2 3) (5 7) (11 13))) '(5 7))   ; unspecified
(example '(assv 5 '((2 3) (5 7) (11 13))) '(5 7))

;6.4 Symbols

(example '(symbol? 'foo) #t)
(example '(symbol? (car '(a b))) #t)
(example '(symbol? "bar") #f)
(example '(symbol? 'nil) #t)
(example '(symbol? '()) #f)
(example '(symbol? #f) #f)

(example '(symbol->string 'flying-fish) "flying-fish")
(example '(symbol->string 'Martin) "martin")
(example '(symbol->string (string->symbol "Malvina")) "Malvina")


; 6.5 Numbers

; ??? mluch to be done here

(zero? 0)



;6.5.6 Numerical input and output.


;6.6 Characters.


(example '(char->integer #\a) 97)
(example '(char->integer #\A) 65)
(example '(char->integer #\() 40)
(example '(char->integer #\ ) 32)
(example '(char->integer #\space) 32)
(example '(char->integer #\newline) 10)


(example '(char? #\a) #t)

(example '(char=? #\a #\a) #t)
(example '(char=? #\a #\b) #f)


(example '(char<? #\A #\B) #t)
(example '(char<? #\a #\a) #f)
(example '(char<? #\a #\b) #t)
(example '(char<? #\b #\a) #f)
(example '(char<? #\0 #\9) #t)

(example '(char>? #\a #\a) #f)
(example '(char>? #\a #\b) #f)
(example '(char>? #\b #\a) #t)

(example '(char<=? #\a #\a) #t)
(example '(char<=? #\a #\b) #t)
(example '(char<=? #\b #\a) #f)

(example '(char>=? #\a #\a) #t)
(example '(char>=? #\a #\b) #f)
(example '(char>=? #\b #\a) #t)


(example '(char-ci=? #\a #\A) #t)
(example '(char-ci=? #\A #\a) #t)
(example '(char-ci=? #\a #\b) #f)

(example '(char-ci<? #\a #\a) #f)
(example '(char-ci<? #\a #\b) #t)
(example '(char-ci<? #\b #\a) #f)


(example '(char-ci>? #\a #\A) #f)
(example '(char-ci>? #\a #\b) #f)
(example '(char-ci>? #\b #\a) #t)

(example '(char-ci<=? #\a #\a) #t)
(example '(char-ci<=? #\a #\b) #t)
(example '(char-ci<=? #\b #\a) #f)

(example '(char-ci>=? #\a #\a) #t)
(example '(char-ci>=? #\a #\b) #f)
(example '(char-ci>=? #\b #\a) #t)

(example '(char-alphabetic? #\a) #t)
(example '(char-alphabetic? #\() #f)


(example '(char-numeric? #\a) #f)
(example '(char-numeric? #\9) #t)
(example '(char-numeric? #\+) #f)
(example '(char-numeric? #\.) #f)

(example '(char-upper-case? #\a) #f)
(example '(char-upper-case? #\() #f)
(example '(char-upper-case? #\A) #t)


(example '(char-lower-case? #\A) #f)
(example '(char-lower-case? #\() #f)
(example '(char-lower-case? #\a) #t)

(example '(char->integer #\A) 65)
(example '(integer->char 65) #\A)

(example '(char-upcase #\a) #\A)
(example '(char-downcase #\A) #\a)


; 6.7 Strings

(example '(string? "freddy") #t)
(example '(string? "The word \"recursion\" has many meanings") #t)
(example '(string? 23) #f)
(example '(make-string 5 (integer->char 66)) "BBBBB")

(example '(make-string 5) (make-string 5 (integer->char 0)))

(example '(string-length "ABCD") 4)
(example '(string-ref "ABCD" 2) #\C)


(example '(string-ref (string-set "ABCD" 2 #\X) 2) #\X )
; this example depends on string-set returning the string as result.


(example '(string=? "ABCD" "ABCD") #t)
(example '(string=? "ABCD" "ABCDE") #f)

(example '(string-ci=? "AbCD" "ABCD") #t)
(example '(string-ci=? "ABCD" "ABCDE") #f)

(example '(string<? "ABCD" "ABCD") #f)
(example '(string<? "ABCD" "ABCDE") #t)
(example '(string<? "ABCD" "ABXY") #t)
(example '(string<? "AXCD" "ABCD") #f)


(example '(string<=? "ABCD" "ABCD") #t)
(example '(string<=? "ABCD" "ABCDE") #t)
(example '(string<=? "ABCD" "ABXY") #t)
(example '(string<=? "AXCD" "ABCD") #f)

(example '(string>? "ABCD" "ABCD") #f)
(example '(string>? "ABCD" "ABCDE") #f)
(example '(string>? "ABCD" "ABXY") #f)
(example '(string>? "AXCD" "ABCD") #t)


(example '(string>=? "ABCD" "ABCD") #t)
(example '(string>=? "ABCD" "ABCDE") #f)
(example '(string>=? "ABCD" "ABXY") #f)
(example '(string>=? "AXCD" "ABCD") #t)


(example '(string-ci<? "AbCD" "ABCD") #f)
(example '(string-ci<? "AbCD" "ABCDE") #t)
(example '(string-ci<? "AbCD" "ABXY") #t)
(example '(string-ci<? "AXCD" "AbCD") #f)


(example '(string-ci<=? "AbCD" "ABCD") #t)
(example '(string-ci<=? "AbCD" "ABCDE") #t)
(example '(string-ci<=? "AbCD" "ABXY") #t)
(example '(string-ci<=? "AXCD" "ABCD") #f)

(example '(string-ci>? "AbCD" "ABCD") #f)
(example '(string-ci>? "AbCD" "ABCDE") #f)
(example '(string-ci>? "AbCD" "ABXY") #f)
(example '(string-ci>? "AXCD" "AbCD") #t)


(example '(string-ci>=? "ABCD" "AbCD") #t)
(example '(string-ci>=? "ABCD" "AbCDE") #f)
(example '(string-ci>=? "ABCD" "AbXY") #f)
(example '(string-ci>=? "AXCD" "AbCD") #t)


; 6.9 Control Features

(example '(procedure? car) #t)
(example '(procedure? 'car) #f)
(example '(procedure? (lambda (x) (* x x))) #t)
(example '(procedure? '(lambda (x) (* x x))) #f)

(example '(apply + (list 3 4)) 7)
(example '(apply + 10 (list 3 4)) 17)

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

(example '((compose sqrt *) 12 75) 30)

(define (reduce l f acc base)
    (if (null? l) base
        (acc (f (car l)) (reduce (cdr l) f acc base))
        )
    )

(example '(reduce '((2) (3)) car + 7) 12)

(example '(map cadr '((a b) (d e) (g h))) '(b e h))
(example '(map (lambda(n) (expt n n)) '(1 2 3 4 5))
             '(1 4 27 256 3125))
(example '(map + '(1 2 3) '(4 5 6)) '(5 7 9))
(example '(map cons '(a b c) '(4 5 6)) '((a . 4) (b . 5) (c . 6)))

for-each  ; ??? needs attention


(define g (open-output-file "~/temp.tmp"))
(example '(output-port? g) #t)
(display "hello\n" g)
(display '(+ 3 4) g)
(display "a" g)
(newline g)
(close-output-port g)

(define f (open-input-file "~/temp.tmp"))
(close-input-port f)
(define f (open-input-file "~/temp.tmp"))
(example '(input-port? f) #t)
(example '(read f) 'hello)
(example '(read f) '(+ 3 4))
(example '(read-char f) #\a)
(example '(read-char f) #\newline)
(example '(eof-object? (read-char f)) #t)
(example '(eof-object? (read-char f)) #t)
(example '(input-port? (current-input-port)) #t)

(example '(load "test_for_load.scm") #t)

(display "DO NOT FORGET TO DO make-vector, and uncomment the make-vector
          example")

(define class_point (record-class 'point '(full full)))
(define cons_point (car class_point))
(define sel_point (caddr class_point))
(define x_point (car sel_point))
(define y_point (cadr sel_point))
(define p1 (cons_point 2 4))
(example '(x_point p1) 2)
(example '(y_point p1) 4)
(example '(props_record p1) 'point)

(define class_funny (record-class '(funny this) '(full full)))
(define cons_funny (car class_funny))
(define sel_funny (caddr class_funny))
(define x_funny (car sel_funny))
(define y_funny (cadr sel_funny))
(define p1 (cons_funny 2 4))

(example '(props_record p1) '(funny this))

(example '(->string p1) "<funny  2 4 >")

; (macro jane (lambda (x) (display x)))

(define level_of_user old_level_of_user)

(example '(sqrt 4)  2)
(example '(cos 0) 1.0)
(example '(sin 0) 0.0)
