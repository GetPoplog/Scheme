
; merge_general.scm                              Robin Popplestone.

; merge_general provides the merge algorithm in a general form. It takes
; two ordered lists, l1 and l2 and proceeds to examine them in such a way
; that the smaller of the first elements of l1 and l2 is processed first.
; merge_general resembles reduce in that the result is built up by
; "accumulator" functions,
;
; The call (merge_general key compare combine acc base_1 base_2 l1 l2)
; has the following meanings.
;
; key is a function which extracts from the first member of l1 and l2 an
; element by which these members will be compared.
;
; compare is a function for comparing elements. (compare k1 k2) evaluates
; to '<  '= or '> depending on whether k1 is less than, equal to or greater
; than k2 according to some ordering relation (not necessarily arithmetic).
;
; The two accumulator functions combine and acc are used to build the
; result.  If two entries x1 and x2 have  equal keys, according to compare
; then (combine x1 x2 mg) is used to combine them into the result mg
; returned by the recursive call of merge_general. [Note that, because two
; entries have equal keys, this doesn't necessarily mean that they are equal]
;
; If two entries x1 and x2 have unequal keys, then (acc x mg) is used to
; combine x, the lesser of x1 and x2, with the result mg returned by
; the recursive call.
;
; the base_1 and base_2 functions are used to handle the case in which
; l2 and l1 respectively are empty. That is to say, if l1 is empty, then
; (base_2 l2) is the result returned by merge_general, and conversely.


(define (merge_general key compare combine acc base_1 base_2 l1 l2)
    (cond
        ((null? l1) (base_2 l2))
        ((null? l2) (base_1 l1))
        (else
            (let*
                (
                 (x1 (car l1)) (x2 (car l2))
                 (k1 (key x1)) (k2 (key x2))
                 (c  (compare k1 k2))
                 ); end let binding
                (cond
                    ((eq? c '<)
                     (acc
                         x1
                         (merge_general
                             key compare combine acc base_1 base_2 (cdr l1) l2)
                         )
                     ) ; end <

                    ((eq? c '=)

                     (combine x1 x2
                         (merge_general
                             key compare combine acc base_1 base_2
                             (cdr l1)
                             (cdr l2))
                         )
                     )


                    ((eq? c '>)
                     (acc
                         x2
                         (merge_general
                             key compare combine acc base_1 base_2
                             l1
                             (cdr l2))
                         )
                     )
                    ((pair? c) (car c))
                    (else (error "illegal result from comparison" c))
                    );end cond
                ) ;end let

            )
        )
    )
