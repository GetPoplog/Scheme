

;;; This is a method to typeset the Scheme conditional (if A B C) in
;;; tabular form.

define:TsM:lisp if(E,S) -> D_ts;
  lvars
    E,S, D_ts;
  unless arity_S(E,S) = 3 then
    ts_default(E,S) -> D_ts;
    return
  endunless;
  lvars
    S1   = with_no_paren(S),
    A    = ts_label(1,E,S1),       ;;; Get the typeset forms of the
    B    = ts_label(2,E,S1),       ;;; arguments A,B,C
    C    = ts_label(3,E,S1),       ;;;
                                  ;;; Now specify the layout
    D_ts =
                  vbox(#|
                      hbox (#| chars('if ',S),
                               A, chars(' then ',S), B|#,S),
                      hbox (#| chars('else ',S),C |#,S)
                      |#,S)


    ;
enddefine;
