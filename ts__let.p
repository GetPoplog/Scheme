

vars procedure(
    ts_bindings
);

define:TsM:scheme let(E,S);
   ts_let_etc(E,S,"let");
enddefine;


define:TsM:scheme letrec(E,S);
   ts_let_etc(E,S,"letrec");
enddefine;

define ts_let_etc(E,S,W_let);
    let
        S1      = with_no_paren(S),
        n       = arity_S(E,S),
        E_bndg  = arg_S(1,E,S),                  ;;; Specifies var. bindings.
        D_bndg  =  ts_bindings(1,E,E_bndg,S1),        ;;; The typeset form
        D_bndg1 =  vbox(#|chars(W_let>< ' ',S), D_bndg|#,S),
        D_let   =  vbox(#|D_bndg1,
                          chars('in',S),
                          push_typeset_sub_terms(2,n,4,E,S1),
                         |#,S)
    in
;;;           D_let
        framebox(D_let,S)
    endlet
enddefine;


ts_let_etc(%consword('let*')%) -> method_TsM(consword('let*'));

/*

ts__let( [let [[x 1] ] 1],S_lisp(S_rm)) =>

*/
