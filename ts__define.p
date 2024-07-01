
;;; (define (f arg1...argn) body)
;;; (define a val)

define lconstant ts_define_fun(E,S);
  let i,
    n         = arity_S(E,S),                ;;; Get number of sub-terms
    S1        = with_no_paren(S),            ;;; no parentheses needed
    E_form    = ts_label(1,E,S),
    E_body    = vbox(#|
                  for i from 2 to n do
                    ts_label(i,E,S1)
                  endfor|#,S),
   E_def      =
                  vbox(#|
                      hbox (#| chars('Define ',S),
                               E_form,
                               /*chars(' as function',S)*/
                             chars(' = ',S)
                            |#,S),
                      hbox (#| chars('  ',S), E_body |#,S)
                      |#,S)
   in
      framebox(E_def,S)
   endlet
enddefine;

define lconstant ts_define_var(E,S);
    let
        n         = arity_S(E,S),                ;;; Get number of sub-terms
        S1        = with_no_paren(S),            ;;; no parentheses needed
        E_var     = ts_label(1,E,S),
        E_body    = ts_label(2,E,S),
        E_def      = hbox (#| chars('Define ',S),
                        E_var,
                        chars(' = ',S),
                        E_body |#,S)
    in
        framebox(E_def,S)
    endlet
enddefine;


define:TsM:scheme define(E,S);
    if isword(arg_S(1,E,S)) then
        ts_define_var(E,S)
    else
        ts_define_fun(E,S);
    endif;
enddefine;
