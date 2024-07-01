

define:TsM:scheme begin(E,S);

  let i,
    n         = arity_S(E,S),                ;;; Get number of sub-terms
    S1        = with_no_paren(S),            ;;; no parentheses needed
    E_body    = vbox(#|
                  for i from 1 to n do
                    ts_label(i,E,S)
                  endfor|#,S),
   E_def      =
                  vbox(#|
                      chars('begin ',S),
                      hbox (#| chars('  ',S), E_body |#,S),
                      chars('end',S)
                      |#,S)
   in
      framebox(E_def,S)
   endlet

enddefine;



define:TsM:scheme sequence(E,S);
  ts__begin(E,S);
enddefine;
