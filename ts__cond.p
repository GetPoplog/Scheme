



;;; This is a method to typeset the Scheme conditional (if A B C) in
;;; tabular form.

define:TsM:scheme cond(E,S) -> D_ts;
  lvars
    E,S, D_ts;
  lvars
    S1   = with_no_paren(S),
    i,
    n    = arity_S(E,S),
    D_ts = ts_conditions(E,back(E),S1)


    ;
enddefine;



define ts_conditions(E,L,S);
    let i in
        if islist(L) then
            table(#|
                    for i from 1 to length(L) do
                        ts_condition(i,E,S)
                    endfor
                |#, false,S)
        else
            mishap_ts('Bad form for condition',[^L]);
        endif;
    endlet
enddefine;

define ts_condition(i,E,S);
    let c = arg_S(i,E,S) in
        if islist(c) and hd(c) == "else" then
              let D_l = ts_label_n(i,1,2,E,S)
              in
                 {% chars('else',S),chars(' ',S),chars(' ',S), D_l%}
              endlet
        else
            let
                V_l = ts_label_n(i,0,2,E,S),
                D_l = ts_label_n(i,1,2,E,S),
                str = if i==1 then 'if    ' else 'else if' endif
            in
                {%chars(str,S), V_l, chars('then',S),  D_l %};
            endlet
        endif
    endlet
enddefine;
