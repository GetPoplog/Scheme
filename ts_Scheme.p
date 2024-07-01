
/*
ts_Scheme.p                                Robin Popplestone.

Allows Scheme abstract syntax to be presented in typographical form.

/* (C) Copyright, University of Massachusetts, June 1994, All Rights Reserved
 *
 * This program carries no warranty.
 *
 * send bug reports and suggestions to pop@cs.umass.edu
 *

This program may reproduced for  academic and experimental purposes,  provided
the above attribution is preserved and extended as appropriate.

Commercial rights are reserved.
*/

*/

uses DrawTAS;
lib  ts__cond;
uses ts__define;
uses ts__lambda;
uses ts__let;
uses ts__setf;
uses ts__begin;
uses ts__if;

mk_W('test',400,400,0,0) -> W_default;
;;;uses S_times_14;
set_Style_times14(W_default);
draw_ts(W_default,20,40,QTS[+ x y], S_rm_14 with_prec 1);

define mk_term_scheme(/* a1...an*/,f,n);
  let E = [] in
    repeat n times
      /*a_i*/::(E) -> E
    endrepeat;
    f::E
  endlet;
enddefine;

example mk_term_scheme
mk_term_scheme(33,44,"+",2)=>
** [+ 33 44]
endexample
  vars procedure(ts__let, ts_bindings);

lconstant with_access = Style$-with_access;
vars S_scheme = with_access((S_rm_14 with_prec 1),
                            ispair,
                            procedure(E); if ispair(E) then front(E) else
                                             E
                                          endif
                            endprocedure,
                            procedure(E); length(E) -1 endprocedure,
                            procedure(i,E); E(i+1)
                            endprocedure,
                            mk_term_scheme);


define ts_bindings(l,E,L,S);
    let i in
        if islist(L) then
            table(#|
                    for i from 0 to length(L)-1 do
                        ts_binding(l,i,E,S)
                    endfor
                |#, false,S)
        else
            mishap_ts('Bad form for binding',[^L]);
        endif;
    endlet
enddefine;

define ts_binding(l,i,E,S);
    let
        V_l = ts_label_n(l,i,0,3,E,S),
        D_l = ts_label_n(l,i,1,3,E,S)
    in
        {%chars('  ',S), V_l, chars('=',S),  D_l %};
    endlet
enddefine;


define push_typeset_sub_terms(m,n,n_indent,D,S);
    let i,
        D_pad = chars(consstring(for i from 1 to n_indent do ` ` endfor,
                                 n_indent),S)
    in
        for i from m to n do
            let
                E_i = typeset(arg_S(i,D,S),S),
                D_i = if n_indent==0 then
                            E_i
                      else
                            hbox(#| D_pad, E_i |#,S)
                      endif
            in
                ts_label(i,D,S)
            endlet
        endfor;
    endlet
enddefine;


define ts_assign(E,S);
    let i = 1, n = arity_S(E,S)
    in
        vbox(#|until i>n do
                    lvars
                        V_i = ts_label(i,E,S),
                        E_i = ts_label(i+1,E,S);
                    hbox(#|V_i, id("leftarrow",S),E_i|#);
                    i+2 -> i;

                enduntil|#,S)
    endlet
enddefine;

/*
clear_graphic(W_default);
draw_ts(W_default,20,20,  [/ x [+ y z]], S_scheme);
clear_graphic(W_default);
draw_ts(W_default,20,20, [let [[x 1] [y 2]] [/ x [+ y z]]], S_scheme);

clear_graphic(W_default);
draw_ts(W_default,20,20, [define [fred x] [+ x 1]], S_scheme)


vars F =
[define [fred x y]
   [let [[a_1 [+ x 2]] [b [+ x y]] ]
    [+ a_1 b]]];

clear_graphic(W_default);
draw_ts(W_default,20,20, F, S_scheme)


vars F = [define x 3];
clear_graphic(W_default);
draw_ts(W_default,20,20, F, S_scheme)


vars F = [lambda [x] 3];
clear_graphic(W_default);
draw_ts(W_default,20,20, F, S_scheme)

*/

define detwiddle(Expr);
  if isword(Expr) then allbutlast(1,Expr)
  elseif isstring(Expr) then '"'<>Expr<>'"'
  elseif atom(Expr) then Expr
  else conspair(detwiddle(front(Expr)), detwiddle(back(Expr)))
  endif;
enddefine;

define ts_Scheme(Expr);
  clear_graphic(W_default);
  let Expr1 = detwiddle(Expr)
  in
    draw_ts(W_default,20,20,Expr1,S_scheme);
  endlet
enddefine;
