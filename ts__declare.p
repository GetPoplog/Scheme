





vars procedure(
  ts__declare
);


define value_decl(D,S);
  lvars D,S,f = functor_S(D,S);
  if f = "type" then
    lvars Var = arg_S(1,D,S),
          Ty  = arg_S(2,D,S);

    $$$ [hbox_s 3 ^Var in ^Ty] $$$
  else value(D,S)
  endif;
enddefine;

define push_declared_sub_terms(m,n,n_indent,D,S);
  lvars m,n,n_indent,D,S,i,
    D_pad = consstring(for i from 1 to n_indent do ` ` endfor,n_indent);
  for i from m to n do
    lvars
    E_i = value_decl(arg_S(i,D,S),S),
    D_i = if n_indent==0 then
            E_i
          else
            QML'hbox(^D_pad,^E_i)'
         endif;
    D_label_n(i,D,S,D_i,1)
  endfor;
enddefine;


define:TsM:lisp declare(D,S);
  lvars D,S,n = arity_S(D,S),
    D_declare   =  mk_term('Declare',
                       push_declared_sub_terms(1,n,4,D,S),
                       "vbox",n+1),
    ;

   $$$ [framedbox 10 10 c c ^D_declare] $$$
enddefine;






/*

ts__declare( [declare [[x 1] ] 1],S_rm.S_lisp) =>

** framedbox(10, 10, c, c, vbox(hbox_gen(0, 0.2, Let , lab(1, table(row(l,
     c, l), row(lab(0, 0, x), =, lab(0, 1, 1))))), in, lab(2, hbox(    ,
     1))))


ts__declare( [declare [[x a+1] [y 2]] [+ x y]],S_rm.S_lisp) =>

*/
