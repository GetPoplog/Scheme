






vars procedure(
  ts__declare
);

define:TsM:lisp unless(D,S);
  lvars D,S,n = arity_S(D,S),
    D_declare   =  mk_term('unless',
                       push_value_sub_terms(1,n,4,D,S),
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
