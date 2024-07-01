

vars procedure(ts__let, ts_bindings);

define:TsM:lisp setf(E,S);
  lvars E,S,
    E_bndg  =  ts_assign(false,E,S),        ;;; The typeset form

    ;

    E_bndg;
enddefine;


/*

ts__let( [let [[x 1] ] 1],S_rm.S_lisp) =>

** framedbox(10, 10, c, c, vbox(hbox_gen(0, 0.2, Let , lab(1, table(row(l,
     c, l), row(lab(0, 0, x), =, lab(0, 1, 1))))), in, lab(2, hbox(    ,
     1))))


ts__let( [let [[x a+1] [y 2]] [+ x y]],S_rm.S_lisp) =>

*/
