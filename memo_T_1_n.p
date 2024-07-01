
;;; memo_T_1_n.p                                 R.J.Popplestone apr 92.

;;; provides memoising for a function of 1 argument and n results.
;;; 20sep92 Minor tidying and commenting.


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


lvars sys_after_gc_old = pop_after_gc,
      is_after_gc,
      L_memoed = [],
      undf = 'undf';

define pop_after_gc();
;;;  npr('after gc');
  lvars P;
  for P in L_memoed do
     newproperty([],32,undf,"tmparg") -> frozval(2,P);
  endfor;
  sys_after_gc_old();
enddefine;

;;; Memoises a function f of one argument - uses == i.e. exact equality.


;;; vars n_same = 0, n_mem_tot = 0; (statitistics indicate
;;; it is probably worth the test (== fastsubscr etc).

define memo_T_1_n(f,n) -> f1;
  lvars f,f1 , ,n;
  define lvars f_mem_1_n(x,f,P,vv,n) /* ->v1...vn*/;
    lvars x  f P vv v n;
    if   x == fast_subscrv(1,vv)         ;;; Exactly the same args as prev. ??
    then                                 ;;; Yes - use previous result.
      destvector(fast_subscrv(3,vv))->;
      return
    else lvars v = P(x);                ;;; Get property assoc. with 1st arg
        if v == undf then                ;;; No value?
          consvector(f(x),n)
             ->> P(x);              ;;; and write computed value into
        else v
        endif;                           ;;; the property
    endif -> v;
    x -> fast_subscrv(1,vv);             ;;; Record immediate memo results.
    v -> fast_subscrv(3,vv);
;;;    pr(v); sp(2) ; pr(vv); nl(1);
    destvector(v)->;
    enddefine;

  if f.isclosure                         ;;; De-memoise if necessary.
  and pdpart(f) = f_mem_1_n then
    frozval(1,f) -> f
  endif;
  lvars
    P  = newproperty([],32,undf,"tmparg");
  f_mem_1_n(%f,P,{%undf,undf,undf,undf%},n%)
      -> f1;
  f1 -> frozval(3,f1)(4);                 ;;; supports purging.
  f1 :: L_memoed -> L_memoed;
enddefine;



example memo_T_1_n;
define test(x); x+1,x*2; enddefine;
vars ff = memo_T_1_n(test,2);
ff(11)=>
** 12 22
ff(11)=>
** 12 22
ff(13) =>
** 14 26
ff(11)=>
** 12 22
endexample
/*
pr_proc(ff);
frozval(3,ff)=>
*/

lvars pr_old = class_print(datakey(sin));

define lvars pr_prop(P);
  lvars P;
 appproperty(P, procedure(b,v);
                         lvars b,v;
                         pr('arg_1 = '); pr(b); pr(' v = ');
                         pr(v); nl(1);
                 endprocedure);
enddefine;

define pr_proc(P);
  lvars P;
  if isclosure(P) and pdprops(pdpart(P)) = "f_mem_1_n" then
     pr('memo ');
     pr_prop(frozval(2,P));
  else
    pr_old(P);
  endif;
enddefine;


pr_proc -> class_print(datakey(sin));
