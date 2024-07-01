
;;; memo_T_2.p                                 R.J.Popplestone apr 92.

;;; provides memoising for a function of two arguments.
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
      L_memoed = [];

define pop_after_gc();
;;;  npr('after gc');
  for P in L_memoed do
     newassoc([]) -> frozval(2,P);
  endfor;
  sys_after_gc_old();
enddefine;

;;; Memoises a function f of two arguments - uses == i.e. exact equality.
;;; This does a two-stage lookup - the first argument is used to map
;;; to a property which maps the second argument to the function value.

;;; vars n_same = 0, n_mem_tot = 0; (statitistics indicate
;;; it is probably worth the test (== fastsubscr etc).

define memo_T_2(f) -> f1;
  lvars f,f1 , undf = 'undf';
  define f_mem(x,y,f,P,vv) -> v;
    lvars x y f P vv v;
;;;    1 + n_mem_tot -> n_mem_tot;
    if   x == fast_subscrv(1,vv)         ;;; Exactly the same args as prev. ??
    and  y == fast_subscrv(2,vv)
    then                                 ;;; Yes - use previous result.
;;;      1 + n_same -> n_same;
      fast_subscrv(3,vv)
    else lvars P1 = P(x);                ;;; Get property assoc. with 1st arg
      if P1 then                         ;;; If there is one, use it to look
   		P1(y) ->> v;                     ;;; up value for second arg
        if v == undf then                ;;; No value?
          	-> ;                         ;;; Clear off the undf
          f(x,y) ->> P1(y);              ;;; and write computed value into
        endif;                           ;;; the property
                                         ;;; value of function now on stack
      else                               ;;; no property for first arg.
		newproperty([],32,undf,"tmparg") ;;; get a new one
        	->> P(x) -> P1;                ;;; and associate it with 1st. arg.
      	f(x,y) ->> P1(y);                ;;; evaluate the function and write
  		endif;                             ;;; to property, leaving result on stk.

;;;      if is_after_gc then                ;;; Clear out the memory after
;;;        newassoc([]) -> frozval(2,vv(4)); ;;; garbage collection.
;;;        false -> is_after_gc;
;;;      endif;
	endif -> v;
    x -> fast_subscrv(1,vv);             ;;; Record immediate memo results.
    y -> fast_subscrv(2,vv);
    v -> fast_subscrv(3,vv);
	enddefine;

  if f.isclosure                         ;;; De-memoise if necessary.
  and pdpart(f) = f_mem then
    frozval(1,f) -> f
  endif;
  lvars
    P  = newanyproperty([],32,1,20,identfn,false,"tmparg",false,false);
  f_mem(%f,P,{%undf,undf,undf,undf%}%)
      -> f1;
  f1 -> frozval(3,f1)(4);                 ;;; supports purging.
  f1 :: L_memoed -> L_memoed;
enddefine;



example memo_T_2;
vars ff = memo_T_2(nonop +);
ff(4,5)=>
** 9
ff(4,5) =>
** 9
ff(9,8)=>
** 17
ff(9,6) =>
** 15
endexample
/*
pr_proc(ff);
frozval(3,ff)=>
*/

lvars pr_old = class_print(datakey(sin));

define lvars pr_prop(a,P);
  pr('arg 1 = '); pr(a); nl(1);
  appproperty(P, procedure(b,v); pr('arg_2 = '); pr(b); pr(' v = ');
                         pr(v); nl(1);
                 endprocedure);
enddefine;

define pr_proc(P);
  if isclosure(P) and pdprops(pdpart(P)) = "f_mem" then
     pr('memo ');
     pr(frozval(1,P)); nl(1);
     appproperty(frozval(2,P), pr_prop);
  else
    pr_old(P);
  endif;
enddefine;


pr_proc -> class_print(datakey(sin));
