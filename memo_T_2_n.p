

/*
 memo_T_2_n.p                                 R.J.Popplestone apr 92.



;;; 20sep92 Minor tidying and commenting.

This is an implementation by Robin Popplestone of the "memo function" idea of
Donald Michie. We use the term "function" for a non-updatable procedure.
The property functions of modern POP-11 are used to perform argument look-up.

Tests show that a memoised POP-11 function  can beat C by a handsome  factor
in the tak benchmark for big  arguments - not surprising since  memoisation
changes complexity.

Note that this requires lib example, which allows POP-11 programs to be
tested as they compile


/* (C) Copyright, University of Massachusetts, June 1994, All Rights Reserved
 *
 * This program carries no warranty.
 *
 * send bug reports and suggestions to pop@cs.umass.edu
 *

This program may reproduced for  academic and experimental purposes,  provided
the above attribution is preserved and extended as appropriate.

Commercial rights are reserved.


         CONTENTS - (Use <ENTER> g to access required sections)

 --  After garbage collection, memoised values are cleared out
 --  memo_T_2_n memoises a function f of two arguments and n results
 --  An example of use

*/
*/


/*
After garbage collection, memoised values are cleared out
----------------------------------------------------------
*/

lvars sys_after_gc_old = pop_after_gc,
      is_after_gc,
      L_memoed = [];

define pop_after_gc();
;;;  npr('after gc');
  lvars P;
  for P in L_memoed do
     newassoc([]) -> frozval(2,P);
  endfor;
  sys_after_gc_old();
enddefine;



/*
memo_T_2_n memoises a function f of two arguments and n results
--------------------------------------------------------------------
This function uses == i.e. exact  equality to compare arguments. It  does a
two-stage lookup - the first  argument is used to  map to a property  which
maps the second argument to the function value.
*/

;;; vars n_same = 0, n_mem_tot = 0; (statitistics indicate
;;; it is probably worth the test (== fastsubscr etc).

define memo_T_2_n(f,n) -> f1;
  lvars f,f1 , undf = 'undf',n;
  define f_mem(x,y,f,P,vv,n) /* ->v1...vn*/;
    lvars x y f P vv v n;
;;;    1 + n_mem_tot -> n_mem_tot;
    if   x == fast_subscrv(1,vv)         ;;; Exactly the same args as prev. ??
    and  y == fast_subscrv(2,vv)
    then                                 ;;; Yes - use previous result.
;;;      1 + n_same -> n_same;
      destvector(fast_subscrv(3,vv))->;
      return
    else lvars P1 = P(x);                ;;; Get property assoc. with 1st arg
      if P1 then                         ;;; If there is one, use it to look
        P1(y) ->> v;                     ;;; up value for second arg
        if v == undf then                ;;; No value?
            -> ;                         ;;; Clear off the undf
          consvector(f(x,y),n)
             ->> P1(y);              ;;; and write computed value into
        endif;                           ;;; the property
                                         ;;; value of function now on stack
      else                               ;;; no property for first arg.
        newproperty([],32,undf,"tmparg") ;;; get a new one
            ->> P(x) -> P1;                ;;; and associate it with 1st. arg.
        consvector(f(x,y),n)
                  ->> P1(y);                ;;; evaluate the function and write
        endif;                             ;;; to property, leaving result on stk.

;;;      if is_after_gc then                ;;; Clear out the memory after
;;;        newassoc([]) -> frozval(2,vv(4)); ;;; garbage collection.
;;;        false -> is_after_gc;
;;;      endif;
    endif -> v;
    x -> fast_subscrv(1,vv);             ;;; Record immediate memo results.
    y -> fast_subscrv(2,vv);
    v -> fast_subscrv(3,vv);
;;;    pr(v); sp(2) ; pr(vv); nl(1);
    destvector(v)->;
    enddefine;

  if f.isclosure                         ;;; De-memoise if necessary.
  and pdpart(f) = f_mem then
    frozval(1,f) -> f
  endif;
  lvars
    P  = newanyproperty([],32,1,20,identfn,false,"tmparg",false,false);
  f_mem(%f,P,{%undf,undf,undf,undf%},n%)
      -> f1;
  f1 -> frozval(3,f1)(4);                 ;;; supports purging.
  f1 :: L_memoed -> L_memoed;
enddefine;

/*
An example of use
------------------
*/



example memo_T_2_n;
vars ff = memo_T_2_n(nonop //,2);
ff(11,3)=>
** 2 3
ff(11,3) =>
** 2 3
ff(9,8)=>
** 1 1
ff(9,6) =>
** 3 1
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
