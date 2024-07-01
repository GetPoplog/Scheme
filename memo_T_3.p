;;; memo_T_3.p                             Robin Poppletone MAY 1996

;;; Memoises a function f of three arguments.


/* (C) Copyright, University of Massachusetts, June 1994, All Rights Reserved
 *
 * This program carries no warranty.
 *
 * send bug reports and suggestions to pop@cs.umass.edu
 *

This program may reproduced for  academic and experimental purposes,  provided
the above attribution is preserved and extended as appropriate.

Commercial rights are reserved.


define memo_T_3(f);
  lvars f, UNDEF = 'UNDEF';
  define f_mem(x,y,z,f,P) -> v;
    lvars x y z f P v,
      	 P1 = P(x),                      ;;; Get property assoc. with x
         P2,                             ;;; Property assoc with y
    		 ;
    if P1 then                           ;;; If there is one, use it to look
   		P1(y) -> P2;                       ;;; up property assoc with (x,y)
      if P2 then                         ;;; If there is one then
   			P2(y) ->> v;                     ;;; up value for second arg
        if v == UNDEF then               ;;; No value?
          	-> ;                         ;;; Clear off the UNDEF
          f(x,y,z) ->> P2(y);              ;;; and write computed value into
        endif;                           ;;; the property
        ;                                ;;; value of function now on stack
      else                               ;;; no property for first arg.
				newproperty([],32,UNDEF,"tmparg")   ;;; get a new one
        	->> P1(y) -> P2;               ;;; and associate it with 2nd. arg.
      	f(x,y,z) ->> P2(z);              ;;; evaluate the function and write
  		endif                              ;;; to property, leaving result on stk.
    else                                 ;;; P1 is not defined
      newassoc([]) ->> P1 -> P(x);       ;;; Get a new property for P1
			newproperty([],32,UNDEF,"tmparg")     ;;; and a new one for P1(y)
        ->> P1(y) -> P2;                 ;;; and associate it with 1st. arg.
      f(x,y,z) ->> P2(z);                ;;; evaluate the function and write
		endif -> v;

	enddefine;

	if f.isclosure                         ;;; De-memoise if necessary.
	and frozval(1,f) = f_mem then
  	frozval(1,f) -> f
	endif;
	f_mem(%f,newassoc([])%)

enddefine;
