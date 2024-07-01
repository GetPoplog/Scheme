


/*  memo.p                                    R.J.Popplestone June 1990 */

section Memoise => memo_gen, memo_n, ,de_memo, memoise memo;  ;;; Modularise the capability.

;;; This is used as an auxiliary function by  memo_gen, to record the
;;; f(x) when it has not been previously evaluated.

define record(x,P,f);     ;;; Record the value of f(x) in property P
    lvars x,P,f;
    f(x) ->> P(x);        ;;; associate f(x) with P(x), AND return it as
enddefine;                ;;; result of the procedure.

;;; The procedure  memo_gen  returns a property which memoises the function  f,
;;; f is a function of ONE argument.
;;; f  is used to generate new values in the property when no value exists.
;;; by a call to the auxiliary function record.

define memo_gen(f,f_hash,f_eq,b_gc,v_default);
  lvars f,f_hash,f_eq,b_gc,v_default;
  newanyproperty([],        ;;; no pre-defined attribute-value pairs.
                 32,        ;;; Initial table size.
                 2,         ;;; Expansion shift
                 false,     ;;; Default expansion threshold.
                 f_hash,    ;;; the hashing function
                 f_eq,      ;;; the comparison function
                 b_gc,      ;;; action on GC -  see ref newanyproperty
                 v_default, ;;; The default value
                 record(%f%);;; The closure of the record procedure
                )
enddefine;

define memo1(f);                 ;;; A simple-to-use version of memo_gen
  lvars f;
  memo_gen(f,syshash,nonop =,false,0)
enddefine;

/* The function memo_n  memoises a function  f  of  n arguments.
It does this by creating a function   f2  which is logically the same function
as  f, except that the call  f(x1,...xn) is replaced by  f2(vec_x), where
vec_x is a vector formed of (x1...xn). I.e. we have replaced the 'light weight
tuple x1...xn  on the stack by a 'heavy weight tuple' vec_x  on the heap.
The 'commented arguments' /* x1...xn */ indicate where these light-weight
tuples are passed into functinos.

All 3 function variables are lexical, so that the closures will be correctly
formed.

*/

define memo_n(f,n,m);                     ;;;
  lvars
    n,m,
    f;
;;;    n = pdnargs(f);                 ;;; Extract  n  from the function  f
/*
  if n=1 and m=1 then
            return(memo1(f))           ;;; memo1 works ok for function of 1 arg.
  endif;
*/
  define lvars f2(vec_x)/* -> vec_y*/;
    lvars vec_x;
    consvector(f(explode(vec_x)),m)                 ;;; convert vec_x to the x1...xn stack
  enddefine;

  lvars f3 = memo1(f2);               ;;; f3 is a memoised version of f2

  define lvars f1(/*x1...xn,*/ ); ;;;
    lvars vec_x = consvector(/*x1...xn,*/ n);
        destvector(f3(vec_x))->;
  enddefine;

;;;  n -> pdnargs(f1);
  f -> pdprops(f1);
  return(f1)

enddefine;

define macro memoise(x);
  lvars x;
  x,1, ".","memo_n", "->",x,";"
enddefine;

define de_memo(f);         ;;; interim.
  lvars f;
  pdprops(f)
enddefine;

vars memo = true;
endsection;
