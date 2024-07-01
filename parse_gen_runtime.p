;;; The run-time capability for parse_gen.p

;;; This now uses the more efficient tree_memo_2_n procedure, which requires
;;; an exact (==) match.

uses tree_memo_1_n;



section PARSE_GEN tree_memo_1_n => call_parse_gen;

vars procedure(
  check_as_class,
  pumpstack
);

vars L_max,
     LW_reserved,            ;;; The list of reserved words.
     NT_tried,
     NT_tried1,
     tok_wanted,
;

/*


Record the "High Water Mark", i.e. the farthest point along the input stream
that the  parser  has  reached. L is the list of tokens, tok is the token
(if unique) that the parser was looking for, and n is the calling depth
of the nearest user-defined non-terminal symbol.  This procedure supports
error reporting.

This procedure updates the  non-local  variables
   L_max          The input list of tokens at this point.
   tok_wanted     A token that, if it had been  present, would give rise
                  to a legal parse
   NT_tried       The grammatical class that we were trying to recognise.
   NT_tried1      The grammatical class that called NT_tried.
In general, the HWM may be reached in more than one way.

*/

define record_HWM(L,tok,n);
  lvars L,tok,n;
    unless ispair(L.back)              ;;; Is the dynamic list L unexpanded?
    and  ispair(L.back.back)
    then                               ;;; Yes - so must be HWM.
        L -> L_max;
        tok -> tok_wanted;
        caller(n) -> NT_tried;
        caller(n+1) -> NT_tried1;
    endunless
enddefine;

;;; This procedure is called by a compiler.

define call_parse_gen(file,P_tok,Parse) -> L_out -> T;
  lvars file,Parse,
      P_tok,P_parse,L_out,T,
      P_rep = if isstring(file) then discin(file)
              elseif isprocedure(file) then file
              else mishap('file or repeater needed',[^file]);
              endif,
      L = pdtolist(P_tok(P_rep));
  dlocal
       tok_wanted = '??dont know',        ;;; Initialise tok_wanted
       L_max      = false;                ;;; Holds the maximum excursion
  Parse(L) -> L_out -> T;                 ;;; Parse the list of tokens.
  lvars i = 10, L1 = L;                   ;;;

  unless L_out=[]  then                   ;;; Have we read all the tokens.
    while ispair(L.back)                  ;;; No - must be error
    and ispair(L.back.back) do            ;;; go through list
      L.back -> L;
      i - 1 -> i;
      if i<0 then L1.tl->L1
      endif;
    endwhile;

     lvars
       tok_found = if null(L) then 'end of input'
                      else hd(L);
                      endif,
        L_here =
            [%for i from 1 to 10 do
             quitif(null(L1));
             L1.hd; L1.tl -> L1;
            endfor; %];
    if isprocedure (tok_wanted) then
      'something satisfying ' >< pdprops(tok_wanted) -> tok_wanted
    endif;
    mishap('Parse failed:\n looking for "' >< tok_wanted
            >< '" found "' >< tok_found
            >< '" parsing "' >< NT_tried.pdprops
            >< '"\n called from "' >< NT_tried1.pdprops
            >< '"\n context before error.. ' ><
            L_here,
           []);
  endunless;
enddefine;



;;; This procedure recognises that the head of the token-list L has
;;; a property -is_it-.

define recog(L,is_it) -> L1 -> Obj;
    lvars L,L1,Obj,is_it;
    if null(L) then               ;;; If we have a null list, then
        is_it -> tok_wanted;      ;;; record failure.
        false -> L_max;
      return(false -> L1);
    endif;
    dest(L) -> L1 -> Obj;         ;;; Look at the list
    unless is_it(Obj) then        ;;; is the first token of the reqd type
        record_HWM(L,is_it,2);      ;;; NO - record High Water Mark for
        false -> L1               ;;; error reporting, and indicate failure.
    endunless;
enddefine;

define caller_prev(i) -> (P,i);
  lvars i, P = caller(i);
    if isclosure(P) then caller_prev(i+1)
    endif;
;;;  if pdprops(P) = "f2" then caller_prev(i+1)
;;;  endif;
enddefine;


;;; Does the list L begin with the token tok?

define check(L,tok) -> L;
  lvars L,tok,i;
  if null(L) then
    false -> L_max;
    tok -> tok_wanted;
    caller_prev(1) -> (NT_tried,i);
    caller_prev(i+1) -> (NT_tried1,i);
    false
  elseif    L.hd = tok then L.tl
  else  false;                                 ;;; parse has failed
    record_HWM(L,tok,                          ;;; Record for benefit of
      if caller(1) = check_as_class            ;;; the user
      then 3
      else 2
      endif);
  endif -> L;
enddefine;


define check_as_class(L,Word) -> L -> Word;
  lvars L,Word;
  check(L,Word) -> L
enddefine;



;;; Define various recognisers.

vars procedure(
  id_any            = recog(%isword%),
  obj_any           = recog(% procedure x;lvars x; true endprocedure %),
  int               = recog(%isinteger%),
  real              = recog(%isreal%),
  number            = recog(%isnumber%),
  string            = recog(%isstring%),
  semicolon         = check_as_class(%";"%),
  rightarrow        = check_as_class(%"->"%),
  less              = check_as_class(%"<"%),
  left_single_quote   = check_as_class(%consword(`\``,1)%),
  right_single_quote  = check_as_class(%consword(`\'`,1)%),

);



define identifier(L) -> L1 -> Obj;
    lvars L,L1,Obj;
    if null(L) then
      'an identifier' -> tok_wanted;
      return(false -> L1)
    endif;
    dest(L) -> L1 -> Obj;
    unless isword(Obj)
    and not(member(Obj,LW_reserved))
    then
       unless ispair(L.back) and ispair(L.back.back) then
         'an identifier' -> tok_wanted;
          L -> L_max;                    ;;; no - make up report.
        caller(1) -> NT_tried;
        caller(2) -> NT_tried1;
       endunless;
        false -> L1;
    endunless;
enddefine;

;;; <lambda> is the empty sequence (not to be confused with the empty set).

define lambda(L) -> L -> W;
  lvars L,W;
  [] -> W;
enddefine;

;;; The null grammar - generates the empty set of sequences.

define null_G(L) -> L -> W;
    lvars L,W;
    false -> L
enddefine;



/*
 Grammar is
<expr_p>
    -> <aexpr> <op> <expr_p>
    -> <unary> <expr_p>
    -> <expr_p>

*/

;;; Parse an expression using operator precedence. L is the list of tokens, Parse_aexpr
;;; is the parser for recognising atomic expressions, i.e. those that are combined together
;;; with the operators, Parse_op is the parser to recogise (binary) operators. Parse_unary
;;; is the parser to recognise unary operators. Prec is a procedure which returns the
;;; precedence of binary operators. Prec_unary is a procedure which returns the precedence
;;; of unary operators. is_more_binding(Op1,Op2) returns true if Op1 is a more binding
;;; operator than Op2. Prec_start is the initial precedence.

define Parse_expr_p(L,Parse_aexpr,Parse_op,Parse_unary,
                    Prec,Prec_unary,is_more_binding,Prec_start,mk_trm);
  lvars
       L,L1,
       Parse_aexpr,Parse_op,Parse_unary,
       Prec,Prec_unary,is_more_binding,
       Prec_start,mk_trm,
       T_expr,T_op,
       Stk_op = [],                         ;;; Operators and expressions are stacked up
       Stk_expr = [];                       ;;; on these while more binding ops. are found.

  repeat                                    ;;; Keep parsing atomic expressions
    Parse_aexpr(L) -> (T_expr,L1);          ;;; Recognise <aexpr>
    unless L1 then                          ;;; No atomic expression found?
      return(undef,false);
    endunless;

    if L then                                 ;;; ???????? error here
      Parse_op(L1)    -> (T_op,L);            ;;; Is
    else
      Parse_unary(L) -> (T_op,L1);
      if L1 then
        'unary_flag'::Stk_expr -> Stk_expr;
      else
        return(Stk_op.hd,L);
      endif
    endif;

    lvars p = if L then Prec(T_op)            ;;; Get precedence of operator
              else Prec_start                 ;;; - default to Prec_start
              endif;

    T_expr :: Stk_expr -> Stk_expr;           ;;; Stack the atomic expression.

    pumpstack(p,Stk_op,Stk_expr,              ;;; Build expressions using stacked up
              Prec,is_more_binding,mk_trm)    ;;; more binding operators.
      -> (Stk_op,Stk_expr);

    unless L then
      return(Stk_expr.hd,L1)
    endunless;

    T_op   :: Stk_op   -> Stk_op;
  endrepeat
enddefine;

;;; Remove expressions and operators from the stack, making
;;; new expressions until a less-binding operator is encountered.

define pumpstack(p,Stk_op,Stk_expr,Prec,is_more_binding,mk_trm)
        -> (Stk_op,Stk_expr);
  lvars p,Stk_op,Stk_expr,Prec,is_more_binding,procedure(mk_trm);
  if Stk_op.null                    ;;; Is enough stuff stacked up to
     or Stk_expr.null               ;;; try making new expressions?
     or Stk_expr.tl.null
  then return
  endif;

  lvars p1 = Prec(Stk_op.hd);       ;;; Get precedence of stacked operator
  if isnumber(p1) and
    (p>0 and (is_more_binding(p1,p)
              or p1 == p)
     or p<0 and is_more_binding(p1,p))
  then
     mk_trm(Stk_expr.tl.hd,
            Stk_expr.hd,
            Stk_op.hd,2)
           ::Stk_expr.tl.tl
           ->Stk_expr;
     Stk_op.tl -> Stk_op;
     pumpstack(p,Stk_op,Stk_expr,Prec,is_more_binding,mk_trm)
       -> (Stk_op,Stk_expr);
  endif;
enddefine;


/* example
uses parse_gen;

;;; Recognise a POP-11 identifier

define Parse_op(L);
  lvars Wd = L.hd;
  if Wd.identprops.isnumber then
     dest(L)
  else
     (undef,false)
  endif
enddefine;


uses terms;

Parse_expr_p([a-b*c-c;,.],    ;;; L
              dest,           ;;; Parse_aexpr - accepts anything
              Parse_op,       ;;; Parse_op
              Parse_op,       ;;; Parse_unary
              identprops,     ;;; Prec
              identprops,     ;;; Prec_unary
              nonop =<,       ;;; is_more_binding
              100,          ;;; Initial precedence.
              mk_term) =>

Parse_expr_p([a = b*c+d;,.]


Parse_expr_p([b*c+d;,.]
Parse_expr_p([a=b*c+d;,.]
Parse_expr_p([a-b-c,.]
identfn-> Prec;
*/


endsection;

vars parse_gen_runtime = true;
