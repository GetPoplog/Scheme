
datatype 'a Maybe = OK of 'a | NotSo;

datatype Token = N of string | I of int
                | R of real | S of string | V of string
                | Begin | End | OBR |CBR |SC | Com ;

(* This is a Scheme-style definition of term*)

datatype Term = B of Token | Ap of Term * Term list;

(*
datatype Expr = B of Token | Ap of Token * Term list | Sum of Term * Term
                | Product of Term * Term;
datatype Statement = Assignment of Token * Expr | If of Expr * Expr * Expr;

datatype 'a Parse = P of 'a * Token list;

*)


datatype Parse = P of Term * Token list;

fun member x [] = false
  | member x (y::l) = if x=y then true else member x l ;


fun mk_parser_singleton l [] = NotSo
  | mk_parser_singleton l (tok::toks) =
        if member tok l then OK(P(B(tok),toks))
        else NotSo;

val parse_x = mk_parser_singleton [V("x")];
parse_x [V("x"), I(2)];

fun number [] = NotSo
  | number (I(i)::toks) = OK(P(B(I(i)), toks))

datatype Parse = P of Term * Token list;
  | number (tok :: toks) = Fail;


fun mk_parse_seq p1 p2 bld [] = Fail
  | mk_parse_seq p1 p2 bld toks =
    let val r1 = p1 toks
    in
       case r1 of
          Fail => fail
       |  OK(term1,toks1) =>
               let val r2 = p2 toks1
               in
                   case r2 of
                      Fail => Fail
                      OK(term2,toks2) =>
    ;

val a = ref(23);

int * a = malloc(sizeof int);
* a = 23;

a := 3;
!a;

case [] of  [] => 34 | x::y => x;
