

datatype 'a Maybe = NotSo | OK of 'a;


datatype Token =
    I of int  |
    R of real |
    V of string ;


(* This is a Scheme-style definition of term *)

datatype Term = B of Token | Ap of Term * Term list;


datatype 'a Parse = Parse of 'a * Token list;

type 'a Parser = Token list -> 'a Parse Maybe;

type 'a Builder2 = 'a * 'a -> 'a;
type 'a Builder1 = Token -> 'a;

val member = List.member;


(*

fun mk_parser_singleton
        (class_of_toks:Token list)
        (bld:'a Builder1)
        (list_toks:Token list)

**** Finish this definition ****

fun mk_parser_seq
        (p1: 'a Parser)
        (p2: 'a Parser)
        (bld: 'a Builder2)
        (toks  = Token list)

**** Finish this definition ****


*)

fun mk_parser_singleton
        (class_of_toks:Token list)
        (bld:'a Builder1)
        (list_toks:Token list)
    =
        case list_toks of
            []       => NotSo
        |   (tok::toks) =>
                if member tok class_of_toks
                then OK(Parse(bld(tok),toks))
                else NotSo
;




fun mk_parser_seq
        (p1: 'a Parser)
        (p2: 'a Parser)
        (bld: 'a Builder2)
        (toks:  Token list)
  =
    let val r1 = p1 toks
    in
        case r1 of
            NotSo => NotSo
        |   OK(Parse(term1,toks1)) =>
            let val r2 = p2 toks1
            in
            case r2 of
                NotSo => NotSo
            |  OK(Parse(term2,toks2)) => OK(Parse(bld(term1,term2),toks2))
            end
    end;



case x of 2=>3 | 4=>5
