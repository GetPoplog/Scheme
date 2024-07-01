exception Test;

fun test(x)=
    case x of
        a::b => a
(*     |  [] => raise Test *)
;


(* fun test(a::b) = a; *)


test([2, 3, 4]);

test([]);
