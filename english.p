

define  Dictionary =
    newassoc([
        [+ ['the sum of %p and %p' add ]]
        [- ['the difference between %p and %p' subtract]]
        [* ['product of %p and %p' multiply]]
        [member ['%p is a member of %p' ]]
    ])
enddefine;


define Lookup(x);
  let name = Dictionary(x)  in
      if name then name
  endlet
enddefine;

define function = front;
enddefine;

lconstant Msg_if = '<UL> <LI> if %p then %p <LI> otherwise %p </UL>';


define e_method_if(Expr);
    sprintf(Msg_if,maplist(Args(Expr),English))
enddefine;

lconstant Msg_expr0 = 'call %p';
lconstant Msg_expr1 = 'the %p of %p';
lconstant Msg_expr2 = 'the %p of %p and %p';


define English(Expr);
    if islist(Expr) then
        let f = function(Expr), A = args(Expr), n = length(A), name_f = Lookup(f)
        in
            if n==0     then sprintf(Msg_expr0,[^name_f])
            elseif n==1 then sprintf(Msg_expr1,name_f::A)
            elseif n==2 then sprintf(Msg_expr2,name_f::(maplist(A,English)))
            else sprintf(Expr,[])
            endif
        endlet
    else
       Expr><''
    endif;
enddefine;



English([+ 2 [* 3 5]]) =>
English([member x [3 4 5]]) =>
