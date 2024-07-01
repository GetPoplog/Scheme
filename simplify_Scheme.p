
define simplify_scm(E,Env);
    if isword(E) then simplify_symbol(E,Env)
    elseif atom(E) then E
    else
        let f = front(E),
        args = back(E),
        simp_special = method_simp(E)
        in
        if simp_special then simp_special(args,Env)
        else simp_apply(f,args,Env)
        endif
        endlet
    endif;
enddefine;

define simplify_symbol(S,Env);
  let V = Env(S) in V or S
  endlet;
enddefine;
