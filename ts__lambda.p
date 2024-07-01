

define :TsM:scheme lambda(E,S);
    hbox(#|id("lambda",S),
           ts_args(front(back(E)),S),
           ts_label(2,E,S)
           |#,S);
enddefine;

define ts_args(Args,S);
    if islist(Args) then
        let comma = id(",",S) in
            id("(",S),
            for A on Args do id(front(A),S),
                unless null(back(A)) then
                    comma
                endunless
            endfor;
            id(")",S),
        endlet;
    else id(Args,S); id(".",S);
    endif
enddefine;
