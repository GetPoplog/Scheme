lib vedfindbracket;

lconstant Msg_wrong_char =
        'Warning: illegal character "%p" in input file %p';

lvars unwarned_wc = true;

define tokenise(Rep);
    lvars c_prev = false;                   ;;; overshoot character from prev
    procedure()->tok;
        lvars c, tok,type = "number";

        repeat                                ;;; (1) Scan blanks
            if c_prev then                      ;;; use overshoot character?
                c_prev, false -> c_prev
            else
                Rep()
            endif -> c;
            ;;;printf('scanning blanks: c=%p,c_prev = %p\n',[^c^c_prev]);
            if c==termin then return(c->tok);
            endif;
            if c == `;` then
                skip_comment(Rep); `\n`->c;
            endif;
        quitif ((subscrs(c,chartype)) /== 0);         ;;; get syntactic type
        endrepeat;
        make_token(#|                       ;;; (2) read token.
                repeat
                    if c==`;` then                ;;; comment ends a token.
                        skip_comment(Rep);
                        if c_prev then
                            `\n` -> c_prev;
                            quitloop
                        else Rep()->c;
                        endif;
                    endif;
                    ;;;printf('making token: c=%p\n',[^c]);
                    lvars t = subscrs(c,chartype);
                quitif(t==0);                    ;;; whitespace not incorporated
                    if t==5 and unwarned_wc then
                        nprintf(Msg_wrong_char, [%c,Rep%]);
                        false -> unwarned_wc;
                    endif;
                    if c==`"` then
                        stack_string(Rep);
                        "string" -> type;
                        quitloop;
                    endif;

                    if t/==2 then
                        if type == "number" and c==`/` then
                            "rational" -> type;
                        else
                            "identifier" -> type       ;;; Cannot be a number.
                        endif
                    endif;
                    c;
                    if c==`,` then Rep() -> c;   ;;; ,@ sticks together
                        if c == `@` then c
                        else c -> c_prev
                        endif ;
                        quitloop;
                    endif;
                    if c==`#` then Rep() -> c;   ;;; deal with # forms
                        if c == `(` or c == `f` or c == `t` or c == `\\` then c
                        else c -> c_prev
                        endif ;
                        if c/== `\\` then quitloop;
                        else  Rep() -> c;
                            unless isalphacode(c) then c; quitloop
                            endunless;
                            c
                        endif;

                    endif;
                quitif(t==3);                    ;;; Delimiter is complete as 1 char
                    Rep()-> c;
                    c->c_prev;
                    ;;;printf('new char = %p\n',[^c]);
                quitif( subscrs(c,chartype) == 3);
                endrepeat |#,type)

            -> tok;
        ;;;      dlocal pop_pr_quotes = true; spr(tok);
    endprocedure
enddefine;

define compile_Scheme(Rep, is_literate);
    ;;; printf('compile_Scheme of %p, %p\n', [^Rep^is_literate]);
    dlocal popfilename;
    if isstring(Rep) then Rep
    else vedpathname
    endif -> popfilename;
    ;;;    printf('popfilename set to %p in compile_Scheme',[^popfilename]);

    if isstring(Rep) or isdevice(Rep) then
        if isstring(Rep) and issubstring('.lscm',1,Rep) = length(Rep)-4

        then true -> is_literate;
        endif;
        discin(Rep) -> Rep
    endif;

    if is_literate then
        skip_text(Rep) -> Rep;
    endif;

    lvars read_item        = tokenise(Rep);
    dlocal cucharin = Rep;
    dlocal depth_max_GDB = 1000000;
    dlocal expr_now;
    dlocal pop_longstrings = true;
    dlocal popprompt = '=> ';
    ;;;    dlocal %class_print(boolean_key)% = pr_scm;
    ;;;    dlocal %class_print(word_key)%    = pr_scm;
    dlocal %class_print(undef_key)%          = pr_scm;
    dlocal %class_print(procedure_key)%      = pr_scm;
    dlocal %class_print(pair_key)%           = pr_scm;
    dlocal systrace_pr                       = systrace_pr_GDB;
    dlocal index_ln_GDB                      = newassoc([]);

    while true do
        lvars E = read_sexpr(read_item);
    quitif(E=termin);
        #_IF identprops("ts_Scheme") == 0
        ts_Scheme(E)
        #_ENDIF
        compile_expr(E,Env_init);
        lvars Val = exec_scm();
        pr(newline);
        pr_scm(Val); pr(newline);
    endwhile;
enddefine;

trace compile_Scheme isstring issubstring;
