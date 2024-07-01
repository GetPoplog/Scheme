

;;; browser.p                       Robin Popplestone
;;;                                 September 1997

uses lexical_args;
compile_mode:pop11 +defpdr +varsch +constr;

/*

Character attributes

*/



uses browse_htmlo;
notify_user('loading html->htmlo translator');
vars file_now = '';

/*
ved lecture1.htmlo
true -> is_vedfile_local(ident vednormaltable,ved_current_file);
*/

;;; [['.html'] /*{vednormaltable false}*/] :: vedfiletypes->vedfiletypes;

vars file_now;
vars procedure (loc_href loc_name loc_img);
vars procedure method_html;
lvars is_underline = false;
lvars colour = 1;
lvars linemax = vedlinemax - 10;
lvars attr   = 0;
lvars stack = [];
lvars stack_a = [];
lvars not_pre_? = true;
lvars n_indent = 0;
lvars di_indent = 0;
lvars head_? = false;
lvars n_line;
lvars n_lines = 0;
lvars Tasks_line = [];
lvars start_line = vednullstring;
lvars n_lines = 0;

vars procedure(
    Browse,
    mishap_html,
    monitor,
    newlines,
    print_mishap,

);

/*
line-tasks take effect -after- a line of text has been output. They affect
subsequent lines.
*/

define add_line_task(T);
    T::Tasks_line -> Tasks_line;
enddefine;


define push(x);
    if x=="a" and member("a",stack) then
        mishap_html('Anchor within anchor',[]);
        delete("a",stack) -> stack;
    endif;
    unless x=="p" and ispair(stack) and hd(stack)=="p" then
        x::stack->stack;
    endunless;
enddefine;


define pop();
    if null(stack) then
        mishap_html('excess closing parenthesis',[]);
        "error"
    else
        hd(stack); tl(stack)->stack;
    endif
enddefine;

define push_a(x);
   x::stack_a->stack_a;
enddefine;

define pop_a();
    if null(stack_a) then
;;;        mishap_html('attribute stack empty - excess </..>?',[]);
        0;
    else
      hd(stack_a); tl(stack_a)->stack_a;
    endif
enddefine;

define spaces(n);
  consstring(#|repeat n times `\s` endrepeat|#)
enddefine;

define set_attr(n,b);
   if b then attr||2**(16+n) -> attr;
   else      attr&&~~2**(16+n) -> attr;
   endif;
enddefine;

/*
0 = bold;
1 = red
2 = green
3 = underline
4 = reverse
5 = underline
6 = flash
7... no effect
set_attr(1,true);
set_attr(1,false);
datalist(spaces(5))=[32 32 32 32 32] =>
** <true>
*/

;;; puts an indentation at the start of a line.

define indent(str,n);
   ;;; n_line sys_><
   spaces(n) <> start_line <> str;
   vednullstring -> start_line;
enddefine;

;;; reads until a closing ">" is found.

define read_>(source);
   until nextchar(source) == `>` do
   enduntil;
   false;           ;;; line not finished.
enddefine;

lconstant str_nl =  '';


define read_head(source,source_toks,sink,tv);
    if tv then push("head");
    endif;
    add_line_task(
        procedure();
            tv -> head_?;
        endprocedure);
    0;
enddefine;

;;;define read_body(source,source_toks,sink,tv);
;;;   0;
;;;enddefine;

define read_compiler(source,source_toks,sink,tv);
   0;
enddefine;




;;; ??? this is very interim - will work ok for me.
;;; <!---- this is a > sign ----!>

define read_comment(source,source_toks,sink,tv);
    monitor("comment",[]);
;;; text is skipped by the "read_command" function, using read_>
    false;
enddefine;

define read_paragraph(source,source_toks,sink,tv);
  if tv then push("p"), 2 else 0
  endif;
enddefine;

define read_pre(src,src_toks,snk,tv);
    add_line_task(procedure();not(tv) -> not_pre_? endprocedure);
    1;
    if tv then push("pre");
    endif;
enddefine;

define read_break(source,source_toks,sink,tv);
   1;
enddefine;


define read_meta(source,source_toks,sink,tv);
   false;
enddefine;

/*
define mishap_html(Msg,List);
   dlocal cucharout = identfn;
   nl(1);
   printf(Msg,List);
   nl(1);
enddefine;
*/

lvars Msg_mishap=false, List_mishap;

define mishap_html(Msg,List);
    Msg -> Msg_mishap;
    List -> List_mishap;
enddefine;

lconstant Msg_bad_slash =
    'Opening tag "%p" mismatched with closing tag "%p"';

define read_slash(source,source_toks,sink,tv);
    let cmd = uppertolower(source_toks()),
        m    = method_html(cmd)
    in

        let
            cmd1 = pop(),
        in
            again:
            unless cmd=cmd1 then
                if cmd1="p" then
                    pop() -> cmd1; goto again;      ;;; goto !horror!
                endif;

                mishap_html(Msg_bad_slash,[^cmd1^cmd])
            endunless;
        endlet;

        if m then m(source,source_toks,sink,false)
        else false
        endif;

    endlet
enddefine;


define attribute_command(source,source_toks,sink,tv,n,name);
   if tv then push(name); push_a(attr); set_attr(n,true);
   else pop_a() -> attr;
   endif;
   false;
enddefine;

vars do_monitor = 0;

define monitor(msg,l);
    dlocal pop_pr_quotes = false;
    if do_monitor&&1=1 then nl(1); nprintf(msg,l)
    endif;
enddefine;

lconstant Msg_no_eq =
'equal sign required instead of %p';

lconstant Msg_no_name =
'name in ".." quotes required instead of %p';

define read_name(source_toks);
  let eq_? = source_toks(),
      name_? = source_toks()
  in
     unless eq_? = "=" then
        mishap_html(Msg_no_eq,[^eq_?]);
     endunless;
     unless isstring(name_?) then
        mishap_html(Msg_no_name,[^name_?]);
        name_? sys_>< '' -> name_?;
     endunless;
     name_?;
  endlet;
enddefine;


lconstant Msg_no_other_img = 'cant handle image <img %p ...>';

define read_img(source,source_toks,sink,tv);

    let kind = uppertolower(source_toks()) in
        if kind = "src" then
            let name = read_name(source_toks) in
                ;;;sysobey('xv '<> name);
                add_line_task(
                    procedure();
                        n_line-1 -> loc_img(name);
                    endprocedure);
                appdata(
                    '[Mouse here to see figure "'<>name<>'"]',
                    procedure(c); c+6*2**16 endprocedure);
            endlet

        else mishap_html(Msg_no_other_img,[^kind]);
        endif
    endlet;
    1;
enddefine;



lconstant Msg_no_other = 'cant handle anchor <a %p ...>';

define read_anchor(source,source_toks,sink,tv);
    monitor('anchor tv=%p',[^tv]);
    if tv then push("a"); push_a(attr);

        let kind = uppertolower(source_toks()) in
            if kind = "href" then
                let name = read_name(source_toks), l=n_line in
                    set_attr(1,true); set_attr(2,true);  ;;; colour blue
                    set_attr(3,true);                    ;;; underline
                    add_line_task(
                        procedure();
                              l -> loc_href(name);
                        endprocedure);
                endlet
            elseif kind = "name" then
                let name =  read_name(source_toks) in
                    add_line_task(
                        procedure();
                            n_line-1 -> loc_name(name);
                        endprocedure);
                endlet
            else mishap_html(Msg_no_other,[^kind]);
            endif
        endlet;
    else pop_a() -> attr;
    endif;
    false;
enddefine;

define sprint_anchor(pair);
    dlocal pop_pr_quotes = true;
    sprintf('%p = %p', pair);
enddefine;


lconstant Msg_anchors =
'\n\n\n'<>
'++++++++++++++++++++++ANCHOR+++INFO+++++++++++++++++++++++++++++++++++';

define report_on_anchors(sink_ln);
    sink_ln(Msg_anchors);
    sink_ln('SOURCE \'' <> file_now <>'\'');

    sink_ln('HREF');
    let L_str = maplist(datalist(loc_href),sprint_anchor)
    in
     applist(L_str,sink_ln);
    endlet;

    sink_ln('NAME');
    let L_str = maplist(datalist(loc_name),sprint_anchor)
    in
     applist(L_str,sink_ln);
    endlet;


    sink_ln('IMG');
    let L_str = maplist(datalist(loc_img),sprint_anchor)
    in
     applist(L_str,sink_ln);
    endlet;

    sink_ln('END');
enddefine;


/*
read_ul arranges for the indentation to be increased/decreased by 4 after
current line is output.
*/

define read_ul(source,source_toks,sink,tv);
    add_line_task(
        procedure();
            n_indent + if tv then 4 else -4 endif -> n_indent;
        endprocedure);
    if tv then push("ul");
    endif; 1;
enddefine;


define read_li(source,source_toks,sink,tv);
    add_line_task
    (procedure();
            consstring(145,32,2) -> start_line;  ;;; "bullet"+space
        endprocedure);
    1;
enddefine;


/*
read_dl initiates a definition list.

*/

define read_dl(source,source_toks,sink,tv);
    add_line_task(
        procedure();
            di_indent + if tv then 8 else -8 endif -> di_indent;
            di_indent -> n_indent;
        endprocedure);
    if tv then push("dl");
    endif; 1;
enddefine;


define read_dt(source,source_toks,sink,tv);
    add_line_task
    (procedure();
            di_indent-8 -> n_indent;
        endprocedure);
    0;
enddefine;

define read_dd(source,source_toks,sink,tv);
    add_line_task
    (procedure();
            di_indent -> n_indent;
        endprocedure);
    1;
enddefine;


/*
Now follows code for handling headers.
*/

define read_header(source,source_toks,sink,tv,hn);
   if tv then push(hn); 2;
   else "h";
   endif;
enddefine;



define do_horiz_rule(source,source_toks,sink,tv);
    if tv then "hr" else false
    endif;
enddefine;

define test_chars();
  lvars i;
  for i from 32 to 255 do nl(1); spr(i); charout(i)
  endfor;
enddefine;


/*
test_chars();
*/

define method_html = newassoc( [
        [!-- ^read_comment]
        [/ ^read_slash]
        [a ^read_anchor]
        [b ^(attribute_command(%4,"b"%))]
;;;        [body ^read_body]
        [br ^read_break]
        [code ^(attribute_command(%1,"code"%))]
        [compiler ^read_compiler]
        [dd ^read_dd]
        [dt ^read_dt]
        [dl ^read_dl]                            ;;; sets up indentation.
        [em  ^(attribute_command(%3,"em"%))]
        [h1 ^(read_header(%"h1"%))]
        [h2 ^(read_header(%"h2"%))]
        [h3 ^(read_header(%"h3"%))]
        [h4 ^(read_header(%"h4"%))]
        [head ^read_head]
        [hr ^do_horiz_rule]
        [i  ^(attribute_command(%3,"i"%))]
        [img ^read_img]
        [meta ^read_meta]
        [p ^read_paragraph]
        [pre ^read_pre]
        [pre ^read_pre]
        [tt ^(attribute_command(%1,"tt"%))]
        [ul ^read_ul]
        [li ^read_li]
    ]);
enddefine;

define count = newproperty([],8,0,"perm");
enddefine;

define read_command(source,sink);
    let source_toks = incharitem(source),
        cmd = uppertolower(source_toks()),
        m   = method_html(cmd),
        n   = if iscompound(cmd) then length(cmd) else false endif,
    in
        unless n then
          mishap_html('Illegal tag "%p" found in HTML',[^cmd]);
          return;
        endunless;
        7 -> item_chartype(`"`,source_toks);
        5 -> item_chartype(`\'`,source_toks);
        monitor('command "%p" method_html %p',[^cmd^m]);
        if m then m(source,source_toks,sink,true);
            read_>(source_toks)->;
        elseif issubstring('!',1,cmd) == 1 then
            monitor('this is a comment',[]);
            if n>2 and issubstring('!>',n-2,cmd)
            then
                monitor('command string included "!>',[]);
                false;
            else
                monitor('reading to end of comment',[]);
                read_>(source_toks);
            endif
        else push(cmd);
            read_>(source_toks);
        endif;
    endlet;
enddefine;

lconstant Msg_no_num =
'The "#" sign in escape sequence "%p" is not followed by a number';

define read_escape(source);
    let c, s =
        consstring(#|
                repeat
                    source()->c;
                quitif(c==`;` or c<=32);
                    c;
                endrepeat
            |#)
    in
        if     s='lt' then `<`
        elseif s='gt' then `>`
        elseif s='quot' then `"`
        elseif s='amp'  then `&`
        elseif s/='' and s(1)=`#` then
            let n = strnumber(allbutfirst(1,s))
            in
                if n then n
                else mishap_html(Msg_no_num,[^s]);
                endif
            endlet
        else mishap_html('unknown & escape sequence "%p"',[^s])
        endif;
    endlet;
enddefine;


define read_line(source,sink);
    let was_space = true, n=0,done=false in
        consdstring(#|
                repeat
                    let c = source()
                    in
;;;                        charout(`|`);charout(c);

                        if   c==termin then
                            c -> done;
                            quitloop;
                        endif;

                        if c==`\n` and not(not_pre_?) then quitloop
                        endif;

                        if not_pre_? and
                            (c==`\n` or c==`\r` or c==`\t`) then `\s` -> c
                        endif ;

                        if   c==`\s` and not_pre_? and was_space then
                            nextloop;
                        endif;

                        if isnumber(c) and c<32 then nextloop;
                        endif;

                        if       c==`<` then
                            read_command(source,sink)->done;

                        quitif(done);
                            nextloop;

                        elseif   c==`&`  then read_escape(source)->c;


                        endif;

                        c==`\s` -> was_space;

                        if c == `\s` and n > linemax - n_indent
                        and not_pre_? then
                            quitloop
                        else
                            c+attr; n+1->n;
                        endif;
                    endlet
                endrepeat|#),done;
    endlet;
enddefine;

;;; puts out a horizontal line of characters c.

define put_line(n,c,sink_ln);
    newlines(1,undef,sink_ln);
    sink_ln(
           if do_monitor&&2=2 then
              n_line sys_>< ': ' sys_><
            consdstring(repeat n times c endrepeat,n);
          else
            consdstring(repeat n times c endrepeat,n);
          endif);
    0 -> n_lines;
    n_line+1->n_line;
enddefine;


define newlines(n,line,sink_ln);
    if n="hr" then                                 ;;; horizontal rule
        put_line(vedlinemax,129,sink_ln);
    elseif n="h" then newlines(1,undef,sink_ln);   ;;; header
        put_line(length(line),129+2**20,sink_ln)
    else
        repeat (n-1-n_lines) times
           if do_monitor&&2=2 then
              sink_ln(n_line sys_>< ': ');
          else
            sink_ln(str_nl);
          endif;
            n_line+1->n_line;
        endrepeat;
        n+n_lines->n_lines;
    endif;
enddefine;


define read_html(source,sink);
    dlocal pop_pr_quotes = true;

    0 -> attr;
    []->stack;
    []-> stack_a;
    0->n_indent;
    false-> is_underline;
    1 ->  colour;
    vedlinemax - 10 -> linemax;
    true ->  not_pre_?;
    false -> head_?;
    0 -> n_lines;
    [] -> Tasks_line;
    vednullstring -> start_line;
    1 -> n_line;
    newassoc([]) -> loc_href;
    newassoc([]) -> loc_name;
    newassoc([]) -> loc_img;

    let sink_ln =  vedfile_line_consumer(sink),
        done,l
    in
        repeat read_line(source,sink_ln) -> (l,done);
            monitor('Line read = \'%p\'',[%l%]);
            monitor(
                'attr=%p,stack=%p, stack_a=%p\nn_indent=%p head_?=%p',
                [%attr>>16,stack,stack_a,n_indent,head_?%]);
            if head_?  and done/==termin then
                applist(Tasks_line,apply);[]->Tasks_line;
                nextloop
            endif;
            if isnumber(done) and done<0 then
                newlines(-done,l,sink_ln); false -> done;
            endif;
            if do_monitor&&2=2 then
                sink_ln(n_line sys_>< ': ' sys_>< indent(l,n_indent));
            else
                sink_ln(indent(l,n_indent));
            endif;
            0 -> n_lines;
            n_line+1->n_line;
            print_mishap(sink_ln);
            if done==termin  then quitloop
            elseif done then newlines(done,l,sink_ln);
            endif;
            applist(Tasks_line,apply);
            [] -> Tasks_line;
        endrepeat;
        report_on_anchors(sink_ln);
        sink_ln(termin);
    endlet;
enddefine;



lconstant Msg_bad_html = '**** BAD HTML:  ';

define print_mishap(sink_ln);
    if Msg_mishap then
        sink_ln(sprintf(Msg_bad_html<>Msg_mishap,List_mishap));
        n_line+1->n_line;
        false -> Msg_mishap;
    endif;
enddefine;



;;;true->pop_pr_quotes;      ;;; seems to be needed for anchor output

/*
Browse('lecture9.html');

ved $cs287/public_html/lecture3.html

read_html(discin('$cs287/public_html/index.html'),'index.htmlo');
read_html(discin('$cs287/public_html/lecture1.html'),'lecture1.htmlo');
read_html(discin('$cs287/public_html/lecture2.html'),'lecture2.htmlo');
read_html(discin('$cs287/public_html/lecture3.html'),'lecture3.htmlo');
read_html(discin('$cs287/public_html/lecture2.html'),'lecture2.htmlo');
read_html(discin('$cs287/public_html/lecture2.html'),'lecture2.htmlo');
read_html(discin('$cs287/public_html/lecture2.html'),'lecture2.htmlo');
read_html(discin('$cs287/public_html/lecture2.html'),'lecture2.htmlo');
read_html(discin('$cs287/public_html/lecture10.html'),'lecture10.htmlo');
read_html(discin('test.html'),'test.txt');
read_html(discin('test_chars.html'),'test_chars.txt');
read_html(discin('test_head.html'),'test_head.txt');
*/



vars browser = true;
