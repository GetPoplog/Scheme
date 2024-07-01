
/*
GenericDebugger.p                       Robin Popplestone JAN 1996

Provides a generic debugging capability for the Scheme and C environments.

/* (C) Copyright, University of Massachusetts, June 1994, All Rights Reserved
 *
 * This program carries no warranty.
 *
 * send bug reports and suggestions to pop@cs.umass.edu
 *

This program may reproduced for  academic and experimental purposes,  provided
the above attribution is preserved and extended as appropriate.

Commercial rights are reserved.

*/

Supporting debugging
--------------------
Debugging  is supported by the "trace" facility of POP-11, and
by language-specific generation of extra code.

*/

;;; the procedures defined outside



vars
    chain_callers     = [],

    Class_err_local   = 'expression',
    Context_err       = false,
    Context_err_local = false,
    Context_err_def   = false,
    debugging_?       = false,
    do_break_?        = false,
    depth_max_GDB     = 1000000,
    expr_now          = false,
    index_ln_GDB          = newassoc([]),
    line_now          = undef,
    n_VarFrames       = 3,
    show_all_?,
    show_chars_GDB    = false,
    shown_GDB         = false,
    val_now,
    ;

;;; procedures exported
vars procedure(
    breakpoint_GDB,
    breakpont_stmt_GDB);

;;; procedures defined in this file.

vars procedure(
    get_source,
    first_function,
    pr_callers
    setup_DebuggerPanel

);

define Add_to_Callers(obj);
      obj::chain_callers -> chain_callers;
enddefine;


;;; The breakpoint function for non-X use of debugger.
vars procedure table_break = newassoc([]);
"next" -> table_break(`n`);
setpop -> table_break(`a`);

procedure();
    let c = charin()
    in
      if c=< `\s` then   ;;; white space etc.?
        pr_callers(chain_callers, 'current value: ', n_VarFrames);
      elseif c>= `0` and c<= `9` then
        pr_callers(chain_callers, 'current value: ', c-`0`)
      else npr('digit 1-9 needed: ' ><consstring(c,1))
      endif
    endlet
endprocedure -> table_break(`f`);


procedure();
  length(chain_callers) -1 -> depth_max_GDB;
endprocedure -> table_break(`s`);

lconstant pop_longstrings_old = pop_longstrings;
true -> pop_longstrings;

procedure();
    npr( '
         a    = abort current computation
         f    = print current variable frame
         f<n> = print n variable frames
         n    = proceed to next break-point.
         s    = skip - do not break until exit from current function
         ');
endprocedure -> table_break(`?`);

define get_source(proc);
  if isclosure(proc) then get_source(pdpart(proc))
  else source_of_proc(proc);
  endif;
enddefine;

pop_longstrings_old -> pop_longstrings;

define breakpoint(expr,line,stmt_?);
    dlocal popprompt = 'BK>';
    expr -> expr_now;
    line -> line_now;
    unless stmt_? then
            ->> val_now;
    endunless;
    if do_break_? and length(chain_callers) <= depth_max_GDB
    then
        100000-> depth_max_GDB;
        let f = first_function(chain_callers),
            file = get_source(f)
        in
            if file and vedediting
                ;;;            and vedediting and cucharin == charin and iscaller(vedsetpop)
            then
                if islist(file) then hd(file) -> file
                endif;
                let path_old = vedpathname in
                    vededitor(vedveddefaults,file);
                    line -> vedline;
                    vededitor(vedveddefaults, path_old);
                endlet
            endif;
            if stmt_? then
                printf('BREAK! in %p after %p\n', [^f ^expr]);
            else
                printf('BREAK! in %p after %p = %p\n', [^f ^expr^val_now]);
            endif
        endlet;
        repeat
            let c = charin(), cmd = table_break(c)
            in
                if cmd == "next" then return
                elseif cmd then cmd();
                    if c==`s` then return
                    endif
                elseif c>=`\s` then
                    spr('illegal BREAK command character:'>< consstring(c,1));
                    pr('- type ? for help');
                endif
            endlet
        endrepeat
    endif;
enddefine;

breakpoint(%true%) -> breakpoint_stmt_GDB;
breakpoint(%false%) -> breakpoint_GDB;



/*
  compile_debug generates code to record variable identifiers for debugging
----------------------------------------------------------------------------
*/

define compile_debug_GDB(Name,Vars,Env);
    if debugging_?
          and Context_Env(Env)
          and not(popexecute)
    then
        sysLOCAL("chain_callers");
        let v in
            for v in Vars do
                sysPUSHQ(v);
                sysIDENT(v)
            endfor;
            sysPUSHQ(2*length(Vars));
            sysCALLQ(consvector);
            sysCALLQ(Add_to_Callers);
        endlet
    endif
enddefine;

lconstant systrace_pr_old = systrace_pr;
lconstant Msg_warn_trace =
  'Warning - system problem - function possibly ' <>
   'not returning one result - alternatively are you tracing a polyadic ' <>
   'function like + - * append?';

define systrace_pr_GDB(Name, Status, Args, Upd);
    lvars i;
    unless is_symbol_GDB(Name) then
        systrace_pr_old(Name,Status,Args,Upd);
        return;
    endunless;
    dlocal cucharout, tracing = false;
    ;;;    if cuchartrace then cuchartrace -> cucharout;
    ;;;    endif;
    for i from 1 to poptraceindent-1 do
        cucharout( if i&&1==1 then `|` else `\s` endif)
    endfor;

    if Upd then spr("updater") endif;
    if Status then
        if isnumber(Status) then cucharout(Status); nl(1);
        endif;
        pr_args_GDB(Name,Args);
    else
        spr(Name); pr(' = '); appdata(Args,spr); pr('\n');
        if length(Args) /== 1 then
           npr(Msg_warn_trace);
        endif;
    endif

enddefine;

/*
The GUI
-------
setup_EditPanel();
*/

'setting up GUI' =>

false -> poplog_ui_enabled;        ;;; Do not use standard user interface.

uses propsheet;
lvars Propbox_gdb = false;

define setup_EditPanel();
    if Propbox_gdb then return
    endif;
    lvars AW_edit =  XtAppCreateShell('EditPanel', 'OK',
                            xtApplicationShellWidget,
                            XptDefaultDisplay,
                            [{allowShellResize ^true}]);

    propsheet_new_box(EditPanel_GDB, AW_edit, false, [])
            -> Propbox_gdb;

    lvars Propsheet_gdb = propsheet_new(EditPanel_GDB,
                                              Propbox_gdb, false);

    lvars fname = 'unnamed' >< FileExt_default_GDB;
    propsheet_field(Propsheet_gdb, [[File ^fname ]]);
    propsheet_field(Propsheet_gdb, [['Do Edit' false ]]);
    propsheet_field(Propsheet_gdb, [['Exit' false ]]);
    propsheet_field(Propsheet_gdb, [['Help' false ]]);

    propsheet_field(Propsheet_gdb, [[Debugger false ]]);
;;;    propsheet_field(Propsheet_gdb, [['VED Immediate Mode' false ]]);


    define do_help(ps,button,val);
        XptDeferApply(edit_GDB(%HelpFile_GDB%));
        false;
    enddefine;

    do_help -> propsheet_field_accepter(Propsheet_gdb, "Help");

    define do_edit(ps,button,val);
      XptDeferApply(edit_GDB(%val%));
      val;
    enddefine;

    do_edit -> propsheet_field_accepter(Propsheet_gdb, "File");


    define do_edit_1(ps,button,val);
        XptDeferApply(
          edit_GDB(%propsheet_field_value(Propsheet_gdb,"File")%));
        false -> propsheet_field_value(Propsheet_gdb,"'Do Edit'");
        false;
    enddefine;

    do_edit_1 -> propsheet_field_accepter(Propsheet_gdb, "'Do Edit'");

    setup_DebuggerPanel -> propsheet_field_accepter(Propsheet_gdb,"Debugger");

    define do_exit(ps,button,val);
          sysexit();
    enddefine;

    do_exit -> propsheet_field_accepter(Propsheet_gdb, "'Exit'");


    define do_im(ps,button,val);
          ved_im();
    enddefine;

;;;    do_im -> propsheet_field_accepter(Propsheet_gdb, 'VED Immediate Mode');

    propsheet_show([% Propsheet_gdb, Propbox_gdb %]);

enddefine;

/*
Updating a field after a specified time. This has to be done this way because
sys_timer does not handle closures.
*/

lvars Msg_time, Propsheet_time,field_time;

define lconstant proc_time ;
  Msg_time -> propsheet_field_value(Propsheet_time,field_time);
  pr_callers(chain_callers,' Frames ', n_VarFrames);
enddefine;

define set_timer(Msg,Propsheet,field,time);
  Msg -> Msg_time;
  Propsheet -> Propsheet_time;
  field -> field_time;
  time ->  sys_timer(proc_time);
enddefine;


/*
setup_EditPanel();
*/

lconstant line_of_dashes =
'_____________________________________________________________________________\n';

lvars Propbox_debug = false;

define setup_DebuggerPanel(ps,button,val);
    if Propbox_debug then return(false)
    endif;


    lvars AW_debug =  XtAppCreateShell('Debugger', 'OK',
                            xtApplicationShellWidget,
                            XptDefaultDisplay,
                            [{allowShellResize ^true}]);
    propsheet_new_box('Debug Panel', AW_debug, false, [])
        -> Propbox_debug;

    lvars Propsheet_debug = propsheet_new('UMASS SCHEME DEBUGGER',
        Propbox_debug, false);

    propsheet_field(Propsheet_debug, [[Trace 'my_function' ]]);
    propsheet_field(Propsheet_debug, [['Untrace' ^false ]]);
    propsheet_field(Propsheet_debug, [[Report '']]);
    propsheet_field(Propsheet_debug, [['Generate Debug Code' ^true ]]);
    propsheet_field(Propsheet_debug, [['Stop at All Breakpoints' ^false]]);
    propsheet_field(Propsheet_debug, [['Abort' ^false]]);
    propsheet_field(Propsheet_debug, [['Number of Frames' 1-10
                (default = ^n_VarFrames)]]);
    propsheet_field(Propsheet_debug, [['Go To Next Breakpoint' ^false ]]);
    propsheet_field(Propsheet_debug, [['Skip Current Function' ^false ]]);
    propsheet_field(Propsheet_debug, [[Help ^false ]]);
    propsheet_field(Propsheet_debug, [['Report POP-11 functions' ^false ]]);

    procedure();
        length(chain_callers) -1 -> depth_max_GDB;
    endprocedure -> table_break(`s`);

    define do_trace(ps,button,string);
        let
            symbol = tokenise_GDB(stringin(string<>' '))()
        in
            datalist(symbol) =>
            if isprocedure(valof(symbol)) then
                let code = [trace ^symbol] in
                    popval(code);
                endlet;
                'Trace set on function: '>< symbol
                    -> propsheet_field_value(Propsheet_debug,"Report");
            else
                'Error: '><symbol >< ' is not a function'
                    -> propsheet_field_value(Propsheet_debug,"Report");
            endif;
            string;
        endlet
    enddefine;

    do_trace -> propsheet_field_accepter(Propsheet_debug, "Trace");

    define do_untrace(ps,button,val);
        let  string = propsheet_field_value(
                Propsheet_debug,"Trace"),
            symbol = make_identifier_GDB(consword(deststring(string)))
        in
            if isprocedure(valof(symbol)) then
                let code = [untrace ^symbol] in
                    popval(code);
                endlet;
                'Any trace removed from: '>< symbol
                    -> propsheet_field_value(Propsheet_debug,"Report");
            else
                'Error: '><symbol >< ' is not a function'
                    -> propsheet_field_value(Propsheet_debug,"Report");
            endif;
            false
        endlet
    enddefine;

    do_untrace -> propsheet_field_accepter(Propsheet_debug, "Untrace");

    false -> propsheet_sensitive(Propsheet_debug,"Report");

    define do_Generate(ps,button,val);
        not(debugging_?) ->> debugging_?;
            ->> propsheet_field_value(ps,button);
        'Debugging mode turned ' ><
        if debugging_? then 'ON'
        else 'OFF' endif
            -> propsheet_field_value(Propsheet_debug,"Report");
    enddefine;

    do_Generate ->  propsheet_field_accepter(Propsheet_debug,
        'Generate Debug Code');

    define do_Stop(ps,button,val);
        not(do_break_?) ->> do_break_?;
            ->> propsheet_field_value(ps,button);
        if do_break_? then 'Will'
        else 'Will not' endif >< ' stop at breakpoints'
            -> propsheet_field_value(Propsheet_debug,"Report");
    enddefine;

    do_Stop ->  propsheet_field_accepter(Propsheet_debug,
        'Stop at All Breakpoints');

    define do_NumberBP(ps,button,val);
        val ->> n_VarFrames;
        let Msg = n_VarFrames >< ' variable frames will be reported'
        in
            set_timer(Msg,Propsheet_debug,"Report",10**6);
        endlet;
    enddefine;

    do_NumberBP ->  propsheet_field_accepter(Propsheet_debug,
        'Number of Frames');

    define do_abort(ps,button,val);
        'Current Computation Aborted' ->
        propsheet_field_value(Propsheet_debug,"Report");
        false -> propsheet_field_value(Propsheet_debug,'Abort');
        setpop();
    enddefine;

    do_abort -> propsheet_field_accepter(Propsheet_debug,'Abort');


    procedure(ps,button,val);
        length(chain_callers) -1 -> depth_max_GDB;

        true -> propsheet_field_value(Propsheet_debug,
            'Go To Next Breakpoint');
        false;
    endprocedure -> propsheet_field_accepter(
        Propsheet_debug, 'Skip Current Function');

    define do_help(ps,button,val);
        XptDeferApply(edit_GDB(%HelpDebug_GDB%));
        false;
    enddefine;

    do_help -> propsheet_field_accepter(Propsheet_debug, "Help");

    define do_Report(ps,button,val);
        not(show_all_?) ->> show_all_?; ->> propsheet_field_value(ps,button);
        if show_all_? then 'All'
        else 'No' endif >< ' POP-11 functions on call-stack will be shown'
            -> propsheet_field_value(Propsheet_debug,"Report");
    enddefine;

    do_Report ->  propsheet_field_accepter(Propsheet_debug,
        'Report POP-11 functions');


    define breakpoint_debug(expr,line,stmt_?);
        ;;; handle a break-point.
        expr -> expr_now;
        line -> line_now;

        unless stmt_? then
                ->> val_now;
        endunless;

        if do_break_? and length(chain_callers) <= depth_max_GDB then

            let Msg = '\nBREAK at line: ' >< line
            in
                npr(line_of_dashes);
                Msg   -> propsheet_field_value(Propsheet_debug,"Report");
                npr(Msg);
                if stmt_? then
                    printf('statement: %p ',[^expr]);
                else
                    printf('expression: %p ',[^expr]);
                endif
            endlet;

            let f = first_function(chain_callers),
                file = get_source(f)
            in
                if islist(file) then hd(file) -> file
                endif;
                if file and vedediting
                    ;;;            and vedediting and cucharin == charin and iscaller(vedsetpop)
                then
                    let path_old = vedpathname in
                        vededitor(vedveddefaults,file);
                        line -> vedline;
                        vededitor(vedveddefaults, path_old);
                    endlet
                endif;
            endlet;
            pr_callers(chain_callers,
                       if stmt_? then false
                       else '='
                       endif,
                       n_VarFrames);
            ;;;        line -> vedline;
            until propsheet_field_value(Propsheet_debug,
                                         'Go To Next Breakpoint')
            do
            enduntil;
            false -> propsheet_field_value(Propsheet_debug,
                'Go To Next Breakpoint');
            'running'  -> propsheet_field_value(Propsheet_debug,"Report");
        endif;
    enddefine;
    breakpoint_debug(%false%) -> breakpoint_GDB;
    breakpoint_debug(%true%) -> breakpoint_stmt_GDB;
    propsheet_show([% Propsheet_debug, Propbox_debug %]);
    false;

enddefine;



/*
setup_DebuggerPanel(1,2,3);  ;;; For testing purposes
*/

;;; To trace a procedure we can use the fact that the
;;; number of arguments is on the stack.
/*

vars systrace_proc_old = systrace_proc;


define systrace_proc_gdb(Proc,Name,Updater);
  if is_symbol_GDB(Name) then                     ;;; A top-level obj lang function?
    lvars args = consvector(/*...*/);         ;;; make a vector of the args
    systrace_pr(Name,">",args,Updater);       ;;; print the entry message
;;;    printf('args are %p Proc %p\n',[^args^Proc]);
    lvars result = consvector(Proc(destvector(args)),1);       ;;; there is ONE result
;;;    printf('result is %p\n',[^result]);
    systrace_pr(Name,`<`,                     ;;;
                result,                       ;;; so print the exit message
                Updater);
    subscrv(1,result);
  else systrace_proc_old(Proc,Name,Updater)   ;;; use old printer.
  endif;
enddefine;
*/

/*
  mishap_GDB(Message,Culprits) treats errors.
---------------------------------------------
*/

vars show_all_? = false;




define summarise_callers;
    false -> shown_GDB;
    let C = syscallers() in
        if C then
            pr('\nCalling sequence:\n');
            applist(C,pr_proc_GDB);
        endif;
    endlet;
    unless shown_GDB then
       pr('(none of your functions in call-chain)\n');
    endunless;
enddefine;

define first_function(Chain);
    lvars entry;
    for entry in Chain do
        unless isvector(entry) then return(entry)
        endunless
    endfor;
enddefine;


define pr_callers_1(Chain,n_VF,pr_fun_?);
    unless null(Chain) then
        let entry = hd(Chain) in
            if isvector(entry) then
                if pr_fun_? then
                    printf('\n\nFrame for function: %p\n',
                        [%first_function(Chain)%])
                else
                    printf('Variable Frame:\n',[]);
                endif;
                let n = datalength(entry),i
                in
                    for i from 1 by 2 to n do
                        let v = idval(entry(i+1)) in
                            if show_chars_GDB and isinteger(v) and
                                v>=32 and v<256 then
                                printf('    %p = %p (in ASCII \'%p\')\n',
                                    [%entry(i),v,consstring(v,1)%])
                            else
                                printf('    %p = %p\n',
                                    [%entry(i),v%])
                            endif;
                        endlet;
                    endfor;

                    pr_callers_1(tl(Chain),n_VF,false);
                endlet

            else
                n_VF - 1 -> n_VF;
                ;;;                printf('END of frame for function %p', [%entry%]);
                if n_VF == 0 then printf(' - (Last requested frame)\n');
                    pr(line_of_dashes);
                    return;
                else pr(newline);
                    pr(line_of_dashes);
                endif;
                pr_callers_1(tl(Chain),n_VF,true);
            endif

        endlet;
    endunless;
enddefine;

define pr_callers(Chain,msg,n_VF);
    if stacklength() > 2 then
        if msg then
            let TOS = /*...*/ in
                pr(msg); pr(TOS); nl(1); TOS
            endlet;
        endif;
    endif;
    pr_callers_1(Chain,n_VF,true);
enddefine;

lvars n_err=3;

define mishap_GDB(String,Culprits,Type_of_err);
    if show_all_? then

        sysprmishap(String,Culprits);
        printf('Type of error = %p\n',[^Type_of_err]);
        setpop();

    endif;
    pr('\n\nError: ');
    if Culprits and issubstring('%',String) then
        printf(String,Culprits);

    else
        pr(String);
        unless atom(Culprits) then
            pr('\nCulprits: '); applist(Culprits,pr_GDB<>pr(%', '%));
        endunless
    endif;
    if popfilename then printf('\nIn file: %p\n',[^popfilename]);
    endif;
    if issubstring('RT',Type_of_err) and  expr_now then
        printf('\nLast expression evaluated %p = %p\n on line %p',
            [%expr_now,val_now,line_now%]);
        summarise_callers();
    endif;

    if Context_err_local then
        printf('Compiling %p ending at line %p:\n    ',
            [%Class_err_local, index_ln_GDB(Context_err_local)%]);
        pr_GDB(Context_err_local);
        nl(1);
    endif;

    if Context_err then
        printf('In or around line %p: ',[%index_ln_GDB(Context_err)%]);
        pr_GDB(Context_err);
        nl(1);
    endif;

    if Context_err_def then
        printf('While defining \'%p\'\n',[%Context_err_def%]);
    endif;

    if Type_of_err = "Expression_RT" then
        pr_callers(chain_callers, '\nValue = ', n_VarFrames);
    else
   ;;; elseif Type_of_err = "Statement_RT" then
        pr_callers(chain_callers, false, n_VarFrames);
    endif;

    let UserName = sysgetusername(popusername),
        Critic   = if null(chain_callers) then
            'Jeremiah Jolt, your compile-time helper'
        else
            'Madeleine Misconception, your run-time assistant'
        endif
    in
        if popunderx and n_err>0 then
            printf(
                'This error report was prepared for %p\n by %p.\n\n',
                [^UserName ^Critic]);
            n_err -1->n_err;
        endif
    endlet;
    setpop();
enddefine;

;;; sysprotect("popexecute");

mishap_GDB(%"Statement_RT"%) -> prmishap;
mishap_GDB(%"Statement_RT"%) -> pop_default_prmishap;


/*
  mishap_wrong_args and mishap_arity are useful utility error functions.
------------------------------------------------------------------------
*/


define mishap_wrong_args(Name);
  ;;; printf('wrong number of args %p\n', [%Name%]);
  ;;; setpop();
  mishap_GDB('function " %p " called with wrong number of arguments',
     [^Name],"Expression_RT");
enddefine;


define mishap_arity(Name,m,n);
  mishap_GDB('Function "%p" needs %p arguments, was given %p',
     [%Name,m,n%], "Expression_RT")
enddefine;




define xsetup_GDB();
  sysxsetup();
  setup_EditPanel();
enddefine;


unless sys_machine_type = [pc] then
    uses vedemacs;
endunless;

'setting sub-system parameters for VED' =>

define :ved_runtime_action;
    ;;;    'changing arrow keys' =>
    vedsetkey('\^[[FF51', '\^B');      ;;; Define cursor keys for X-windows
    vedsetkey('\^[[FF52', '\^P');
    vedsetkey('\^[[FF53', '\^F');
    vedsetkey('\^[[FF54', '\^N');

    vedsetkey('\^[[D', '\^B');         ;;; Define cursor keys for terminal.
    vedsetkey('\^[[A', '\^P');
    vedsetkey('\^[[C', '\^F');
    vedsetkey('\^[[B', '\^N');

enddefine;


;;;lconstant popexit_old = popexit;

;;;lconstant Help_for_lock =
;;;  [
;;;   'The UMASS language environment does not allow you to write files that you have edited'
;;;   'when there appears to be another process using the environment in existence'
;;;   'in which any files have been edited.'
;;;   'This is a safeguard to prevent two different and conflicting  versions'
;;;   'of a file being created.'
;;;   'You may override this safeguard -an override may be necessary if the'
;;;   'previous  process has terminated abnormally.'
;;;  ];

;;; vars files_writable = true;

;;;define popexit();
;;;   if files_writable then
;;;     sysdelete('~/lock_ved.tmp');
;;;     'VED lock removed' =>
;;;   endif;
;;;   popexit_old();
;;;enddefine;

;;;lconstant vedinitialise_old = vedinitialise;

;;;define vedinitialise(item);
;;;    if item or files_writable then
;;;    else
;;;       false -> vedwriteable;
;;;    endif;
;;;    2 -> vedindentstep;
;;;    vedinitialise_old(item);
;;;enddefine;

define dialogue_lock(set_lock_?);
   'new file locking mechanism in use' =>
enddefine;

/*
define dialogue_lock(set_lock_?);
;;;    printf('dialogue_lock %p\n', [^set_lock_?]);
    if sys_file_exists('~/lock_ved.tmp') then
        'Another UMASS environment process is/was running VED' =>
        'Remove global file lock (at your own risk)? y-n-?' =>
        let Rep = incharitem(charin) in
            repeat
                let item = uppertolower(Rep()) in
                    if item == "y" then
                        sysdelete('~/lock_ved.tmp');
                        'Global file lock removed' =>
                        return;
                    elseif item == "n" then
                        false -> files_writable;
                        'WARNING : You will be unable to write any file that you edit' =>
                        return;
                    elseif item == "?" then applist(Help_for_lock,npr);
                    else 'answer y  for "yes" or n for "no" or ? for help' =>
                    endif
                endlet;
            endrepeat
        endlet
    elseif set_lock_? then
        let Rep = discout('~/lock_ved.tmp')
        in
            Rep(`l`); Rep(termin);
            npr('Lock placed on writing files in any other VED process');
        endlet;
    endif;
enddefine;
*/

vars GenericDebugger = true;
