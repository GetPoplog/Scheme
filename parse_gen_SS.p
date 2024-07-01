/*
parse_gen_SS.p                         Robin Poppletone MAY 1996
Provides POPLOG subsystem capabilities for parse_gen


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

*/



section PARSE_GEN;

vars procedure(
  init_default,
  mishap_ptx,
  parse_ved_decl,
  parse_subsystem_bindings,
  Rep_code,
  searchlists,
  vedsetup_default,
);

/*
new_subsystem sets up a subsystem for the language defined in a grammar
-----------------------------------------------------------------------
*/

define new_subsystem(Name,FileExt,Dir);
  lvars Name,FileExt,Dir,
     Compile = "compile_"<>Name;
  [% Name,                      ;;; subsystem name
     valof(Compile),            ;;; function to compile char. repeater.
     FileExt,                   ;;; default file extension for source files
     consstring(destword(Name))<>': ',    ;;; Prompt
    "reset_"<>Name,             ;;; Compiler reset procedure.
    identfn,                    ;;; Setup = do nothing.
    npr(%lowertoupper(Name)%),  ;;; Banner
    init_default(%FileExt%),    ;;; Initialisation of subsystem
    identfn,                    ;;; dont process command-line args.
    vedsetup_default(%Name,FileExt%),;;; Setup
    identfn,                         ;;; Setup X environment - do nowt.
    searchlists(Dir,Compile),
    Name %] :: sys_subsystem_table -> sys_subsystem_table;
    Name;
    parse_ved_decl([^Compile ^FileExt ;]);
enddefine;


define parse_subsystem(L) -> (x,L_out);
  lvars L,x,L_out,
    (Name,L1) = dest(L),
    (FileExt,L2) = dest(L1),
    (Dir,L3)     = dest(L2),
  ;
    compile_pr([PARSE_GEN$-new_subsystem(" ^Name ", ^FileExt, ^Dir) -> ; ],Rep_code);
   parse_subsystem_bindings(L3,Name) -> (x,L_out);
   dlocal cucharout = Rep_code;
    printf(
      '\n\n/* Subsystem: \nNAME = %p\n  COMPILER = %p\n  FILE_EXTN= %p\n  PROMPT = %p\n  RESET = %p\n  SETUP = %p\n  BANNER = %p\n  INITCOMP = %p\n  POPARG1 = %p\n  VEDSETUP = %p\n  XSETUP = %p\n  SEARCH_LISTS = %p\n  TITLE = %p\n*/\n',
      hd(sys_subsystem_table));
enddefine;

define parse_subsystem_bindings(L,Name)->(x,L_out);
    lvars L,Name,L_out,
        x = undef,
        (Field,L1) = dest(L);
    if Field = "FILE_EXTN" or Field = "NAME" then
        mishap_ptx('Cant reset this field value',[^Field])
    endif;
    if Field = ";" then return(L1 -> L_out);
    endif,
    lvars
        (Value,L2) = dest(L1),

        SS_Field = "SS_" <> Field;
    compile_pr([^Value -> subscr_subsystem(^SS_Field,"^Name");],Rep_code);
    parse_subsystem_bindings(L2,Name) -> (x,L_out);
enddefine;




define searchlists(Dir,Compile);
    lvars Dir,Compile;
    [
        vedhelpname     [[% Dir dir_>< 'help', "help",  Compile%]
                         [% Dir dir_>< 'teach', "teach", Compile%]]
        vedteachname    [[% Dir dir_>< 'teach', "teach", Compile%]
                         [% Dir dir_>< 'help',  "help",  Compile%]]
        vedrefname      [[% Dir dir_>< 'ref',  "ref", Compile%]]
        ved_??_name     [% Dir dir_><'help' %]
        vedlibname      [% Dir dir_><'auto' %]

    ],
;
enddefine;


define trycompile_default(file,Compile);
    lvars file,Compile;
    if readable(file) ->> file then
        Compile(file), true;
    else
        false;
    endif;
enddefine;


define init_default(FileExt,Compile);
    lvars FileExt,Compile;
    dlocal  vedvedname;
    trycompile_default('$popsys/init'><FileExt,Compile) -> ;
    unless trycompile_default('$poplib/init'><FileExt,Compile) then
        trycompile_default('init'><FileExt,Compile) -> ;
    endunless;
enddefine;


define vedsetup_default(Name,FileExt);
    lvars Name,FileExt;
    unless vedsetupdone then
        'temp' <> FileExt -> vedvedname;
        'output' <> FileExt
            ->> vedlmr_print_in_file -> vedlmr_errs_in_file;
        'Name' -> vedhelpname;
    endunless;
enddefine;


endsection;
