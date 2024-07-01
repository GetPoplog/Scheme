


;;; example.p                               R.J.Popplestone January 1990

;;; The macro example provides for self-checking of programs. It reads lines
;;; of text until endexample is encountered. Any line not beginning with '**'
;;; is compiled, and any output is recorded in a string, ss_out. Any line
;;; beginning with '**' is read into a string, which is compared with ss_out.
;;; If there is a difference, a warning message is printed out.


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

section examples => example /*sss,sss_out*/, endexample;
vars n_call, ss_out,i_out, procedure(out_to_string);
vars example_off = false;

;;; example <f> <examples> endexample runs the examples and checks output OK

;;; Disabling the example macro.
/*
define macro example;
  until readitem() = "endexample" do
  enduntil
enddefine;
*/

lvars n_ss_out = 200;

define macro example;                    ;;; Full explanation at TOP of FILE
    lvars f_name = readstringline();     ;;; Read function name (ignored)
    lvars linenum = poplinenum;
    printf('example %p\n', [%f_name%]);
    dlocal cucharout,
        popgctrace     = false,         ;;; Turn off tracing of garbage coll.
        tracing        = false,         ;;; Turn off tracing.
        n_call = callstacklength(1);    ;;; Used for exit at endexample
    dlocal pr = syspr;
    dlocal prautoloadwarn = erase;

;;; lvars i_out;
    dlocal ss_out = inits(n_ss_out);
    for i_out from 1 to n_ss_out do ` ` -> subscrs(i_out,ss_out)
    endfor;
    define lvars prmishap_example(Msg,Culprits); lvars Msg,Culprits;
        dlocal cucharout = cucharerr;
        linenum->poplinenum;
        printf('Error in example %p in file = %p\n',
            [%f_name,pdprops(cucharin)%]);
        sysprmishap(Msg,Culprits);
    enddefine;
    dlocal prmishap = prmishap_example;

    repeat lvars ss = readstringline();  ;;; Read the next line of text
    ;;;    ss -> sss; ss_out -> sss_out;
        poplinenum -> linenum;
        if example_off then
            if issubstring('endexample',ss)
            then return
            endif;
        elseif ss.datalength>=2            ;;; Begins with ** ?
        and ss(1) = `*`
        and ss(2) = `*` then                ;;; If so, compare with previous
            unless issubstring(ss,ss_out)=1 ;;; output, they should be
            and i_out <= datalength(ss)+3   ;;; identical substrings.
            then                            ;;; Not identical, so print
                charout -> cucharout;       ;;; error message
                pr('\nexample failed for ');
                pr(f_name);
                pr('.\n    Expected ');
                pr(ss);
                pr('\n    Produced ');
                pr(ss_out); nl(1); ;;;.setpop;
            endunless;
        else                                ;;; Line did not begin with **
            dlocal                          ;;; switch standard output to
                cucharout = out_to_string(); ;;; the string ss_out
            lvars Rep = stringin(ss);
            pdprops(cucharin) -> pdprops(Rep);
            pop11_compile(Rep);          ;;; and compile the example line.
        endif;
    endrepeat
enddefine;


define out_to_string;   ;;; is a (not very subtle) consumer.
  1->i_out;
  procedure(c);
  c -> ss_out(i_out); i_out+1 -> i_out;
  endprocedure;
enddefine;

define macro endexample;
  '' -> ss_out;
  charout -> cucharout;
;;;  npr('example macro changed to check for store corruption');
;;;  true -> popgctrace;  sysgarbage();
  exitfrom(n_call);
enddefine;


example +
1+4=>
** 5
endexample
/* Commented out since Prolog may not be loaded.
example is
:- X is 4+5, print('** '), print(X).
** 9
endexample
*/

/*
example -
;;; This shows a failure.
1-8 =>
** 9
endexample

example mishap
  hd(45);

endexample
*/
endsection;
