

;;; lexical_args.p                             Robin Popplestone Jan92

;;; Makes all arguments be lexical by default rather than dynamic, provided
;;; the +varsch switch is used in compile_mode.


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

define pop11_vars_default_check(v);
  lvars v;
  sysLVARS
enddefine;

vars lexical_args = true;



/* For example:

uses showcode;
true -> pop_show_code;

compile_mode:pop11 +defpdr +varsch +constr;
define fred(x,y);
  dlocal y;
enddefine;

Code produced is:

    PROCEDURE fred 2
    LOCAL   y
    LVARS   x 0
    POP     y
    POP     x
return
    ENDPROCEDURE
    PASSIGN <procedure fred> fred
Redefining : fred
    EXECUTE
    EXECUTE


*/
