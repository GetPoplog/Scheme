/*

Scheme.p                             Robin Popplestone, May 1995

This file builds the Poplog Scheme environment.

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

;;;load ~pop/init.p

extend_searchlist('$popscheme', popuseslist) -> popuseslist;
extend_searchlist('$popscheme', popautolist) -> popautolist;


uses show_libraries;
lvars (script_move,exclude,incl) = explode(show_libraries(false));

exclude(['$usepop' ]);



uses compile_Scheme;

;;; vars Scheme = true;
