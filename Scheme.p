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

 * Modified by Aaron Sloman, to take account of undefined $popscheme
 * 5 Oct 1999

*/

*/

;;;load ~pop/init.p

global vars popschemedir = sys_fname_path(popfilename);

lvars root_dir = systranslate('popscheme');

unless root_dir then
	popschemedir -> systranslate('popscheme')
endunless;

extend_searchlist(sysfileok('$popscheme'), popuseslist) -> popuseslist;
extend_searchlist(sysfileok('$popscheme'), popautolist) -> popautolist;


uses show_libraries;
lvars (script_move,exclude,incl) = explode(show_libraries(false));

exclude(['$usepop' ]);



uses compile_Scheme;

;;; vars Scheme = true;
