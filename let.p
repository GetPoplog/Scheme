/*
let.p                      Robin Popplestone MAY 1996


The following macro definition provides a "let" statement.

let
    jane = 7,
    joy = 6
in
    jane+joy
endlet

which would seem reasonably natural to users of Scheme and ML,
controls the scope of the declared variables, and avoids the
perennial pitfall of "lvars" namely that you can easily write:

   lvars jane = 4; joy = 34;

when you mean

   lvars jane = 4, joy = 34;

(the kind of feature regarded as a virtue in C...).


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

====================================================================
*/

;;; let.p                   Robin Popplestone, spring 1995

define lconstant check_no_semicolon();
    lvars l,L = proglist, d=0;
    repeat; hd(L) -> l;
        if member(l, vedopeners) then d+1->d
        endif;
        if l == ";" and d == 0  then
            mishap('";" encountered in let binding', [^proglist]);
        endif;
        if member(l,vedclosers) then d-1->d;
        endif;
        if l = "in" and d==0 then ";" -> hd(L); quitloop;
        endif;
/*
        if d==0                           ;;; e.g. let + = Add in ....
        and isword(l)
        then lvars n = identprops(l);
            if  isnumber(n) and n>0
            and hd(tl(L)) == "="
            then
                identprops(l) -> hd(L);
                l::tl(L) -> tl(L);
                tl(L) -> L;
            endif
        endif;
*/
        tl(L) -> L;

    endrepeat;
enddefine;

define macro let;
  check_no_semicolon();
  "lblock", "lvars"
enddefine;

define macro endlet;
  "endlblock"
enddefine;

"let"::vedopeners -> vedopeners;
"endlet"::vedclosers -> vedclosers;
"in"::vedbackers->vedbackers;


/*

define test(x);
    let y = 1+x, z=3
        in y+z
    endlet
enddefine;

test()=>
*/
