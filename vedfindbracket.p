/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/vedfindbracket.p
 > Purpose:         Find matching bracket in VED
 > Author:          John Williams, Oct 21 1988 (see revisions)
 > Documentation:   REF * vedfindbracket
 > Related Files:   LIB * VED_GBL, LIB * VED_GEL, LIB * VED_MP, etc.
 */

compile_mode :pop11 +strict;

section;

/* Searches for ___bra using _______movepdr to move the cursor.
	Signals an error if _______endtest is ever true.
	Matches intervening non-quoted instances of ___bra and ___ket
*/

define vedfindbracket(bra, ket, endtest, movepdr);
	lvars c, n, q, bra, ket, quotes, procedure (endtest, movepdr);
	dlocal vedpositionstack;
	vedpositionpush();

	define dlocal interrupt();
		vedpositionpop();
		veddointerrupt();
	enddefine;

	returnif(vedcurrentchar() == bra);
	if subsystem == "lisp" then
		'"|'
    elseif subsystem == "scheme"
		or subsystem == "lscheme"
		or subsystem == "ml" then
        '"'
	else
		'\'"`'
	endif -> quotes;
	false -> q;
	0 -> n;
	repeat
		if endtest() then
			vedpositionpop();
			vederror('Matching "' <> consstring(q or ket, 1) <> '" not found')
		endif;
		movepdr();
		vedcurrentchar() -> c;
		if q then
			if c == q then
				false -> q
			endif
		elseif c == bra then
			quitif(n == 0);
			n fi_+ 1 -> n
		elseif c == ket then
			n fi_- 1 -> n
		elseif strmember(c, quotes) then
			c -> q
		endif
	endrepeat
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jun 30 1993
		Added ` to set of Pop-11 quote characters (cf. BR robertg.2)
--- John Gibson, Jan 13 1993
		popcom*piler -> subsystem
 */
