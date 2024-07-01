
;;; ved_source.p                  A.Sloman and R.Popplestone Fall and Autumn 87

/*
NOV 94 Robin Popplestone modified "Redefining procedure.." not to
print anything if show_libraries was loaded. Instead, redefined procs.
will be listed by show_libraries.

OCT94 Robin Popplestone arranged for a list of -all- sources of a
procedure-name to be kept.

MAR 94 Robin Popplestone added redefinition of  sysGLOBAL to provide
some support for lib objectclass

OCT91 R.Popplestone changed "Redefining procedure.." message to "Redefining.."
to reflect what actually happens (and to make it shorter).

??? Problems - the ** in example refers to the exponentiation operator

?? Quote the function names when explaining bindings.

?? How about variables bound to constants.

?? How about recognising that you are in a comment, including the open
and close comment brackets.

(a) I have replaced the call to get the one-line explanation of system
identifiers with the call to get a few lines of text (i.e. ved_?? rather
than ved_?). This is more helpful to POPLOG beginners.

(b) I have put in a special test for  (%, since they are not beginners at
Computer Science, but closures are still(!) funny beasts to them.

(c) I have changed the "UNDECLARED IDENTIFIER" error message to "Identifier
not declared globally" since you get it with local error messages. I
contemplate a hack redefining sysVARS and sysLVARS and ved_lcp to
spot the local identifiers, but life is short...

*/
vars procedure source_of_proc;
uses npr;

vars rep_warning;
unless isprocedure(rep_warning) then charout -> rep_warning endunless;

define global vars ved_procslike();
    ;;; '<ENTER> procslike <string>' gets procnames containing <string>
    lconstant tempfile=systmpfile( false, 'procslike', '' );
    dlocal cucharout = vedcharinsert, vedbreak=false;
    vededitor(vedhelpdefaults,tempfile);
    vedendfile();
    appproperty(source_of_proc,
        procedure(name,file);
            lvars name,file;
            if issubstring(vedargument,1,name) then
                pr(name);
                max(vedcolumn + 2, 30) -> vedcolumn;
                pr(if isref(file) then cont(file)
                    elseif file then file
                    else '(unknown)' endif);
                pr(newline);
            endif
        endprocedure)
enddefine;

section $-source rep_warning npr=>
        source, ved_source, ved_source_table_size, source_of_proc;

lvars rep_pr_warning;

define pr_current_file();
    if rep_pr_warning/==cucharin then
        printf('Current file: %p\n', [%pdprops(cucharin)%]);
        cucharin -> rep_pr_warning;
    endif;
enddefine;

;;; Allow user to pre-define size of table or source_of_proc

global vars ved_source_table_size;
unless isinteger( ved_source_table_size) then
    126 -> ved_source_table_size
endunless;

global vars procedure source_of_proc;

if isundef(source_of_proc) then
    newproperty([],ved_source_table_size,undef,false) -> source_of_proc
endif;


define constant property_print(f);
  pr('\nThis is a property (see *HELP PROPERTIES)\n Entries:\n');
  appproperty(f, procedure(k,v); k.pr; pr('  =  '); v.pr; pr('\n    ');
     endprocedure)
enddefine;


define constant array_print(f);
  pr('\nThis is an array (see *HELP ARRAYS)\n boundslist:\n');
  pr(f.boundslist);
enddefine;

define constant closure_print(f);
    ;;; print information about closures.
    ;;; Altered [A.S] to print in ved buffer if vedlmr_print_in_file is string
    lvars i,f,n=f.length;
    dlocal vedargument cucharout;
    if isstring(vedlmr_print_in_file) then
        vedselect(vedlmr_print_in_file);
        vedendfile();
        vedcharinsert -> cucharout
    endif;
    f.pdpart.pdprops.recursive_front.pr; '(%'.pr;
    for i to n do
        pr(frozval(i,f));
        if i < n then pr(",") endif;
    endfor;
    pr('%)');
enddefine;

define explain_constant(c);
    pr('This is an object of type "' >< dataword(c) >< """)
enddefine;

define is_in_comment() -> bool;     ;;; Special check reqd. since comments
                                    ;;; are not reported by the itemiser.
  lvars c;
  dlocal vedline vedcolumn;
  while (vedcurrentchar() ->> c) = ` ` or c= `\^I` do
          vedrepeater() ->;
  endwhile;
  lvars c1 = vedrepeater(),
    c2 = vedrepeater(),
    c3 = vedrepeater();
  if c1 = `;` and c2 = `;` and c3 = `;` then
    pr(';;; makes rest of line into comment');
  elseif c1 = `/` and c2 = `*` then
    pr('/* is start of comment - text ignored until */');
  elseif c1 = `*` and c2 = `/` then
    pr('*/ is end of comment begun by /*');
  elseif c1 = `*` and c2 = `*` then
    pr(
    '** is output by the print-arrow, => as well as exponentiation');
    false -> bool
  else false -> bool
  endif;

enddefine;

/*
ved_source moves the cursor to the source code of a procedure
--------------------------------------------------------------
*/


define global vars procedure ved_source;
    ;;; Argument is name of a procedure. Get source file into VED
    lvars  file, name, oldarg, im_file = false, p;
    ;;; If no argument supplied, use item to right of cursor
    if vedargument = vednullstring then
        if is_in_comment() then return
        endif;
        vednextitem() -> name;
        name sys_>< vednullstring -> vedargument;
    else
        lvars rep_item = incharitem(stringin(vedargument));
        rep_item() -> name;
        consstring(destword(name)) -> vedargument;
    endif;
    unless isword(name) then
        explain_constant(name);
        return
    endunless;

    name.source_of_proc -> file;

    if isref(file) then
        ;;; Was compiled in "immediate mode".
        ;;; ved_f will be screwed by prompts, so set im_file
        true -> im_file;
        cont(file) -> file
    endif;

    if islist(file) then
       vedputmessage('Method or multiple definition in ' >< file);
       hd(file)->file;
    endif;
    if isstring(file) then
        vedargument -> oldarg;
        ;;; With luck, vvedgotoplace can ensure cursor starts in right place
        space >< oldarg -> vvedgotoplace;
        file -> vedargument;
        if vedediting then ved_ved()
        else
            ;;; prepare to run ved_source inside vededitor
            vedinput(procedure; oldarg -> vedargument; ved_source()
                    endprocedure);
            chain(ved_ved);
        endif;
        ;;; only gets here if vedediting is true
        oldarg -> vedargument;
        if im_file then
            ;;; already at possible location, because of vvedgotoplace
            ;;; ved_f will not work.
        else
            ;;; use '-x' for exact match
            '-x ' sys_>< vedargument -> vedargument;
            ved_f();         ;;; find the procedure

            vedcharuplots(); vedchardown();
        endif
    elseif file.not then
        ;;; In table, but source file not available
        vederror(
            vedargument sys_>< ' defined in POPVAL or direct from keyboard')

    elseif identprops(name) == undef then
        pr('IDENTIFIER not declared globally: ' sys_>< name)

    elseif isproperty(valof(name)) then
        property_print(valof(name))      ;;; not sure this is right

    elseif isarray(valof(name)) then
        array_print(valof(name))      ;;; not sure this is right

    elseif isclosure(valof(name)) then
        closure_print(valof(name))      ;;; not sure this is right

    elseif name = "("
    and (vedmoveitem()->>; vednextitem() = "%") then
         ved_help('partapply'->vedargument)

    elseif isprocedure(valof(name))
    and isword(pdprops(valof(name))->>p)
    and name /= pdprops(valof(name)) then
    pr('\nFunction variable "'><name><'" bound to "'>< p >< """);
    ved_source(p><'' -> vedargument);
    else
    lvars n = identprops(name),
          msg = if isword(n) then n
                elseif n==0 then "general"
                else 'operator precedence ' >< n
                endif;

    vedputmessage('\n"'
       >< name
       >< '"  is identifier of type :  '
       >< msg);

      ved_??()

    endif
enddefine;


;;; A version for use outside VED. Should be a syntax word and plant code.
define global syntax source;
    sysPUSHQ(readstringline());
    sysPOP("vedargument");
    sysCALL("ved_source");
    ";" :: proglist -> proglist;
enddefine;


;;; Now redefine sysPASSIGN after saving original
sysunprotect("sysPASSIGN");

;;; Next bit guards against re-compilation
constant oldPASSIGN;
unless isprocedure(oldPASSIGN) then
    sysPASSIGN -> oldPASSIGN
endunless;


define lvars record_for_source(word);
    ;;; Store association between word and the source file using source_of_proc
    lvars proc,word,arg1,frozargs,filename, do_print_file = false;
    dlocal cucharout = rep_warning;
    ;;; Find name of file or VED buffer from which compiling

    lvars src = source_of_proc(word);
    if   isword(word)
    and  identprops(word) /= undef
    and  not(isundef(valof(word)))
    and identprops("show_libraries")==undef
    and  not(word=="_")
    then lvars file = source_of_proc(word);
        /*
        if isstring(file) then
            printf('Redefining: %p, first defined in %p\n ',
                [%word,source_of_proc(word)%]);
        else printf('Redefining : %p, not defined in this program\n',
                [%word%])
        endif;
        true -> do_print_file;
        */
    endif;

    unless  isincharitem(itemread) then
        ;;; No character repeater, so can't be a file. Might be popval(list)
        false

    elseif popfilename then
        sysfileok(popfilename) -> filename;
        ;;; add directory if necessary
        if sysfiledir(filename) = vednullstring then
            current_directory dir_>< filename
            else
            filename
            endif

    elseif vedediting and cucharin == charin and iscaller(vedsetpop) then
        ;;; Immediate mode, so put file name in reference, as cue
        consref(vedpathname)

    elseunless isclosure(cucharin) then
        false   ;;; Can't identify file name. Perhaps typed in direct

    else
        ;;; Compiling from result of discin, or ved buffer perhaps

        length(cucharin) -> frozargs;
        frozval(1,cucharin) -> arg1;
        if frozargs == 1 and isdevice(arg1) then
            device_open_name(arg1)

            ;;; Next bit recognises compilation from VED buffer, etc. UGH!!
        elseif frozargs == 3
        and    ispair(arg1)
        and    isref(frozval(2,cucharin))
        and    isprocedure(frozval(3,cucharin))
        then
            vedpathname     ;;; should be compiling from VED buffer...?
        else
            false           ;;; could be any repeater
        endif
    endunless   -> file;
    if src == undef then file
    elseif islist(src) then file::src
    else [%file,src%]
    endif -> source_of_proc(word);

    if do_print_file then
        pr_current_file();
    endif;
enddefine;

define vars procedure sysPASSIGN(proc,word);
    record_for_source(word);
    chain(proc,word,oldPASSIGN);
enddefine;


sysunprotect("sysGLOBAL");
constant oldGLOBAL;
unless isprocedure(oldGLOBAL) then
    sysGLOBAL -> oldGLOBAL
endunless;

define vars procedure sysGLOBAL(word);
  record_for_source(word);
  chain(word,oldGLOBAL);
enddefine;


define prwarning(v);
  lvars v, r = cucharin;
;;;  dlocal cucharout = rep_warning;
;;;  vedputcommand('ved warning.tmp');
  if isclosure(r) then frozval(1,r) -> r
  endif;

  pr('DECLARING VAR '); npr(v);
  pr_current_file();

enddefine;


sysprotect("sysPASSIGN");

endsection;
