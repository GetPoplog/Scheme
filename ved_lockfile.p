

/* --- University of Birmingham 1992. . ------
 > File:            $poplocal/local/lib/ved_lockfile.p
 > Purpose:         Lock and unlock ved files
 > Author:          Aaron Sloman, May  9 1992
 > Documentation: HELP * VED_LOCKFILE
 > Related Files:
Hacked by robin popplestone. 31mar97 to put up dialogue box etc.
 */

section;


;;; Warning, to prevent files read using pved from being locked,
;;; redefine pved and ved_pved to ensure that vedwriteable is made
;;; false early enough. The complex definition is given to ensure
;;; that vedsearchlist is used.

vars pvedtemp;

define ved_pved;
    if vedediting and vedargument = nullstring then
        false -> vedwriteable
    else
        define vedsysfileerror(type,name);
            ;;; catch the case where new file is to be created
            lvars type, name;
            vededitor(vedhelpdefaults, name)
        enddefine;

        ;;; give this a search list able to cope with relative and
        ;;; absolute path names, as well as with vedsearchlist
        vedsysfile("pvedtemp",['.' ^^vedsearchlist '/'],false)
    endif
enddefine;


;;; Check that file was locked in this process
global constant procedure ved_locked
    = newproperty([], 64, false, "tmparg");

global constant
    ved_lock_extn = '.VLCK',
;

;;; The user's login directory name will be assigned to this
lvars userhome = false;

;;; Make ved_lock_files false locally or globally to turn off locking
;;; Make it "mine" to ensure only locking of files in your own directory
global vars ved_lock_files = true;


define setup_WarnBox(FileName);

    lvars AW_edit =  XtAppCreateShell('Warning FILE LOCKED', 'OK',
                            xtApplicationShellWidget,
                            XptDefaultDisplay,
                            [{allowShellResize ^true}]);
    lvars Propbox_gdb =
    propsheet_new_box('Warning FILE LOCKED', AW_edit, false, []);

    lvars Propsheet_gdb = propsheet_new('You wont be able to write the file: ' <> FileName,
                                              Propbox_gdb, false);

    propsheet_field(Propsheet_gdb, [['OK' false ]]);
    propsheet_field(Propsheet_gdb, [['Help' false ]]);

    define do_help(ps,button,val);
        XptDeferApply(edit_GDB(%'$popscheme/help_lock'%));
        false;
    enddefine;

    do_help -> propsheet_field_accepter(Propsheet_gdb, "Help");


    define do_kill(ps,button,val);
        propsheet_destroy(ps);
        false;
    enddefine;

    do_kill -> propsheet_field_accepter(Propsheet_gdb, 'OK');


    propsheet_show([% Propsheet_gdb, Propbox_gdb %]);

enddefine;

define ved_lockfile();
    ;;; Lock the current file if necessary.
    ;;; Warn user if it is already locked

    unless userhome then sysfileok('$HOME') -> userhome endunless;

    if vedwriteable and ved_lock_files then
        lvars lockfile = vedpathname <> ved_lock_extn;

        if isstartstring('/tmp/', lockfile) then
            ;;; do not lock
        elseif ved_locked(vedpathname) or sys_file_exists(lockfile)
        then
            if popunderx then
                setup_WarnBox(vedcurrent);
            else
                vedputmessage('WARNING FILE LOCKED - NOT WRITEABLE');
                setpop();
                ;;;'$popscheme/explain_locked_file' -> vedpathname;
            endif;

            false -> vedwriteable;
            ;;;            setpop();
            ;;;            'setting wiggle count'=>
            ;;;            dlocal vedwiggletimes=10;
            ;;;            'doing wiggles' =>
            ;;;            vedwiggle(0,min(vedscreencolumn,65));
            ;;;            'wiggling done' =>

        elseif ved_lock_files == "mine"
        and userhome and not(issubstring(userhome, vedpathname))
        then
            ;;; best not to lock!
        else
            sysobey('touch ' sys_>< lockfile, `!`);
            lockfile -> ved_locked(vedpathname);
        endif;
    endif;
    ;;; 'leaving ved_lockfile' =>
enddefine;

define ved_unlockfile();
    if ved_lock_files and ved_locked(vedpathname) then
        false -> ved_locked(vedpathname);

        dlocal pop_file_versions = false, vedversions = false;
        ;;; delete lock
        if sysdelete( vedpathname <> ved_lock_extn ) then
            vedputmessage('DELETED LOCK FILE');
        else
            vedputmessage('COULD NOT DELETE LOCK FILE');
        endif
    else
        ;;; For debugging. Will be removed.
        if vedwriteable then vedputmessage('FILE NOT LOCKED') endif;
    endif
enddefine;

lconstant popexit_old = popexit;


define vedlockfileexit();
    'Exiting VED: Checking lock files' =>
    vedappfiles(
        procedure(); vedpathname => ved_unlockfile() endprocedure);
    appproperty(ved_locked,
        procedure(name, val); lvars name, val;
            'WARNING ' <> name <> ' STILL LOCKED' =>
        endprocedure)
enddefine;

define popexit();
   vedlockfileexit();
enddefine;

vedinitialise <> ved_lockfile -> vedinitialise;

vedvedquitfile <> ved_unlockfile -> vedvedquitfile;

;;; Now make sure that files are unlocked by ved_rqq, ved_xx, etc.
;;; and by ved_name.

define ved_rqq();
    vedlockfileexit();
    [] -> vedbufferlist;
    sysexit();
enddefine;

lconstant old_ved_name = ved_name;

define ved_name;
    ;;; unlock file before renaming
    ved_unlockfile();
    old_ved_name();
enddefine;

endsection;
