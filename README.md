    $poplocal/local/Scheme/README
    Originally by Robin Popplestone
    Dept. of Computer and Information Science
    Lederle Graduate Research Center
    University of Massachusetts
    Amherst, MA  01003
    USA
    Email: Robin Popplestone <pop@cs.umass.edu>

-----------------------------------------------------------------------

## Additional notes by Stephen Leach

This is a version of Scheme for Poplog, originally developed by Robin
Popplestone at the University of Massachusetts to support a course on
programming paradigms. It was archived by Aaron Sloman as part of his 
extensive archive hosted by the University of Birmingham. This has 
been transferred to GitHub with Aaron's permission to be a long term home.

Stephen Leach, 01 June 2024


## NOTES by Aaron Sloman

I unpacked Robin's tar file which had absolute pathnames making it hard
to untar without superuser privileges. I then repackaged it so that it
could be untarred in the $poplocal/local/ directory, creating the
directory

    $poplocal/local/Scheme

I modified the file Scheme.p which builds the Scheme system on top of
pop-11, so that it works even if the environment variable $popscheme has
not been set.

I deleted the two .psv files which are probably of no use outside umass.

I modified Robin's "make" files, producing make_scheme, which, if run
creates

    $poplocalbin/scheme.psv

so that scheme can be run with the command:

    pop11 +scheme

If you wish to test the new scheme, you can then run this:

    pop11 +scheme  $poplocal/local/Scheme/examples.scm


WARNING:
Scheme built thus modifies Ved to suit Emacs users. I may later produce
another version which leaves Ved unchanged. The user's vedinit.p file
can then specify required changes.

Aaron
6 Oct 1999


## ORIGINAL TEXT BY ROBIN POPPLESTONE

This directory contains the source of UMASS Scheme together with  .psv
files for decstations and alphas. Suitable make scripts are provided.
Note that the make process checks out the .psv file by running a
lot of scheme examples past it.

In UMASS we provide a .cshrc script which starts up POP-11 with
the appropriate .psv file depending on which machine the user
is logged in on.

This implementation of Scheme is tailored to the UMASS course on
"Programming Paradigms", so it does leave out certain Scheme
features not required for that class, and adds certain features
(notable opaque records using the record-class facility of POP-11,
and the use of updaters) which are not standard Scheme.

It is intended for use under X-windows. The file menubar_scm.p
specifies how each window appears.

Documentation is written in HTML, and is available to Scheme using
a browser that I cobbled up.

Robin.
