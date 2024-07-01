

compile_mode :pop11 +strict;


uses vedemacs;

section;

include xved_constants;

vars menubar_scm =

[
    ['File'
        {'Open File...' 'openfile'}
        {'Save Current' 'w1'}
        {'Save As...' 'savefileas'}
        {'Write To...' 'writefileto'}
        {'Insert File...' 'insertfile'}
        {'Quit Current' 'q'}
        {'List Current Directory' 'dired .'}
        {'Print'    'print -o ps'}
    ]
    ['Edit'
        {'Cut' 'seln cut'}
        {'Copy' 'seln copy'}
        {'Paste' 'seln paste'}
;;;        {'Undo' 'seln undo'}
        {'Clear buffer' 'clear'}
        space
        {'Search for...' 'searchfor'}
        {'Search and Replace...' 'replace'}
        {'Repeat last Search' 're_search'}
        {'Repeat last Backsearch' 're_backsearch'}
        space
        {'Justify current procedure' 'jcp'}
    ]
    ['View'
        {'Start of File' vedtopfile}
        {'End of File' vedendfile}
        {'Start of Range' vedmarkfind}
        {'End of Range' vedendrange}
        {'Start of Procedure' {vedmarkpush ved_mcp vedmarkfind vedmarkpop}}
        {'End of Procedure' {vedmarkpush ved_mcp vedendrange vedmarkpop}}
    ]
    ['Compile'
        {'Current Line' vedloadline}
        {'Current Procedure' 'lcp'}
        {'Current File' 'l1'}
        {'Range' 'lmr'}
        {'Selection' 'seln compile'}
        space
        {'File...' 'compilefile'}
    ]

    ['Lectures 1-23'
;;;      {'List of Lectures' 'ved $popscheme/course_contents.txt'}
      {'Index to Home Page' 'browse index.html'}
      {'Lecture 1'       'browse lecture1.html'}
      {'Lecture 2'       'browse lecture2.html'}
      {'Lecture 3'       'browse lecture3.html'}
      {'Lecture 4'       'browse lecture4.html'}
      {'Lecture 5'       'browse lecture5.html'}
      {'Lecture 6'       'browse lecture6.html'}
      {'Lecture 7'       'browse lecture7.html'}
      {'Lecture 8'       'browse lecture8.html'}
      {'Lecture 9'       'browse lecture9.html'}
      {'Lecture 10'       'browse lecture10.html'}
      {'Lecture 11'       'browse lecture11.html'}
      {'Lecture 11 O-O'   'browse lecture11a.html'}
      {'Lecture 12'       'browse lecture12.html'}
      {'Lecture 13'       'browse lecture13.html'}
      {'Lecture 14'       'browse lecture14.html'}
      {'Lecture 14 O-O'   'browse lecture14a.html'}
      {'Lecture 15'       'browse lecture15.html'}
      {'Lecture 16'       'browse lecture16.html'}
      {'Lecture 16 O-O'   'browse lecture16a.html'}
      {'Lecture 17'       'browse lecture17.html'}
      {'Lecture 18'       'browse lecture18.html'}
      {'Lecture 19'       'browse lecture19.html'}
      {'Lecture 20'       'browse lecture20.html'}
      {'Lecture 21'       'ved $popscheme/lecture21.lscm'}
      {'Lecture 22'       'ved $popscheme/lecture22.p'}
      {'Lecture 23'       'ved $popscheme/lecture23.lscm'}
      {'Lecture 24'       'ved $popscheme/lecture24.lscm'}
    ]


    ['287 '
      {'See your grade' 'lib show_grade.p'}
      {'About SuperScheme' 'ved $popscheme/SuperScheme.scm'}
      {'Welcome'   'ved /users/users2/cs287/cs287/welcome'}
      {'Welcome from TA'
                    'ved /users/users2/cs287/cs287/welcome_from_TA'}
      {'Syllabus'    'browse syllabus.html'}

      {'Lambda Calculus'
                       ':sysobey(\'ghostview $popscheme/lambda.ps &\',`%`)'}

      space


 {'Homework 1'         'browse F99/hwk1.html'}
{'Homework 2'         'browse F99/hwk2.txt'}
/*
{'Homework 3'         'ved /users/users2/cs287/cs287/public_html/S99/hwk3.scm'}
{'Homework 4'         'ved /users/users2/cs287/cs287/public_html/S99/hwk4.lscm'}
{'Homework 5'         'ved /users/users2/cs287/cs287/public_html/S99/hwk5.lscm'}
{'Lecture 11a as .lscm' 'ved /users/users2/cs287/cs287/public_html/lecture11a.lscm'}
{'Homework 6'         'ved /users/users2/cs287/cs287/public_html/S99/hwk6.lscm'}
{'Homework 7'         'ved /users/users2/cs287/cs287/public_html/S99/hwk7.lscm'}
 {'Homework 8'         'ved /users/users2/cs287/cs287/public_html/S99/hwk8.lscm'}
{'Homework 9'         'ved /users/users2/cs287/cs287/public_html/S99/hwk9.lscm'}
{'Homework 10'         'ved /users/users2/cs287/cs287/public_html/S99/hwk10.lscm'}
*/
      space

;;; {'Some Answers for Midterm' 'ved /users/users2/cs287/cs287/ans_midterm.scm'}
{'Sample Exam 1-S95'    'ved /users/users2/cs287/cs287/exam1s95.scm'}
{'Answer 1-S95'         'ved /users/users2/cs287/cs287/ans_exam1s95.scm'}
{'Sample Exam 1-F95'    'ved /users/users2/cs287/cs287/F95/class_test_F95.scm'}
{'Answer 1-F95'         'ved /users/users2/cs287/cs287/ans_exam1f95.scm'}
      space

      {'Scheme Examples' 'ved $popscheme/examples.scm'}
      {'Scheme Definition (last MIT)'
                    ':sysobey(\'ghostview $popscheme/Scheme_r4rs.ps &\',`%`)'}
      space
      {'Late Policy'      'ved /users/users2/cs287/cs287/late_policy'}
;;;      {'Sample Final S94' 'ved /users/users2/cs287/cs287/sample_final'}  NBG - no detail
      space
      {'Help with editing' 'ved $popscheme/help_ved_scm.txt'}
      {'Help Remote X Windows' 'ved $popscheme/help_x_remote.txt'}
      {'Scheme Errata'     'ved $popscheme/Scheme_Errata.scm'}
    ]

    ['Honors'
      {'Honors Section'    'ved $cs287/honors.txt'}
;;;      {'Honors Assignment-2' 'ved $cs287/honors_hwk2.ml'}
;;;         {'Honors Assignment-1' 'ved $cs287/honors_hwk1.ml'}
;;;      {'Honors Assignment-3' 'ved $cs287/honors_hwk3.ml'}
;;;         {'parse.ml'             'ved $popscheme/parse.ml'}
;;;      {'objectclass'       ': lib objectclass; teach objectclass'}
        {'family and cryptarithmetic'             'ved $cs287/public_html/family_relations.pl'}
;;;      {'queens.pl'             'ved $popscheme/queens.pl'}
       {'Sets signature'     'ved $cs287/Sets.sig'}
    ]

;;; [^XVMB_HELP_LABEL
;;;     {'For Current Item' vedgetsysfile}
;;;     {'For Selected Item' 'seln help'}
;;;     {'Next Cross Reference' vednexthelp}
;;;     {'Previous Cross Reference' vedprevioushelp}
;;;     space
;;;     {'Other help...' 'gethelp'}
;;;     {'About Poplog...' 'aboutpoplog'}
;;; ]

];

vars menubar_TA = [];


trycompile('$cs287/menubar_seth.p')->;

menubar_scm <> menubar_TA -> menubar_scm;


endsection;
