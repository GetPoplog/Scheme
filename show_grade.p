

npr('preparing your grade');

lconstant gradefile = '/tmp/grade_' <> popusername;
lconstant gradefile1 = '/tmp/grade1_' <> popusername;
lconstant hdrfile1 = '/tmp/header' <> popusername;

lconstant cmd0 =
    'crypt Tasefuar < $cs287/students_F99.cry | grep ' <> 'LOGIN'
    <> ' > ' <>  hdrfile1;

sysobey(cmd0, `%`);

lconstant cmd =
    'crypt Tasefuar < $cs287/students_F99.cry | grep ' <> popusername
    <> ' > ' <>  gradefile;

sysobey(cmd, `%`);
lconstant cmd_app =
'cat '<> hdrfile1 <> ' ' <> gradefile <> ' >' <> gradefile1;

sysobey(cmd_app, `%`);


vededit(gradefile1);

npr('done');
