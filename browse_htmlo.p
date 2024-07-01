

vars file_source, do_monitor, directory_html, name_now;
vars procedure href_line = newassoc([]);
vars procedure line_name = newassoc([]);
vars procedure img_line  = newassoc([]);
vars procedure(
    Browse_at_loc,
    file_ok,
    follow_href,
    read_anchors,
    read_file_name,
    read_hrefs,
    read_images,
    read_names,
    show_img,
);


/*

*/

define notify_user(msg);
       vedputmessage(msg);
       if popusername = 'pop' and do_monitor then  syssleep(200);
       endif;
enddefine;

include xved_constants;

define vedmouse__ved__follow_anchor();
    let V = vvedmousedata,
        r =  V(XVM_ROW) + vedlineoffset-1,
        c =  V(XVM_COL),
        a = href_line(r),
       img = img_line(r),
    in
        vedjumpto(r,max(1,c-1));
        if a then
            notify_user(sprintf('mouse row %p, anchor %p',[%r,a%]));
            true -> xvedeventhandled;
            follow_href(a);
        elseif img then
            notify_user(sprintf('mouse row %p, anchor %p',[%r,a%]));
            true -> xvedeventhandled;
            show_img(img);
        endif;
    endlet
enddefine;

define show_img(img);
    let name_full = directory_html<>img
    in
        if file_ok(name_full,'Image file') then
            sysobey('xv ' <> name_full<>' &');
        endif;
    endlet
enddefine;


vedset mouse (override, at front )
    ved__follow_anchor = release btn1
endvedset

define is_destructive(p);
    not(member(p,
        [%vedscreenleft,vedcharleft,vedtextright,vedcharright,
          vedenter,vedrefresh,vedchardown,vedlineabove,vedcharup,
          vednextscreen,ved_lcp%]));
enddefine;

define no_insert(p);
    if isprocedure(p) and is_destructive(p) then
       vedscreenbell else p
    endif;
enddefine;

define vedinitialise(item);
   if issubstring('.htmlo',vedcurrent)
   then
       unless popusername = 'pop' then
         true -> is_vedfile_local(ident vednormaltable,ved_current_file);
         mapdata(vednormaltable,no_insert) -> vednormaltable;
       endunless;
       true -> is_vedfile_local(ident href_line,ved_current_file);
       true -> is_vedfile_local(ident line_name,ved_current_file);
       true -> is_vedfile_local(ident file_source,ved_current_file);
       read_anchors();
       if name_now/='' then follow_href('#'<>name_now); endif;
   endif;
enddefine;

define parse_href(string);
    let
        n = length(string),
        k = issubstring('#',1,string)
    in
        if k then substring(1,k-1,string),substring(k+1,n-k,string)
        else string,''
        endif;
    endlet;
enddefine;

/*
parse_href('lecture1.html#lambda') =>
** 'lecture1.html' 'lambda'
parse_href('lecture1.html') =>
** 'lecture1.html' ''
*/

/*
So, string is a "URL" which we have to parse (1) into a file and a name within the
file.

*/


define follow_href(string);
    notify_user('follow href ' <> string);
    let (file,name) = parse_href(string) in              ;;; (1)
        if file = '' or file = file_source then
            let l = line_name(consword(name))
            in
                if isnumber(l) then
                    notify_user('going to line: '><l);
                    vedjumpto(l,1); vedchardownlots(); vedcharuplots();
                else  notify_user('undef HREF '><string);
                endif;
            endlet

        else
               Browse_at_loc(file,name);
        endif
    endlet;
enddefine;

/*
follow_href('browse_htmlo.p#fred');
follow_href('browse_htmlo.p#oop');
follow_href('lecture1.html#oop');
*/



define read_anchors();
    vedpositionpush();
    if vedteststartsearch('++++++++') ;;;and vedteststartsearch('HREF')
    then
       vednextline();
       read_file_name() -> file_source;
       read_hrefs()->href_line;
       read_names()->line_name;
       read_images() ->img_line;
       vedpositionpop();
    else notify_user('cant find start of anchor information');
    endif;
enddefine;


define read_file_name();
  let word = vedmoveitem()
  in
      if word = "SOURCE" then vedmoveitem();
      else notify_user('SOURCE missing');
      endif;
  endlet;
enddefine;

;;; Tidy this up - Prop_0 is not needed


define read_ref(HDR,Prop_0);
;;;    []->report;
    dlocal pop_pr_quotes = false;
    notify_user('reading ' >< HDR >< ' info');
    let name,k,Prop=newassoc([]),
    in
        vedmoveitem() -> name;
           notify_user('Header found = ' >< name);
;;;        name::report->report;
        if  name= HDR then
            repeat

                vedmoveitem() -> name;
;;;                name::report->report;

                if name=termin then
                    notify_user('File corrupted '<> vedcurrent);
                    return(false);
                endif;

                if isword(name) then
                    vedtextleft();
                    return(Prop);
                endif;

                unless vedmoveitem() = "=" then
                    notify_user('missing "="');
                endunless;

                vedmoveitem() -> k;
   ;;;             k::report->report;
                name -> Prop(k);

            endrepeat
        else
            notify_user(HDR sys_>< ' missing in ' sys_>< vedcurrent);
        endif;
        datalist(Prop)=>
        Prop;
    endlet;
enddefine;

define read_hrefs = read_ref(%"HREF",href_line%)
enddefine;

define read_images = read_ref(%"IMG",img_line%)
enddefine;


define read_names();
    let name,k,line_name=newassoc([]),
    in
        vedmoveitem() -> name;
        if  name="NAME" then
            repeat
                vedmoveitem() -> name;

                if name=termin then
                    notify_user('File corrupted '<> vedcurrent);
                    return(false);
                endif;

                if isword(name) then
                    vedtextleft();
                    return(line_name);
                endif;

                unless vedmoveitem() = "=" then
                    notify_user('missing "="');
                endunless;


                vedmoveitem() -> k;

                k -> line_name(consword(name));
            endrepeat
        else
            notify_user('NAME missing in '<>vedcurrent);
        endif;
        line_name;
    endlet;
enddefine;

;;; 11,22,33,read_anchors() =>

/*
++++++++++++++++++++++ANCHOR+++INFO+++++++++++++++++++++++++++++++++++
HREF
'#oop' = 21
'#scm' = 24
'#fp' = 12
'#ip' = 15
'#lp' = 18
NAME
'scm' = 185
'fp' = 63
'lp' = 125
'ip' = 96
'oop' = 169
END

read_anchors();
*/

vars browse_htmlo = true;
