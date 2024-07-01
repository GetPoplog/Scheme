

vars directory_html = '$choo/public_html/';


uses browse_htmlo;

vars procedure read_html;
vars file_now = '';
vars name_now = '';

#_IF pop_internal_version<150000
define vededit(file); vededitor(identfn,file) enddefine;
#_ENDIF

define Browse_at_loc(file,name);
    file -> file_now;
    name -> name_now;
    let
        name_in  = directory_html sys_>< file,
        name_choo = choo_html sys_>< file,
        name_out = '/tmp/287_' sys_>< file sys_><"." sys_>< popusername sys_>< '.htmlo',
    in
;;;        if file_ok(name_choo, 'The requested URL') then name_choo->name_in
;;;        endif;                                            ;;; what a hack!!!
        if file_ok(name_in, 'The requested URL') then
            if issubstring('.html',file) then
                unless (sys_file_exists(name_out)) and
                    sysmodtime(name_out) > sysmodtime(name_in)
                then
                    notify_user('making new .htmlo file');
                    uses browser;
                    read_html(discin(name_in),name_out);
                endunless;
            else name_in -> name_out
            endif;
            vededit(name_out);
        endif
    endlet
enddefine;

define Browse(file);
   Browse_at_loc(file,'');
enddefine;

lconstant msg_not_found = '" was not found on this server.' ;

define file_ok(file,msg);
    dlocal pop_pr_quotes = false;
    let
        status = {0 0 0 0 0 0 0 0 0 0},
    in
        unless sys_file_exists(file) then
            notify_user(msg ><' "' >< file >< msg_not_found);
            return(false);
        endunless;

        sys_file_stat(file,status) -> status;
        unless status(5)&&4==4 then
            notify_user(msg >< ' "' >< file >< '" - access denied.');
            return(false);
        endunless;
    endlet;
    true;
enddefine;
