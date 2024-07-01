
/*
Motif.p                                    Robin Popplestone MAY 1996

This file serves to provide capabilities for a limited creation of Motif
widgets in the POPLOG Scheme environment. It is not required for any
other purpose in Scheme.


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

*/

exload_batch;

;;; Load X-related libraries
uses popxlib;
uses xt_widget;
uses xt_callback;
uses xt_event;

include xpt_coretypes;

;;; Access widgetclasses
uses
    xtApplicationShellWidget,
    xmRowColumnWidget,
    xmPushButtonWidget,
    xmScaleWidget;

endexload_batch;

include XmConstants;

;;; Initialise Poplog X Toolkit and connect to server

XptDefaultSetup();

define make_ShellWidget(Name);
    XtAppCreateShell(Name, Name, xtApplicationShellWidget,
                        XptDefaultDisplay,
                        [{allowShellResize ^true}]);
enddefine;



uses lexical_args;
true -> popradians;

compile_mode:pop11 +defpdr +varsch +constr;

define mk_Callback(Proc);
  procedure(Widget,D_client,D_call);
     Proc(Widget,1)->;
  endprocedure;
enddefine;


define TypeSpec(Val);
   if isstring(Val) then TYPESPEC(:XmString)
   elseif isboolean(Val) then TYPESPEC(:XptBoolean)
   else "int"
   endif;
enddefine;

vars Unix = "Unix";

vars bad_method_name = 'bad method name';
vars Msg_bad_method = 'Bad method name "%p%" sent to object of type "%p"';

define send(Msg);
    let Obj = hd(Msg), Args = tl(Msg) in
        if isXptDescriptor(Obj) then
            let Cmd = hd(Args), Qual = tl(Args) in
                if Cmd = 'on' then
                    XtAddCallback(Obj,
                             popval([XmN %hd(Qual)%]),
                             mk_Callback(hd(tl(Qual))),'');
                             undef_scm;
                elseif last(Cmd) == `!` then           ;;; e.g. (send w "width!" 4)
                    let Val = hd(Qual),
                        Ty  = TypeSpec(Val)
                    in
                        Val -> XptValue(Obj,allbutlast(1,Cmd),Ty);
                    undef_scm;
                    endlet;
                else XptValue(Obj,Cmd)
                endif
            endlet
        elseif Obj = Unix then sysobey(hd(Args)); undef_scm;
        else
           let Val =  apply_scm(Obj,tl(Msg),2)
           in
              if Val = bad_method_name then
                   mishap_scm(Msg_bad_method,
                                [%hd(tl(Msg)),
                                  send([%Obj,Schemify("type")%])
                                %])
              else Val
              endif
           endlet
        endif;
    endlet
enddefine;

define send_scm(/*...*/);
   send(listify_count(/*...*/));
enddefine;


VAR send = send_scm;
VAR 'bad-method-name' = bad_method_name;
VAR make_ShellWidget = SF(make_ShellWidget,"make_ShellWidget");
VAR XtDestroyWidget  = SF(XtDestroyWidget, "XtDestroyWidget");
VAR xmRowColumnWidget  = xmRowColumnWidget;
VAR xmPushButtonWidget = xmPushButtonWidget;
VAR xmScaleWidget      = xmScaleWidget;
VAR XtCreateManagedWidget = SF(XtCreateManagedWidget,"XtCreateManagedWidget");
VAR XtRealizeWidget       = SF(XtRealizeWidget<>identfn(%true%),"XtRealizeWidget");
VAR 'set-XptValue!'       = SF(procedure(W,name,val);
                                 val->XptValue(W,name);
                                 val;
                               endprocedure, 'set-XptValue!');

VAR Unix = Unix;
loadinclude XmConstants;    ;;; rather than: include XmConstants

constant XmH = XmHORIZONTAL, XmV = XmVERTICAL;
VAR 'XmHORIZONTAL' = XmH;
VAR 'XmVERTICAL'   = XmV;

define rep_FF = mishap_scm(%'File Fountain not initialised',[]%);
enddefine;


vars Msg_FF = 'The File Fountain does not recognise this command';

define FileFountain(/*...*/n);
    if n==2 then
        let (Cmd,File) = /*...*/
        in
            if Cmd = 'find' then
                sys_file_match(File,false,false,false) -> rep_FF;
                undef_scm;
            else
                mishap_scm(Msg_FF,[%Cmd,File%])
            endif;
        endlet
    elseif n==1 then
        let Cmd = /*...*/
        in
            if Cmd = 'next' then
                let Name = rep_FF() in
                  if Name==termin then false else Name
                  endif;
                endlet
            else
                mishap_scm(Msg_FF,[%Cmd,File%])
            endif
        endlet
    else mishap_scm('send given wrong number %p of arguments for FileFountain',
                    [%n%])
    endif;
enddefine;

VAR FileFountain = FileFountain;
