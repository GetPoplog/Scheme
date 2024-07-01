vars undef_persistent = 'undef_persistent';


defclass _persistent
   {
     ident_persistent,
     file_persistent,
     val_persistent
   };


define get_persistent(x);
  if is_persistent(x) then
      let v = val_persistent(x) in
         if v/==undef_persistent then v
         else get_from_file(ident_persistent(x),file_persistent(x))
         endif;
      endlet
  else x
  endif;
enddefine;



define car_pscm(x,n);
  unless n==1 then mishap_wrong_args("car")
  endunless;
  if ispair(x) then fast_front(x)
  else
     front(get_persistent(x))
  endif;
enddefine;


define open_persistent(file);


enddefine;
