

define show_machine_code(P);
  for i from 1 to 10 do pr_hex(fast_subscrv(i,P)); nl(1);
  endfor
enddefine;

define pr_hex(x);
lvars i;
  for i from 28 by -4 to 4 do
    pr_hex_dig((x fi_>> (i-2) )  && 15)
  endfor;
    pr_hex_dig((x fi_<< 2) && 15)
enddefine;

define pr_hex_dig(d);
  if d>=10 then cucharout(d-10+`A`);
  else pr(d);
  endif
enddefine;
