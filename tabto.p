
;;; tabto.p                           R.J.Popplestone Sep. 1987

;;; Written independently, 1987.


define tabto n;
  if n<pop_charout_col then 1.nl; tabto(n)
  else sp(n-pop_charout_col)
  endif
enddefine;
