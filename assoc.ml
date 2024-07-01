

datatype 'a Maybe = NotSo | OK of 'a;

fun assoc x [] = NotSo
  | assoc x ((x1,v) :: l) =
        if x=x1 then OK(x1,v)
        else assoc x l;


assoc 1 [(2,3),(1,4)];
assoc 1 [(2,3),(9,4)];

exception Lookup of 'a;

fun lookup x alist =
    let val maybe = assoc x alist
    in
        case maybe of
            OK(a,v) => v
         |   NotSo   => raise Lookup(x,alist)
       end
    ;



lookup 1 [(2,3),(1,4)];
lookup 1 [(2,3),(9,4)] handle Lookup => (print("cant find it");0);
