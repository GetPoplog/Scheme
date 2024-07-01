


signature point =
  sig
    type point;
    val mk_point:real*real->point;
    val x_point:point->real;
    val y_point:point->real;

  end;

structure MyPoints:point = struct
    type point = real*real;
    fun x_point(x,y) = x;
    fun y_point(x,y) = y;
    fun mk_point(x,y) = (x,y):point;
end;

MyPoints.x_point(3.4,5.6);

structure MyOtherPoints:point = struct
    type point = real list;
    fun x_point(x::l) = x;
    fun y_point(x::y::l) = y;
    fun mk_point(x,y) = [x,y];
end;


structure YetMorePoints:point = struct
    datatype point = mk_point of real*real;
    fun x_point(mk_point(x,y)) = x;
    fun y_point(mk_point(x,y)) = y;
end;


functor MakeLine(p:point):
  sig
    type point
    type line;
    val mk_line:real*real*real*real -> line;
    val point_0_line:line->point;
    val point_1_line:line->point;
  end

  =

   struct

        open p;
        type line = point*point;
        fun point_0_line(x,y) = x;
        fun point_1_line(x,y) = y;
        fun mk_line(x1,y1,x2,y2) = (mk_point(x1,y1), mk_point(x2,y2))

   end;

structure MyLines = MakeLine(MyPoints);

structure MyOtherLines = MakeLine(MyOtherPoints);


open MyLines;
mk_line(3.0,4.0,5.0,6.0);
