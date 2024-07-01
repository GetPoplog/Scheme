

              CMPSCI 287    Programming Assignment  3  Spring 96

         Submit as prog3.scm in your cs287 directory by 23:59, 11APR96


(1) Write a Scheme function merge_general which takes two ordered lists and
performs a generalised merge operation.  A call:

   (merge_general key same? less? combine acc base_1 base_2 l1 l2)

takes two lists l1 and l2 and produces a result as follows.

If l1 is empty, then the result is obtained by applying base_2 to l2
If l2 is empty, then the result is obtained by applying base_1 to l1

Otherwise key  is  applied  to the  first  members  of l1  and  l2  to  obtain
"key-values". These are then checked with same?

If same? returns #t then  combine is used to combine  the first members of  l1
and l2 with the result of a recursive call on the rest of l1 and l2.

Otherwise the key-values  of the first members of l1 and l2 are compared with
less?.

If less? returns #t  then acc is  used to generate a  result derived from  the
first member of l1 and result of a recursive call of merge_general on the rest
of l1 with l2.

Otherwise acc is used to generate a result derived from the first member of l2
and result of a recursive call of merge_general on the rest of l2 with l1.

If either same? or less? returns a  list, then merge_general returns  the car
of that  list. (This seemingly bizarre requirement allows you to cause
merge_general to return a result without further recursion if the result is
manifest.)

Any other result from the comparison functions same? and less? results in an
error.

(2) Using merge_general and no  other explicit recursion, write the  functions
union, intersect, subset, merge_adding.

Here, union and intersect are to  work on lists of numbers.

The function call  (subset l1 l2) returns #t when l1  and l2 are ordered
lists, and  the members of l1 form  a subset of the members  of l2, #f
otherwise.

The function merge_adding takes two ordered lists of string-number  pairs,
and combines them  by adding  the numbers associated  with identical  strings.
Note that you can use string<? and string=? to compare strings.

(example '(intersect '(1 3 4 5 6) '(3 4 6 7)) '(3 4 6))

(example '(union '(1 3 4 5 6) '(3 4 6 7)) '(1 3 4 5 6 7))

(example '(subset '(4 6 7)  '(2 4 5 6 7 9) #t))

(example '(subset '(4 6 7 20)  '(2 4 5 6 7 9) #f))

(example '(subset '(4 6 7)  '(2 4 5  7 9) #f))

(example
    '(merge_adding
        '(("apples" 4) ("bananas" 7) ("oranges" 2))
        '( ("bananas" 3) ("oranges" 4))

        )

      '(("apples" 4) ("bananas" 10) ("oranges" 6))
    )
