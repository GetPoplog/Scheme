

Proof of the soundness of resolution.
-------------------------------------
This section is under construction, and is not examinable.


_____Lemma _2
If a sentence S of the propositional calculus is unsatisfiable and S' is a
sentence of the propositional calculus, then S&S' is unsatisfiable.

_____Proof
This is immediate, since a conjunction can only evaluate to T if both its
arguments evaluate to T.


We shall write Res(C_1,C_2) for the resolvent of C_1 and C_2.

For any set of clauses {C_1...C_m}, we will write R({C_1...C_m},n) for the set
of clauses obtained  by n  resolutions. We  will require  that any  clause-set
{C'_1...C'_m'} obtainable by successive  resolutions from {C_1...C_m} will  be
equal to R({C_1...C_m},n) for some n. Note that in the propositional  calculus
we can only obtain a ______finite number of new clauses by resolution.

_____Lemma 3
If S is a sentence of the propositional logic and the conjunction S&Res(C1,C2)
is unsatisfiable,  where  C1  and  C2  are  clauses, then  S  &  C1  &  C2  is
unsatisfiable.

_____Proof
Suppose S & C1 & C2 is satisfiable. Then there exists an assignment of values
to variables that make S, C1 and C2 all evaluate to T. Therefore there are
literals  l in C1  and k in C2 that evaluate to T under this assignment.
____Case _1 l  = l_i. Then l_i occurs in the resolvent C, so C evaluates to
true under the assignment, as does S. Therfore C&S evaluates to true, a
contradiction.
____Case _2 l  = p, k = k_j. Then k_j occurs in the resolvent C, thus similarly C&S
is satisfiable, a contradiction.
____Case _3  l = p, k = ~p. But in this case l and k cannot both evaluate to T, a
contradiction.


_______Theorem
Let {C_1, C_2....C_m} be a set of clauses. If, for some n>=0, R({C_1...C_m},n)
is unsatisfiable, then the conjunction C_1&C_2...C_m is unsatisfiable.

_____Proof __by _________induction
We shall prove this by induction on n.

Base case: n=0
Suppose R({C_1.....C_m},0) is unsatisfiable. But R({C_1...C_m},0) =
{C_1...C_m}. Hence the conjunction C_1 & C_2....C_m  is unsatisfiable.

Inductive step:
Suppose for some n, for any set of clauses {C_1...C_m}, if R({C_1...C_m},n)
is unsatisfiable then C_1&C_2...C_m is unsatisfiable.

Now let  {C_1....C_m} be a set of clauses for which

       R({C_1...C_m},n+1) = {C'_1...C'_m'}

is unsatisfiable. But

       R({C_1...C_m},n+1) = {C"_1...C"_m",Res(C"_j",C"_k")}

where {C"_1...C"_m"}  = R({C_1...C_m}).
Hence by lemma 3,  the conjunction C"_1&C"_2..C"_m" & C"_j" & C"_k" is
unsatisfiable. Hence by the commutativity, associativity and idempotency of
conjunction, C"_1&C"_2..C"_n is unsatisfiable. So, by the inductive
hypothesis, C_1&C_2...C_m is unsatisfiable.
