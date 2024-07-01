
/*
The circuit is constructed out of nodes and devices.
Each terminal of each device in the circuit is connected to a node.
The currents going into a node add up to zero (Kirchoff's law #?).

A node is represented by a prolog term nd(Volts,Currents) where Volts
is a symbol (atom in prolog) representing the voltage at the node,
and Currents is a list of the currents put into a node by the devices
connected to it.

*/

/*
device(D) creates a new name for a device
*/

device(D) :- D is gensym(d).

node(nd(Name,L)) :- Name is gensym(v).

/*
initialise is a utility predicate that sets the counts of all the gensym's
to zero.
*/

initialise:- X is initialise(0).

:-prolog_language(pop11).

define initialise(dummy);
  0 -> gensym("d");
  0 -> gensym("v");
  0;
enddefine;

:-prolog_language('prolog').

/*
Find the voltage of a node (or create a node with given voltage).
*/

volts(nd(V,I_sum),V).

/*
Assert that current I flows into a node with voltage V and currents I_sum
*/

current(nd(V,I_sum),I) :- member(I,I_sum).


/*
Define member(X,L), is X a member of the list L?
*/

member(X,[X|L]).
member(X,[Y|L]) :- member(X,L).


/* This predicate allows us to specify that a resistor of R ohms is
connected between points P2 and P2 in the circuit. This adds the constraint
E (which says "V=IR") to the circuit constraints */

resistor(R,P1,P2,Constraints,[E|Constraints]) :-
    device(D),                 /* Get a new device-name */
    volts(P1,V1),              /* The voltage at P1 is V1 */
    volts(P2,V2),              /* The voltage at P2 is V2 */
    E = (V1-V2 = R*i(D)),      /* resistor equation V=IR */
    current(P1,i(D)),          /* D gets its current from P1 */
    current(P2,-i(D)),         /* and puts it back into P2 */
    !.

/* This predicate allows us to specify that a Zener diode

*/

zener(V,P1,P2,Constraints,[E|Constraints]) :-
    device(D),
    volts(P1,V1),
    volts(P2,V2),
    E = (V1-V2 =< V),
    current(P1,i(D)),
    current(P2,-i(D)),!.

/*
A transistor of gain G has its collector connected to node C,
its base to node B and its emitter to node E. This adds 2
constraints to the circuit.
    C1 the base-emitter voltage drop is less than 0.5 volts.
    C2 the collector current is the base current multiplied by the gain.

*/

transistor(G,C,B,E,Constraints,[Con1,Con2|Constraints]) :-
    device(D),
    volts(C,VC),
    volts(E,VE),
    volts(B,VB),
    Con1 = (VB-VE=<0.5),       /* C1 */
    current(B,ib(D)),
    current(E,-ib(D)-ic(D)),
    current(C,ic(D)),
    Con2 = (ic(D) = G*ib(D)),  /* C2 */
    !.

/*
Here we have the definition of the voltage regulator in terms of the  basic
circuit components defined above. A regulator producing voltage V is
connected to nodes Vplus (the positive unregulated input supply), Gnd (the
common ground) and Out (the regulated output). This creates new constraints
for the whole  circuit which embodies the regulator.

(1) We need a new node for the point at which the zener, resistor and
transistor base are connected, since this is purely internal to the regulator.
(2) A 1000 ohm resistor is connected from Vplus to Node.
(3) A zener for voltage V is connected from Node to ground.
(4) A transistor of gain 40 is connected as an emitter follower, that
is with its collector connected to Vplus, its Base connected to Node and
its emitter connected to the output.
(5) Since all connections to Node are made, we sum the currents and adjoin
the constraint that the sum is zero to the circuit constraints.

*/



regulator(V,Vplus,Gnd,Out,Constraints1,Constraints_out) :-
    node(Node),                                                 /* 1 */
    resistor(1000,Vplus,Node,Constraints1,Constraints2),        /* 2 */
    zener(V,Node,Gnd,Constraints2,Constraints3),                /* 3 */
    transistor(40,Vplus,Node,Out,Constraints3,Constraints4),    /* 4 */
    sum_node(Node,Constraints4,Constraints_out),                /* 5 */
    !.

/*
This is the most non-logical part of the program.
*/


sum_node(nd(N,L),C,[S=0|C]) :- sum_junction(L,0,S).

/*
What we are doing here is naughty! We are assuming that the list of
currents flowing into a node is now closed. So if we come to the
variable at the end of the list, we use the sum accumulated so far.
*/

sum_junction(L,Sum,Sum) :- var(L),!.
sum_junction([I|L],Sum,I+Sum1) :- sum_junction(L,Sum,Sum1).


:- initialise.
?- regulator(5.6,nd(vcc,Icc),nd(gnd,Ignd),nd(out,Z),[],Con).

/*

Icc = [i(d0), ic(d2) | _1]   d0 and d2 draw their currents from vcc
Ignd = [- i(d1) | _2]        d1 puts its current into the ground.
Z = [- ib(d2) - ic(d2) | _3] the base and collector currents of d2
                             are combined and put into the output.

Con =
    [
    - i(d0) + (i(d1) + (ib(d2) + 0)) = 0,
    v0 - out =< 0.5,
    ic(d2) = 40 * ib(d2),
    v0 - gnd =< 5.6,
    vcc - v0 = 1000 * i(d0)
    ] ?

*/
