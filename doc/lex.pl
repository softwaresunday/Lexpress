%<html>
%<head>
%<style>
%h3       { font-size: 16pt; }
%body     { margin-left: 100; margin-right: 100; }
%</style>
%</head>
%<body>
%<h3>lexpress//0</h3>
%<p>
%The <b>L'express</b> procedure uses the Definite Clause Grammar
%notation to pass a <b>cost</b> vector and <b>PLA</b> structure to
%each of the goals in the body of this predicate.
%<center><b>costs(Irr,Red,Exp,Global)-PLA</b></center>
%Each step in the algorithm can create a new PLA structure
%which will produce a new cost evaluation for the new PLA.
%<code>
lexpress -->   unwrap,
               complement,
               expand,
               essential_primes,
                 iterate(init_cost),
               add_to_care,
               sub_from_dont_care,
               make_sparse.

%</code>
%<h3>iterate//1</h3>
%<ul><li>
%performs one step of the algorithm (or terminates).
%</li><li>
%calculates the new cost of the PLA
%</li><li>
%and selects the next step depending upon cost change.
%</li></ul>
%<code>

iterate(done) -->  [].
iterate(Step) -->  call(Step),
	           cost_value(Step, Previous, Current, PLA),
		   { sum_costs(PLA, cost(0, 0, 0), Current),
		     Previous = Current -> next(Step, Next,    _)
				        ;  next(Step,    _, Next)
		   },
		   iterate(Next).

%</code>
%<h3>next/3</h3>
%<p>
%In the form of a table, <b>next/3</b> defines which step to take
%in the algorithm, depending upon whether or not the cost is changing.
%<br>
%<b>next( Current-Step, Cost-Changed, Cost-Unchanged )</b>
%<code>

next( init_cost,   reduce,      reduce  ).
next( reduce,      expand,      done    ).
next( expand,      irredundant, reduce2 ).
next( irredundant, reduce,      reduce2 ).
next( reduce2,     init_cost,   done    ).

%</code>
%<h3>cost_value//4 (cost_value/6)<br>
%cost_value(+Step, -Prev, -New, -PLA, +DataIn, -DataOut)</h3>
%<p>Reaches down into the two hidden variables
%of the Definite Clause Grammar to give us the current
%PLA, the previous cost value, and the new cost variable.a
%<code>

cost_value(reduce,      I, N, P, costs(I,R,E,G)-P,costs(N,R,E,G)-P).
cost_value(expand,      R, N, P, costs(I,R,E,G)-P,costs(I,N,E,G)-P).
cost_value(irredundant, E, N, P, costs(I,R,E,G)-P,costs(I,R,N,G)-P).
cost_value(iterate,     G, N, P, costs(I,R,E,G)-P,costs(I,R,E,N)-P).
cost_value(reduce2,     G, N, P, costs(I,R,E,G)-P,costs(I,R,E,N)-P).

sum_costs([], Cost, Cost).
sum_costs([c(Input,Output)|Cs], cost(P0,In0,Out0), Cost) :-
   length(Input, LI),
   length(Output,LO),
   P1 is P0 + 1,
   In1 is LI + In0,
   Out1 is LO + Out0,
   sum_costs(Cs, cost(P1,In1,Out1), Cost).

init_cost(_-PLA, costs(C,C,C,C)-PLA) :-
    sum_costs(PLA, cost(0,0,0),C).
%</code>
%<ul>
%<li>
%Unification: a generalization of assignment
%</li><li>
%Resolution:  a generalization of "checking return codes"
%</li><li>
%Backtracking: a generalization of "if-then-elsif... "
%</li><li>
%Clauses:      a generalization of "switch statements"
%</li><li>
%Predicate:    generalization of "procedure" (e.g. enumeration)
%</li>
%</ul>

%When we say "a generalization of" <em>something</em>, we often mean
%that it is a more powerful form of something, in that it adds at
%least one functional benefit to the original concept.
%Unification is assignment with no left-hand-side vs. right-hand-side
%restriction, but data can't merely flow in either direction, it
%might actually move in both directions during the unification.
%<p>
%If <b>A = f(X, t(x,3,Y))</b> and <b>B = f(p(Y), J)</b>
%Then the single unification:  <b>A = B</b>
%has the result that  <b>A = B = f(p(Y), t(x,3,Y))</b>
%
%Espresso computations involve things like measuring the Hamming distance
%between two rows (``cubes'' in boolean N-space), and computing intersections.
%
%<h3>Cube Consensus</h3>
%<code>
consensus(C, D, Consensus) :-
    cube_distance(C, D, In, Out),
    consensus1(In, Out, C, D, Consensus).

consensus1(0, 0, c(In,Out), c(In2, Out2), c(IIn,IOut)) :-
    intersect( In,  In2,  IIn),
    intersect(Out, Out2, IOut).
consensus1(1, 0, c(In,Out), c(In2,Out), c(R,Out)) :-
    raise_intersection(In, In2, R).
consensus1(0, 1, c(In,Out), c(In,Out2), c(In,L)) :-
    lower_intersection(Out, Out2, L).

cube_distance(c(In,Out), c(In2,Out2), In, Out) :-
    distance(In,   In2, 0,  In ),
    distance(Out, Out2, 0, Out ).

%</code>

%<h3>union/4: Include mis-matched elements</h3>
%<code>
union([],  B, B, _) :- !.
union( A, [], A, _) :- !.
union([A|As], [B|Bs], [P|Ps], Goal) :-
      A>>1 < B>>1 -> B = P, union(As, [B|Bs], Ps, Goal)
   ;  A>>1 > B>>1 -> A = P, union([A|As], Bs, Ps, Goal)
   ; call(Goal, A, B, P),
     union(As, Bs, Ps, Goal).

%</code>
%<h3>intersect/4: Exclude mis-matched elements</h3>
%<code>
intersect([],  _, [], _) :- !.
intersect( _, [], [], _) :- !.

intersect([A|As], [B|Bs], Ps, Goal) :-
      A>>1 < B>>1,
      intersect(As, [B|Bs], Ps, Goal).

intersect([A|As], [B|Bs], Ps, Goal) :-
      A>>1 > B>>1,
      intersect([A|As], Bs, Ps, Goal).

intersect([A|As], [B|Bs], [P|Ps], Goal) :-
     call(Goal,A,B,P),
     intersect(As,Bs,Ps,Goal).
%</code>
%<h3>distance/4: Compute the distance between to cubes</h3>
%<code>
distance([],  _, D, D) :- !.
distance( _, [], D, D) :- !.
distance([A|As], [B|Bs], D0, D) :-
	   A>>1 < B>>1 -> distance(As, [B|Bs], D0, D)
	;  A>>1 > B>>1 -> distance([A|As], Bs, D0, D)
	; ( A \== B -> D1 is D0 + 1 ; D1 = D0 ),
	  distance(As, Bs, D1, D).
%</code>
%
%<p>
%Here we justify the use of if-then-else because in simple arithmetic tests,
%the creation of choice points can be completely avoided.
%<h3>Co-Factors of a single cube</h3>
%
%An example of the most fundamental of the low-level operations is the
%computation of the Shannon co-factor of a matrix, relative to a particular
%positive or negative variable.
%
%The computation of the co-factor corresponds to the following definition
%from [Brayton84] (They used 3 and 4 for the 0s and 1s in the output terms).
%When the Factor p is just a single coordinate, this becomes:
%<code>
co_cover([],_,[]).
co_cover([C|Cs],P,[X|Xs]) :-
   cofactor(C,P,X),
   !,
   co_cover(Cs,P,Xs).
co_cover([_|Cs],P,Xs) :-
   co_cover(Cs,P,Xs).
%</code>
%<h3>cofactor/3</h3>
%<p>
%<ul>
%<li>
%Terminate on End of List
%</li><li>
%Terminate on Exact Match
%</li><li>
%Terminate if current column number is past comparison point.
%</li><li>
%Not yet reached comparison column, continue with list.
%</li><li>
%The missing case is when the bits 'conflict', so cofactor/3 fails.
%</li>
%</ul>
%<code>
cofactor([],_,[]).         
cofactor([C|Cs],C,Cs).
cofactor([C|Cs],P,[C|Cs]) :-  C>>1 > P>>1.
cofactor([C|Cs],P,[C|Xs]) :-  C>>1 < P>>1, cofactor(Cs,P,Xs).
%</code>
%<h3>Shannon Co-Factors of a ``Cover''</h3>
%<p>
%Our cover for a function is the entire matrix, where each row is a
%cube in the space of the boolean variables. We frequently need to
%compute both positive and negative Shannon co-factors of the entire matrix.
%<code>
cofactors(Cover,Var,C1,C0) :-
    V1 is Var<<1 /\ 1,
    V0 is Var<<1 /\ 0,
    co_cover(Cover,V1,C1),
    co_cover(Cover,V0,C0).
%</code>
%
%And a more general version of the cofactor routine is provided to
%accept arbitrary cubes, rather than a single variable.
%<code>
gen_cofactor([],_,[]) :- !.

gen_cofactor(_,[],[]) :- !.

gen_cofactor([C|Cs],[F|Fs],[X|Xs]) :-
    ( C>>1 > F>>1     ->
	                 F = X,
                         gen_cofactor([C|Cs],Fs,Xs)
    ; C>>1 < F>>1     ->
	                 C = X,
                         gen_cofactor(Cs,[F|Fs],Xs)
    ; evaluate(C,F,X) ->
                         gen_cofactor(Cs,Fs,Xs)
    ). 

gen_cofactor([C|Cs],[F|Fs],Xs) :-
    A = C>>1,
    B = F>>1,
    compare(A,B,Result),
    gen_cofactor(Result,Cs,Fs,Xs).

cofactor(=, C, _, C).
cofactor(<, _, F, F).
cofactor(>, C, _, C).
%</code>
%
%<h3>make_parse//0</h3>
%<code>
make_sparse -->
       raise_inputs(Primes),
       lower_outputs(Primes).
%</code>
%
%<h3>raise_inputs//1: raise_inputs(-Prime)</h3>
%<h3>lower_outputs//1: lower_outputs(+Prime)</h3>
%<code>
raise_inputs(Primes, Cost-PLA0,Cost-PLA_Out) :-
      raise_inputs2(PLA0, PLA0, PLA_Out, Primes, []).

lower_outputs(Primes, Cost-PLA_In,Cost-PLA_Out) :-
      lower_outputs(PLA_In, Primes,PLA_Out).

raise_inputs2([], _, [], []).
raise_inputs2([Cube|Cubes], PLA, [P|Ps], [c(RIn,Out)|Ds]) :-
      inOut(Cube,In,Out),
      prime(In,PLA,P),
      raise_cube(In,RIn),
      raise_inputs2(Cubes, PLA, Ps, Ds).

raise_inputs2([c(In,Out)|Cs], PLA, Ps, [c(RIn,Out)|Ds]) :-
      raise_cube(In,RIn),
      raise_inputs2(Cs, PLA, Ps, Ds).
raise_inputs2([], _, [], []).
raise_inputs2([c(In,Out)|Cs], PLA, [P|Ps], [c(RIn,Out)|Ds]) :-
      prime(c(In,Out),PLA,P),
      raise_cube(In,RIn),
      raise_inputs2(Cs, PLA, Ps, Ds).

raise_inputs2([c(In,Out)|Cs], PLA, Ps, [c(RIn,Out)|Ds]) :-
      raise_cube(In,RIn),
      raise_inputs2(Cs, PLA, Ps, Ds).

%</code>
%
%
%
%<code>
prime(P) :- atom_chars(P,[a,x|_]), !.
prime(P) :-
   mysql(Connection),
   genSQL(candidates,[1],and(chemical='?',phase>3),SQL,[]),
   odbc_prepare(Connection, SQL, [default], Statement),
   odbc_execute(Statement, [P], [1]).

mysql(C) :-
    existing_sql_connection(C),
    !.

mysql(C) :-
    prompt_for_password(PW),
    odbc_connect('myDatabase', C, [user(peter),password(PW)]),
    !,
    assert(existing_sql_connection(C)).

mysql(_) :-
    writeln('Could not connect to myDatabase'),
    fail.


%<h3>genSQL//3 : genSQL(+Table, +Selected, +Conditions)</h3>
%<p>Generate SELECT statements from an expression tree.
%E.g. <b>or</b>(<b>and<b>(<b>=<b>(col1,val1),<b>&lt;</b>(col3,val3)),<b>&gt;</b>(col9,val9))
%
%<code>
genSQL(Table,Select,Cond) -->
     ['Select '], genList(Select), [' from ',Table,' where '],genCond(Cond).

genList([])     --> [].
genList([S])    --> [S].
genList([S|Ss]) --> [S],', ',genList(Ss).

genCond(and(X,Y)) --> '(', genCond(X), ' AND ', genCond(Y), ')'.
genCond(or(X,Y))  --> '(', genCond(X), ' OR ', genCond(Y), [')'].
genCond(<(A,B))   --> [A], ' < ', [B].
genCond(>(A,B))   --> [A], ' > ', [B].
genCond(=(A,B))   --> [A], ' = ', [B].
genCond('!='(A,B))--> [A], ' <> ',[B].
genCond(not(X))  --> '( not ', genCond(X), ')'.
genCond(notin(X,L))--> '( ', [X], ' not in (', genList(L), ')'.



%</body>
%</html>
