input(N1, T, O, N2) --> find, a, set, of, num(N1), num_type(T), integers, that, oper(O), to, num(N2). 
input(N1, T1, N2, T2, O, N3) --> find, a, set, of, num(N1), num_type(T1), and, num(N2), num_type(T2), integers, that, oper(O), to, num(N3). 
oper("sum") --> ["sum"].
oper("multiply") --> ["multiply"].
num_type("even") --> ["even"].
num_type("odd") --> ["odd"].
num(N) --> [Z], {number_string(N, Z)}.
find --> ["Find"].
a --> ["a"].
and --> ["and"].
set --> ["set"].
of --> ["of"].
integers --> ["integers"].
that --> ["that"].
to --> ["to"].

% Will display a list of numbers separatedby commas
display_answer([Head|[]]) :- !,write(Head),nl   .
display_answer( [Head|Tail] ) :-
   write( Head ),
   write(','),
   display_answer( Tail ).

% Function that takes in two numbers and does the correct operation and puts it in res
% Base Case if there are no more even or odd to use the result is if Start is Goal
process(Start, Goal, 0, 0,_ ,_,List) :- Start is Goal,display_answer(List).

% Otherwise find the next even number and add it
process(Start, Goal, NumEven, NumOdd, "sum", Last, List) :- 
    NumEven > 0,
    between(Last, 128, N1),
    0 is N1 mod 2,
    NewEven is NumEven-1,
    NewStart is Start + N1,
    NewLast is N1 + 1,
    append(List, [N1], L2),
    process(NewStart, Goal, NewEven, NumOdd, "sum", NewLast, L2).

% Otherwise find the next odd number and add it
process(Start, Goal, 0, NumOdd, "sum", Last,List):- 
    NumOdd > 0,
    between(Last, 128, N1),
    1 is N1 mod 2,
    NewOdd is NumOdd-1,
    NewStart is Start + N1,
    NewLast is N1 + 1,
    append(List, [N1], L2),
    process(NewStart, Goal, 0, NewOdd, "sum", NewLast, L2).

% Otherwise multiply the next even number
process(Start, Goal, NumEven, NumOdd, "multiply", Last, List) :- 
    NumEven > 0,
    between(Last, 128, N1),
    0 is N1 mod 2,
    NewEven is NumEven-1,
    NewStart is Start * N1,
    NewLast is N1 + 1,
    append(List, [N1], L2),
    process(NewStart, Goal, NewEven, NumOdd, "multiply", NewLast, L2).

% Otherwise multiply start by the next odd integer
process(Start, Goal, 0, NumOdd, "multiply", Last, List):- 
    NumOdd > 0,
    between(Last, 128, N1),
    1 is N1 mod 2,
    NewOdd is NumOdd-1,
    NewStart is Start * N1,
    NewLast is N1 + 1,
    append(List, [N1], L2),
    process(NewStart, Goal, 0, NewOdd, "multiply", NewLast, L2).

main(W) :-
    % W is a list of atoms so get the first element since it is only a 1 element list
    W = [H|_],
    % Convert it to a string so we can split by space
    string_to_atom(S, H),
    split_string(S, " ", "", L),
    (   
        % Case where they only ask for only even numbers and sum operation
        input(Num1, "even", "sum", Num2, L, []) -> (process(0, Num2, Num1, 0, "sum", 0, []); write("No Solution"));

        % Case where they only ask for only odd numbers and sum operation
        input(Num1, "odd", "sum", Num2, L, []) ->  (process(0, Num2, 0, Num1, "sum", 0, []); write("No Solution"));

        % Case where they only ask for only even numbers and multiply operation
        input(Num1, "even", "multiply", Num2, L, []) ->(process(1, Num2, Num1, 0, "multiply", 0, []); write("No Solution"));

        % Case where they only ask for only odd numbers and multiply operation
        input(Num1, "odd", "multiply", Num2, L, []) -> (process(1, Num2, 0, Num1, "multiply", 0, []); write("No Solution"));

        % Case for when they ask for both even AND odd numbers for the sum operation
        input(Num1, _, Num2, _, "sum", Num3, L, []) -> (process(0, Num3, Num1, Num2, "sum", 0, []); write("No Solution"));

        % Case for when they ask for both even AND odd numbers for the multiply operation
        input(Num1, _, Num2, _, "multiply", Num3, L, []) -> (process(1, Num3, Num1, Num2, "multiply", 0, []); write("No Solution"));

        % Otherwise it is an invalid string
        write("Invalid String")
    ).
    
     

    