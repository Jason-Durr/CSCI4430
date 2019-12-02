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

% Function that takes in two numbers and does the correct operation and puts it in res

% Base Case if there are no more even or odd to use the result is if Start is Goal
process(Start, Goal, 0, 0,_ ,_) :- Start is Goal.

%otherwise find the next even number and add it
process(Start, Goal, NumEven, NumOdd, "sum", Last) :- 
    NumEven > 0,
    between(Last, 120, N1),
    0 is N1 mod 2,
    NewEven is NumEven-1,
    NewStart is Start + N1,
    NewLast is N1 + 1,
    process(NewStart, Goal, NewEven, NumOdd, "sum", NewLast),
    write(N1),nl.

% Otherwise find the next odd number and add it
process(Start, Goal, 0, NumOdd, "sum", Last):- 
    NumOdd > 0,
    between(Last, 120, N1),
    1 is N1 mod 2,
    NewOdd is NumOdd-1,
    NewStart is Start + N1,
    NewLast is N1 + 1,
    process(NewStart, Goal, 0, NewOdd, "sum", NewLast),
    write(N1),
    nl.

% otherwise multiply the next even number
process(Start, Goal, NumEven, NumOdd, "multiply", Last) :- 
    NumEven > 0,
    between(Last, 120, N1),
    0 is N1 mod 2,
    NewEven is NumEven-1,
    NewStart is Start * N1,
    NewLast is N1 + 1,
    process(NewStart, Goal, NewEven, NumOdd, "multiply", NewLast),
    write(N1),
    nl.

% otherwise multiply start by the next odd integer
process(Start, Goal, 0, NumOdd, "multiply", Last):- 
    NumOdd > 0,
    between(Last, 120, N1),
    1 is N1 mod 2,
    NewOdd is NumOdd-1,
    NewStart is Start * N1,
    NewLast is N1 + 1,
    process(NewStart, Goal, 0, NewOdd, "multiply", NewLast),
    write(N1),
    nl.

%      Test cases to copy paste into terminal
%      swipl -q -f part_2.pl -t main "Example Input String"
%      swipl -q -f part_2.pl -t main "Find a set of 4 prime numbers that sum to 3"
%      swipl -q -f part_2.pl -t main "Find a set of 2 even integers that sum to 6"
%      swipl -q -f part_2.pl -t main "Find a set of 1 even and 1 odd integers that multiply to 6"


main(W) :-
    % W is a list of atoms so get the first element since it is only a 1 element list
    W = [H|_],
    % Convert it to a string so we can split by space
    string_to_atom(S, H),
    split_string(S, " ", "", L),
    (   
        % Case where they only ask for only even numbers and sum operation
        input(Num1, "even", "sum", Num2, L, []) -> process(0, Num2, Num1, 0, "sum", 0);

        % Case where they only ask for only odd numbers and sum operation
        input(Num1, "odd", "sum", Num2, L, []) ->  process(0, Num2, 0,Num1,"sum",0);

        % Case where they only ask for only even numbers and multiply operation
        input(Num1, "even", "multiply", Num2, L, []) -> process(1, Num2, Num1, 0, "multiply", 0);

        % Case where they only ask for only odd numbers and multiply operation
        input(Num1, "odd", "multiply", Num2, L, []) ->  process(1,Num2, 0, Num1, "multiply", 0);

        % Case for when they ask for both even AND odd numbers for the sum operation
        input(Num1, _, Num2, _, "sum", Num3, L, []) -> process(0, Num3, Num1, Num2, "sum", 0);

        % Case for when they ask for both even AND odd numbers for the multiply operation
        input(Num1, _, Num2, _, "multiply", Num3, L, []) -> process(1, Num3, Num1, Num2, "multiply", 0);

        % Otherwise it is an invalid string
        write("Invalid String")
    ).
    
     

    