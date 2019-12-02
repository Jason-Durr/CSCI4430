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
        % Case where they only ask for even OR odd numbers
        input(Num1, Type, Oper, Num2, L, []) -> write(Num1), write(Type), write(Oper), write(Num2);

        % Case for when they ask for both even AND odd numbers
        input(Num1, Type1, Num2, Type2, Oper, Num3, L, []) -> write(Num1), write(Type1), write(Num2), write(Type2), write(Oper), write(Num3);
        
        % Otherwise it is an invalid string
        write("Invalid String")
    ).
    
     

    