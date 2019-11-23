%% Runs the program to find curious set of integers
main() :-
    curious_set(A,B,C,D).

% Makes sure number isnt in the original curious set of integers
not_in_og(N1) :- 
    N1 \= 1,
    N1 \= 3,
    N1 \= 8,
    N1 \= 120.

% Checks 2 numbers to see if they follow the rule that the product
% is 1 less than a perfect square
curious_set_help(N1,N2) :- 
    PSq1 is N1*N2+1,
    Sq is round(sqrt(PSq1)),
    PSq1 is Sq**2.

% This does the main check for four numbers to see if they
% make a curious set of integers
curious_set(N1,N2,N3,N4) :-
    % Check the first and second numbers
    between(0,10000,N1),
    not_in_og(N1),
    between(0,10000,N2),
    not_in_og(N2),
    N1 < N2,
    curious_set_help(N1,N2),
    
    % Check the third number
    between(0,10000,N3),
    not_in_og(N3),
    N2 < N3,
    curious_set_help(N1,N3),
    curious_set_help(N2,N3),

    % Check the fourth number
    between(0,10000,N4),
    not_in_og(N4),
    N3 < N4,
    curious_set_help(N1,N4),
    curious_set_help(N2,N4),
    curious_set_help(N3,N4),

    % Write the output
    write(N1),write(','),
    write(N2),write(','),
    write(N3),write(','),
    write(N4).