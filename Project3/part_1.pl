%% Runs the program and prints command line arguement
main() :-
     curious_set(A,B,C,D).

not_in_og(N1) :- 
    N1 \= 1,
    N1 \= 3,
    N1 \= 8,
    N1 \= 120.
curious_set_help(N1,N2) :- 
    PSq1 is N1*N2+1,
    Sq is round(sqrt(PSq1)),
    PSq1 is Sq**2.

curious_set(N1,N2,N3,N4) :-
    between(0,10000,N1),
    not_in_og(N1),
    between(0,10000,N2),
    not_in_og(N2),
    N1 < N2,
    
    
    curious_set_help(N1,N2),
    
    between(0,10000,N3),
    not_in_og(N3),
    N1 < N3,
    N2 < N3,
    curious_set_help(N1,N3),
    curious_set_help(N2,N3),
    between(0,10000,N4),
    not_in_og(N4),
    N1 < N4,
    N2 < N4,
    N3 < N4,
    curious_set_help(N1,N4),
    
    curious_set_help(N2,N4),
    curious_set_help(N3,N4),
    write(N1),write(','),
    write(N2),write(','),
    write(N3),write(','),
    write(N4),nl.
    % write(N3,',').
    % write(N4,'\n').