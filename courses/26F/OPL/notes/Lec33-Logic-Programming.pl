%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Logic Programming %%%
%%%         in        %%%
%%%       Prolog      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%


human(socrates).

mortal(X) :- human(X).


parent(dale, paul).
parent(terry, paul).
parent(carmella, dale).
parent(carl, dale).
parent(ethel, carl).
parent(ceryl, carl).

parent(carl, dennis).
parent(carl, cathy).

% the comma "," means "and"
grandparent(A, C) :- parent(A, B), parent(B, C).

ancestor(A, B) :- parent(A, B).
%ancestor(A, C) :- parent(A, B), parent(B, C).
%ancestor(A, D) :- parent(A, B), parent(B, C), parent(C, D).
ancestor(A, C) :- parent(A, B), ancestor(B, C).

related(A, B) :- ancestor(X, A), ancestor(X, B).

equal(X,X).

% the semicolon ";" means "or"
different_number(X,Y) :- (X < Y); (X > Y).

palindrome(X) :- reverse(X,X).

end(Xs, X) :- reverse(Xs, [X | _]).

sum([X | Rest], N) :- sum(Rest, M), N is X + M.
sum([], 0).

pairwise([ X, Y | Rest ], [ [X,Y] | Pairs ]) :-
		pairwise([Y | Rest], Pairs).
pairwise([_], []).
pairwise([], []).

typeof(true, bool).
typeof(false, bool).

%typeof(0, num).
%typeof(1, num).
%typeof(2, num).
%...
typeof(N, num) :- between(0, infinite, N).

typeof(N+M, num) :- typeof(N, num), typeof(M, num).
typeof(zero(N), bool) :- typeof(N, num).

typeof(if(Check,Then,Else), T) :-
		typeof(Check, bool),
		typeof(Then, T),
		typeof(Else, T).

typein(Env, var(X), T) :- member([X, T], Env).

typein(Env, app(M, N), T) :-
    typein(Env, M, arrow(S, T)),
    typein(Env, N, S).

typein(Env, fun(X, T, M), arrow(T, S)) :-
    typein([[X, T] | Env], M, S).


%%%%%%%%%%%%%%%%%%%%%%%
%% What's for lunch? %%
%%%%%%%%%%%%%%%%%%%%%%%

% 10 friends want to decide what's for lunch. They vote on these options:

%  1. Curry
%  2. Pizza
%  3. Tacos
%  4. Sushi
%  5. Dumplings
%  6. BBQ
%  7. Mezze

% In total, the tallies were

%  * 1 option received 4 votes
%  * 2 options received 2 votes each
%  * 2 options received 1 vote each
%  * 2 options recieved 0 votes

% along with these constraints:

%  1. Curry and Tacos got different numbers of votes.

%  2. Sushi either got the most (4) votes, or it got 0 votes

%  3. Curry got more votes than Mezze did

%  4. In the order of the list, each of the options with 2 votes
%     follows an option with 0 votes

%  5. Either Mezze got one fewer votes than Pizza, or it got one
%     fewer votes than Tacos.

votes([Curry, Pizza, Tacos, Sushi, Dumplings, BBQ, Mezze]) :-
		% Consider all possible outcomes for the tallies
		permutation(
				[Curry, Pizza, Tacos, Sushi, Dumplings, BBQ, Mezze],
				[4, 2, 2, 1, 1, 0, 0]),
		% Constraint 1.
		different_number(Curry, Tacos),
		% Constraint 2.
		(Sushi = 4 ; Sushi = 0),
		% Constraint 3.
		Curry > Mezze,
		% Constraint 4
		rule4([Curry, Pizza, Tacos, Sushi, Dumplings, BBQ, Mezze]),
		% Constraint 5
    (Mezze is Pizza-1; Mezze is Tacos-1).

rule4([0, 2 | Rest]) :- rule4(Rest).
rule4([X | Rest]) :- X \= 2, rule4(Rest).
rule4([]).
