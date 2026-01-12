% ---------------------------
% GAME DEFINITIONS
% ---------------------------

colors([green, yellow, blue, orange, white, black]).

% board = [A, B, C, D, E, F]

% ---------------------------
% CONSTRAINTS
% ---------------------------

% %%%%%%%%%%%%%%%%%%%%%%
% ANYWHERE

% token X can be placed anywhere in Board
anywhere(_, _).

% %%%%%%%%%%%%%%%%%%%%%%
% NEXT TO

% verifies if the first two parameters occur consecutively in the list provided in the third parameter
consecutive(X, Y, Board) :-
    append(Prefix, [X, Y | Suffix], Board).

% X is always next to itself
next_to(X,X,_).

% X is next to Y
next_to(X,Y,[A,B,C,D,E,F]) :-
    consecutive(X,Y,[A,B,C,D,E,F]).

% Y is next to X
next_to(X,Y,[A,B,C,D,E,F]) :-
    consecutive(Y,X,[A,B,C,D,E,F]).

% %%%%%%%%%%%%%%%%%%%%%%
% ONE SPACE

% X is always one space apart from itself (contraint of constraint)
one_space(X, X, _).

% X is one space apart from Y
one_space(X, Y, [A, B, C, D, E, F]) :-
    one_space_aux(X, Y, [A, B, C, D, E, F]).

% Y is one space apart from X
one_space(X, Y, [A, B, C, D, E, F]) :-
    one_space_aux(Y, X, [A, B, C, D, E, F]).

% one space aux using append
one_space_aux(X, Y, Board) :-
    append(Prefix, [X, _mid, Y | Suffix], Board).

% %%%%%%%%%%%%%%%%%%%%%%
% ACROSS

% X and Y precisam ficar em lados opostos

across(X, X, _ ).

across(X, Y, Board) :-
    % pegar posicao de X e Y e depois checar se estão across
    get_position(X, Board, PosX),
    get_position(Y, Board, PosY),
    valid_across(PosX, PosY).

% get position(+Color, +Board, -Pos)
get_position(Color, [Color | Tail], 1).

get_position(Color, [_ | Tail], Pos) :-
    get_position(Color, Tail, Pos1),
    Pos is Pos1+1.

% valid position for across

valid_across(1, 6).
valid_across(6, 1).
valid_across(2, 6).
valid_across(6, 2).

valid_across(1, 5).
valid_across(5, 1).
valid_across(2, 5).
valid_across(5, 2).

valid_across(1, 4).
valid_across(4, 1).
valid_across(2, 4).
valid_across(4, 2).

% %%%%%%%%%%%%%%%%%%%%%%
% SAME EDGE

% X e Y precisam ficar na mesma aresta

same_edge(X, X, _).

same_edge(X, Y, Board) :-
    % pegar a posicao de X e de Y e checar se sao posicoes do mesmo edge
    get_position(X, Board, PosX),
    get_position(Y, Board, PosY),
    valid_same_edge(PosX, PosY).

valid_same_edge(Pos1, Pos2) :-
    in_edge1(Pos1, Pos2) ; in_edge2(Pos1, Pos2).

in_edge1(Pos1, Pos2) :-
    % member checks if X is member of List
    member(Pos1, [1,2]), member(Pos2, [1,2]).

in_edge2(Pos1, Pos2) :-
    member(Pos1, [4,5,6]), member(Pos2, [4,5,6]).

% %%%%%%%%%%%%%%%%%%%%%%
% POSITION

% X precisa estar numa das posicoes de L

position(X, L, Board) :-
    get_position(X, Board, PosX),
    member(PosX, L).

% ---------------------------
% PART 1
% ---------------------------

% solve(+Constraints, -Board) that receives a list of constraints and returns a valid board that satisfies all constraints, or fails if that is not possible => generate and test

solve(Constraints, Board) :-
    colors(Colors),
    permutation(Colors, Board),
    check_constraints(Constraints, Board).

% generate all boards
permutation([], []).

% get a List and calculate all permutations for that List
permutation(List, [Elem | RestPermutation]) :-
    select(Elem, List, Rest),
    permutation(Rest, RestPermutation).

% select an Elem from List and returns the Rest
select(Elem, [Elem | T], T).

% if Elem is not the head, skip it and searches for it in Tail
select(Elem, [OtherElem | T], [OtherElem | Rest]) :-
    select(Elem, T, Rest).

% apply constraints
check_constraints([], _).

% get list of constraints and apply
check_constraints([Con | Cons], Board) :-
    call(Con, Board),
    check_constraints(Cons, Board).

% %%%%%%%%%%%%%%%%%%%%%%
% EXEMPLOS

%% 12 solutions
example(1, [ next_to(white,orange),
    next_to(black,black),
    across(yellow,orange),
    next_to(green,yellow),
    position(blue,[1,2,6]),
    across(yellow,blue) ]).

%% 1 solution
example(2, [ across(white,yellow),
    position(black,[1,4]),
    position(yellow,[1,5]),
    next_to(green, blue),
    same_edge(blue,yellow),
    one_space(orange,black) ]).

%% no solutions (5 constraints are satisfiable)
example(3, [ across(white,yellow),
    position(black,[1,4]),
    position(yellow,[1,5]),
    same_edge(green, black),
    same_edge(blue,yellow),
    one_space(orange,black) ]).

%% same as above, different order of constraints
example(4, [ position(yellow,[1,5]),
    one_space(orange,black),
    same_edge(green, black),
    same_edge(blue,yellow),
    position(black,[1,4]),
    across(white,yellow) ]).

% ---------------------------
% PART 2
% ---------------------------
