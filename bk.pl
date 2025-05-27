% ============================================
% Background Knowledge for Color Matching Game
% ============================================
% This file contains the core rules and background knowledge
% for the Color Matching Game, including color definitions,
% adjacency rules, and scoring rules.

% Dynamic predicates
:- dynamic cell/3.  % cell(X, Y, Color)
:- dynamic block/2. % block(BlockID, Color)

% Basic facts
color(yellow).
color(blue).
color(orange).
color(red).
color(magenta).
color(purple).
color(cyan).
color(green).

% Helper predicates
valid_coord(X, Y) :-
    grid_size(Size),
    Max is Size - 1,
    X >= 0,
    X =< Max,
    Y >= 0,
    Y =< Max.

cell_occupied(X, Y) :-
    cell(X, Y, placed).

cell_has_color(X, Y, Color) :-
    cell(X, Y, Color),
    Color \= placed.

% Game rules
can_place_block(BlockID, X, Y) :-
    valid_coord(X, Y),
    block(BlockID, Color),
    cell_has_color(X, Y, Color),
    \+ cell_occupied(X, Y).

% Utility predicates
between(L, U, L) :- L =< U.
between(L, U, X) :- L < U, L1 is L+1, between(L1, U, X).

cell(X, Y) :-
    grid_size(S),
    between(0, S-1, X),
    between(0, S-1, Y).

adjacent(X1, Y1, X2, Y2) :-
    (X1 = X2, Y2 is Y1 + 1);
    (X1 = X2, Y2 is Y1 - 1);
    (Y1 = Y2, X2 is X1 + 1);
    (Y1 = Y2, X2 is X1 - 1).

calculate_score(X, Y, Color, Score) :-
    findall(1,
            (adjacent(X, Y, AdjX, AdjY),
             cell_occupied(AdjX, AdjY),
             cell(AdjX, AdjY, Color)),
            AdjacentBlocks),
    length(AdjacentBlocks, Score).

% ============================================
% Common Predicates
% ============================================

valid_move(BlockID, X, Y) :-  % Check if a move is valid
    valid_coord(X, Y),
    can_place_block(BlockID, X, Y).

% ============================================
% Background Knowledge
% ============================================

init_grid :-  % Initialize grid with random colors
    retractall(cell(_, _, _)),
    grid_size(Size),
    forall(between(1, Size, X),
           forall(between(1, Size, Y),
                  (random_color(Color),
                   assertz(cell(X, Y, Color))))).

random_color(Color) :-  % Get a random color
    findall(C, color(C), Colors),
    random_member(Color, Colors).

display_grid :-  % Display the current grid
    grid_size(Size),
    nl,
    forall(between(1, Size, Y),
           (nl,
            forall(between(1, Size, X),
                   (cell(X, Y, Color) ->
                       write(Color), write('\t')
                   ;
                       write('empty\t')
                   )))),
    nl, nl.

valid_coordinate(X, Y) :-  % Valid coordinates within grid
    grid_size(Size),
    Max is Size - 1,
    X >= 0, X =< Max,
    Y >= 0, Y =< Max.

:- dynamic object_color/2.  % Object colors from predefined layouts
:- dynamic cell_color/3.

cell_occupied(X, Y, state(place(_, X, Y), _)).  % Check if a cell is occupied
cell_occupied(X, Y, state(place(_, OtherX, OtherY), State)) :-
    (X \= OtherX ; Y \= OtherY),
    cell_occupied(X, Y, State).

game_over :-  % Game is over when no more blocks are available
    \+ block(_, _).
game_over :-  % Game is over when no valid moves are possible
    \+ (block(_, Color),
        cell(X, Y, Color),
        \+ cell_occupied(X, Y)). 