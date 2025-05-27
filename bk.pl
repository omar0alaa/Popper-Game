% ============================================
% Background Knowledge for Color Matching Game
% ============================================
% This file contains the core rules and background knowledge
% for the Color Matching Game, including color definitions,
% adjacency rules, and scoring rules.

% Dynamic predicates
:- dynamic cell/3.  % cell(X, Y, Color)
:- dynamic block/2. % block(BlockID, Color)
:- discontiguous color/1.
:- discontiguous cell/3.
:- discontiguous neg/1.
:- discontiguous pos/1.

% Basic facts
color(red).
color(blue).
color(green).
color(yellow).

block(b1, red).
block(b2, blue).
block(b3, green).
block(b4, yellow).

cell(0,0,red).
cell(0,1,blue).
cell(1,0,green).
cell(1,1,yellow).

cell_has_color(X,Y,C) :- cell(X,Y,C).
cell_occupied(_,_) :- fail. % No cells are occupied in this simple setup
valid_coord(X,Y) :- member(X,[0,1]), member(Y,[0,1]).

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

% Background knowledge for object-to-cell color matching

% Define available colors
color(red).
color(blue).
color(green).
color(yellow).

% Define objects and their colors
object(o1).
object(o2).
object(o3).
object(o4).
object_color(o1, red).
object_color(o2, blue).
object_color(o3, green).
object_color(o4, yellow).

% Define cells and their colors
cell(0,0,red).
cell(0,1,blue).
cell(1,0,green).
cell(1,1,yellow).

% Helper: a cell is available if it is not occupied
cell_available(X,Y,State) :- \+ member(placed(_,X,Y), State).

% Helper: an object is available if it is not placed
object_available(O,State) :- \+ member(placed(O,_,_), State).

% Valid move: place object O in cell (X,Y) if colors match and both are available
can_place(O,X,Y,State) :-
    object_color(O,Color),
    cell(X,Y,Color),
    object_available(O,State),
    cell_available(X,Y,State).