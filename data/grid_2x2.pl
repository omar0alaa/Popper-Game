% ============================================
% 2x2 Grid Template
% ============================================
% This file serves as a template for 2x2 grid layouts.
% The actual game now uses random colors.

% Example grid layout (for reference only)
example_grid([
    [yellow, blue],
    [orange, red]
]).

% Example object colors (for reference only)
example_objects([
    o1-yellow,
    o2-blue,
    o3-orange,
    o4-red
]).

% Example valid moves (for reference only)
example_moves([
    place(o1, 0, 0),  % Place yellow block on yellow cell
    place(o2, 0, 1),  % Place blue block on blue cell
    place(o3, 1, 0),  % Place orange block on orange cell
    place(o4, 1, 1)   % Place red block on red cell
]).

% ============================================
% GRID CONFIGURATION
% ============================================
:- dynamic grid_size/1.
grid_size(2).  % 2x2 grid

% ============================================
% OBJECTS CONFIGURATION (Outside the grid)
% ============================================

% Define objects (outside the grid initially)
object(o1). object(o2). object(o3). object(o4).

% Define object colors
% These objects are initially outside the grid and can be placed
% in any grid cell with a matching color
object_color(o1, 'RED').
object_color(o2, 'BLUE').
object_color(o3, 'GREEN').
object_color(o4, 'YELLOW').

% ============================================
% GRID CONFIGURATION
% ============================================

% Define grid cell colors (2x2 grid)
% These are the target cells where objects can be placed
cell_color(0, 0, 'RED').
cell_color(0, 1, 'BLUE').
cell_color(1, 0, 'GREEN').
cell_color(1, 1, 'YELLOW').

% ============================================
% EXAMPLE STATES & TRANSITIONS
% ============================================

% Define example states for testing
% Initial state (all objects outside grid)
my_state(state0).

% After placing o1 in the red cell at (0,0)
my_state(state1) :- 
    my_state(state(place(o1, 0, 0), state0)).

% After placing o2 in the blue cell at (0,1)
my_state(state2) :- 
    my_state(state(place(o2, 0, 1), state1)).

% ============================================
% ADDITIONAL TEST CASES
% ============================================

% Positive examples 
test_pos(valid_place_action(o1, 0, 0, state0)).  % o1(red) in red cell
test_pos(valid_place_action(o2, 0, 1, state0)).  % o2(blue) in blue cell
test_pos(valid_place_action(o3, 1, 0, state0)).  % o3(green) in green cell
test_pos(valid_place_action(o4, 1, 1, state0)).  % o4(yellow) in yellow cell

% Negative examples
test_neg(valid_place_action(o1, 0, 1, state0)).  % Color mismatch: red object in blue cell
test_neg(valid_place_action(o2, 1, 0, state0)).  % Color mismatch: blue object in green cell
test_neg(valid_place_action(o1, 0, 0, state1)).  % Object already placed
test_neg(valid_place_action(o2, 0, 0, state1)).  % Cell already occupied 