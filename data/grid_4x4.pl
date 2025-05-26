% grid_4x4.pl - Configuration for 4x4 grid size

% ============================================
% GRID CONFIGURATION
% ============================================
:- dynamic grid_size/1.
grid_size(4).  % 4x4 grid

% ============================================
% OBJECTS CONFIGURATION (Outside the grid)
% ============================================

% Define objects (outside the grid initially)
object(o1). object(o2). object(o3). object(o4).
object(o5). object(o6). object(o7). object(o8).
object(o9). object(o10). object(o11). object(o12).
object(o13). object(o14). object(o15). object(o16).

% Define object colors
% These objects are initially outside the grid and can be placed
% in any grid cell with a matching color
object_color(o1, 'RED').
object_color(o2, 'BLUE').
object_color(o3, 'GREEN').
object_color(o4, 'YELLOW').
object_color(o5, 'ORANGE').
object_color(o6, 'PURPLE').
object_color(o7, 'CYAN').
object_color(o8, 'MAGENTA').
object_color(o9, 'RED').        % Same color as o1
object_color(o10, 'BLUE').      % Same color as o2
object_color(o11, 'GREEN').     % Same color as o3
object_color(o12, 'YELLOW').    % Same color as o4
object_color(o13, 'ORANGE').    % Same color as o5
object_color(o14, 'PURPLE').    % Same color as o6
object_color(o15, 'CYAN').      % Same color as o7
object_color(o16, 'MAGENTA').   % Same color as o8

% ============================================
% GRID CONFIGURATION
% ============================================

% Define grid cell colors (4x4 grid)
% These are the target cells where objects can be placed
cell_color(0, 0, 'RED').
cell_color(0, 1, 'BLUE').
cell_color(0, 2, 'GREEN').
cell_color(0, 3, 'YELLOW').
cell_color(1, 0, 'ORANGE').
cell_color(1, 1, 'PURPLE').
cell_color(1, 2, 'CYAN').
cell_color(1, 3, 'MAGENTA').
cell_color(2, 0, 'RED').        % Same color as (0,0)
cell_color(2, 1, 'BLUE').       % Same color as (0,1)
cell_color(2, 2, 'GREEN').      % Same color as (0,2)
cell_color(2, 3, 'YELLOW').     % Same color as (0,3)
cell_color(3, 0, 'ORANGE').     % Same color as (1,0)
cell_color(3, 1, 'PURPLE').     % Same color as (1,1)
cell_color(3, 2, 'CYAN').       % Same color as (1,2)
cell_color(3, 3, 'MAGENTA').    % Same color as (1,3)

% ============================================
% EXAMPLE STATES & TRANSITIONS
% ============================================

% Define example states for testing
% Initial state (all objects outside grid)
my_state(state0).

% First row placements
my_state(state1) :- 
    my_state(state(place(o1, 0, 0), state0)).

my_state(state2) :- 
    my_state(state(place(o2, 0, 1), state1)).

my_state(state3) :- 
    my_state(state(place(o3, 0, 2), state2)).

my_state(state4) :- 
    my_state(state(place(o4, 0, 3), state3)).

% Alternative placements (same color objects in different cells)
my_state(alt_state1) :- 
    my_state(state(place(o1, 2, 0), state0)).  % o1 in red cell at (2,0) instead of (0,0)

my_state(alt_state2) :- 
    my_state(state(place(o9, 0, 0), state0)).  % o9 in red cell at (0,0) instead of o1

% ============================================
% ADDITIONAL TEST CASES
% ============================================

% Positive examples for testing - different objects in matching cells
test_pos(valid_place_action(o1, 0, 0, state0)).  % o1(red) in red cell
test_pos(valid_place_action(o1, 2, 0, state0)).  % o1(red) in alternative red cell
test_pos(valid_place_action(o9, 0, 0, state0)).  % o9(red) in red cell (same color as o1)

% Test cases showing multiple placement options
test_pos(valid_place_action(o2, 0, 1, state0)).  % o2(blue) in blue cell (0,1)
test_pos(valid_place_action(o2, 2, 1, state0)).  % o2(blue) in blue cell (2,1)
test_pos(valid_place_action(o10, 0, 1, state0)). % o10(blue) in blue cell (0,1)
test_pos(valid_place_action(o10, 2, 1, state0)). % o10(blue) in blue cell (2,1)

% Sequential placements
test_pos(valid_place_action(o2, 0, 1, state(place(o1, 0, 0), state0))).  
test_pos(valid_place_action(o3, 0, 2, state(place(o2, 0, 1), state(place(o1, 0, 0), state0)))).

% Negative examples - color mismatches
test_neg(valid_place_action(o1, 0, 1, state0)).  % Color mismatch: red object in blue cell
test_neg(valid_place_action(o2, 0, 0, state0)).  % Color mismatch: blue object in red cell

% Negative examples - already placed objects
test_neg(valid_place_action(o1, 2, 0, state(place(o1, 0, 0), state0))). % Object o1 already placed
test_neg(valid_place_action(o9, 0, 0, state(place(o1, 0, 0), state0))). % Cell already occupied

% Out of bounds
test_neg(valid_place_action(o1, 4, 4, state0)).  % Out of bounds coordinates

% ============================================
% 4x4 Grid Template
% ============================================
% This file serves as a template for 4x4 grid layouts.
% The actual game now uses random colors.

% Example grid layout (for reference only)
example_grid([
    [yellow, blue, orange, red],
    [magenta, yellow, blue, orange],
    [red, magenta, yellow, blue],
    [orange, red, magenta, yellow]
]).

% Example object colors (for reference only)
example_objects([
    o1-yellow,
    o2-blue,
    o3-orange,
    o4-red,
    o5-magenta,
    o6-yellow,
    o7-blue,
    o8-orange,
    o9-red,
    o10-magenta,
    o11-yellow,
    o12-blue,
    o13-orange,
    o14-red,
    o15-magenta,
    o16-yellow
]).

% Example valid moves (for reference only)
example_moves([
    place(o1, 0, 0),   % Place yellow block on yellow cell
    place(o2, 0, 1),   % Place blue block on blue cell
    place(o3, 0, 2),   % Place orange block on orange cell
    place(o4, 0, 3),   % Place red block on red cell
    place(o5, 1, 0),   % Place magenta block on magenta cell
    place(o6, 1, 1),   % Place yellow block on yellow cell
    place(o7, 1, 2),   % Place blue block on blue cell
    place(o8, 1, 3),   % Place orange block on orange cell
    place(o9, 2, 0),   % Place red block on red cell
    place(o10, 2, 1),  % Place magenta block on magenta cell
    place(o11, 2, 2),  % Place yellow block on yellow cell
    place(o12, 2, 3),  % Place blue block on blue cell
    place(o13, 3, 0),  % Place orange block on orange cell
    place(o14, 3, 1),  % Place red block on red cell
    place(o15, 3, 2),  % Place magenta block on magenta cell
    place(o16, 3, 3)   % Place yellow block on yellow cell
]). 