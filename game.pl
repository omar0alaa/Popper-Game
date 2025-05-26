% ============================================
% Color Matching Game - Main Game Logic
% ============================================
% This file contains the main game logic for the color matching game.
% The game consists of a grid where players can place colored blocks
% on matching colored cells.

% Load required files
:- consult(bk).              % Load background knowledge (colors, rules)
:- consult(generate_results). % Load HTML visualization generator

% ============================================
% Dynamic Predicates Declaration
% ============================================
% These predicates can be modified during runtime
:- dynamic cell/3.    % cell(X, Y, Color) - Represents a cell at position (X,Y) with Color
:- dynamic block/2.   % block(BlockID, Color) - Represents a block with ID and Color
:- dynamic score/1.   % score(Points) - Tracks the current game score
:- dynamic grid_size/1. % grid_size(Size) - Stores the current grid size

% ============================================
% Game Initialization
% ============================================

% Initialize the game state
% This predicate:
% 1. Clears all existing game state
% 2. Prompts user for grid size
% 3. Initializes the grid and blocks
% 4. Displays the initial game state
init_game :-
    % Clear all existing game state
    retractall(cell(_, _, _)),    % Remove all cell facts
    retractall(block(_, _)),      % Remove all block facts
    retractall(score(_)),         % Remove score fact
    retractall(grid_size(_)),     % Remove grid size fact
    
    % Get grid size from user
    write('Choose grid size (2, 3, or 4): '), nl,
    read_line_to_string(user_input, String),
    atom_number(String, Size),
    
    % Validate and initialize game
    (member(Size, [2,3,4]) ->
        assertz(grid_size(Size)),     % Store grid size
        assertz(score(0)),            % Initialize score to 0
        init_grid(Size),              % Create the grid
        init_blocks(Size),            % Create the blocks
        display_game_state            % Show initial state
    ;
        % Invalid size - try again
        write('Invalid grid size. Please choose 2, 3, or 4.'), nl,
        init_game
    ).

% Initialize grid with random colors
% This predicate:
% 1. Creates a grid of specified size
% 2. Fills each cell with a random color
init_grid(Size) :-
    Max is Size - 1,                  % Calculate maximum index
    findall(Color, color(Color), Colors), % Get list of available colors
    length(Colors, NumColors),        % Count available colors
    % Fill each cell with a random color
    forall(between(0, Max, Y),        % For each row
           forall(between(0, Max, X),  % For each column
                  (random(0, NumColors, RandomIndex),  % Get random color index
                   nth0(RandomIndex, Colors, Color),   % Get color at index
                   assertz(cell(X, Y, Color))))).      % Create cell with color

% Initialize blocks with matching colors
% This predicate:
% 1. Creates blocks for each grid cell
% 2. Assigns colors to match the grid
init_blocks(Size) :-
    MaxBlocks is Size * Size,         % Calculate total number of blocks
    % Get all colors from the grid
    findall(Color, cell(_, _, Color), GridColors),
    % Shuffle the colors
    random_permutation(GridColors, ShuffledColors),
    % Create blocks with shuffled colors
    forall(between(1, MaxBlocks, N),
           (format(atom(BlockID), 'o~w', [N]),  % Create block ID (o1, o2, etc.)
            nth1(N, ShuffledColors, Color),     % Get color for this block
            assertz(block(BlockID, Color)))).   % Create block with color

% ============================================
% Display Functions
% ============================================

% Display the current game state
% Shows the grid, available blocks, and current score
display_game_state :-
    nl,
    write('Current Grid:'), nl,
    display_game_grid,                     % Show the grid
    nl,
    write('Blocks Outside Grid:'), nl,
    display_blocks,                   % Show available blocks
    nl,
    score(Score),
    format('Current Score: ~w~n', [Score]),  % Show current score
    nl.

% Display the grid
% Shows the current state of the grid with colors
display_game_grid :-
    grid_size(Size),
    Max is Size - 1,
    % Display each row
    forall(between(0, Max, Y),
           (nl,
            % Display each cell in the row
            forall(between(0, Max, X),
                   (cell(X, Y, Color) ->
                       write(Color), write('\t')  % Show cell color
                   ;
                       write('empty\t')          % Show empty cell
                   )))),
    nl.

% Display available blocks
% Shows all blocks that can be placed
display_blocks :-
    block(BlockID, Color),
    format('Block ~w: ~w~n', [BlockID, Color]),  % Show each block
    fail.  % Force backtracking to show all blocks
display_blocks.  % Succeed after showing all blocks

% ============================================
% Game Logic
% ============================================

% Place a block on the grid
% This predicate:
% 1. Validates the move
% 2. Updates the game state
% 3. Updates the score
% 4. Checks for game over
place_block(BlockID, X, Y) :-
    grid_size(Size),
    Max is Size - 1,
    % Validate coordinates
    X >= 0, X =< Max,
    Y >= 0, Y =< Max,
    % Get block color
    block(BlockID, Color),
    % Check if move is valid
    cell(X, Y, Color),
    \+ cell_occupied(X, Y),
    % Update game state
    retract(block(BlockID, _)),      % Remove block
    retract(cell(X, Y, _)),          % Remove old cell
    assertz(cell(X, Y, placed)),     % Mark cell as placed
    update_score,                    % Update score
    display_game_state,              % Show new state
    % Check for game over
    (check_game_over -> 
        announce_game_over,
        generate_results
    ;
        true
    ).

% Update the game score
% Increments the score by 1 for each successful move
update_score :-
    retract(score(OldScore)),
    NewScore is OldScore + 1,
    assertz(score(NewScore)).

% Check if game is over
% Game ends when:
% 1. No more blocks to place, or
% 2. No valid moves left
check_game_over :-
    \+ block(_, _).  % No more blocks to place
check_game_over :-
    block(_, _),  % There are blocks left
    \+ (block(BlockID, Color),
        cell(X, Y, Color),
        \+ cell_occupied(X, Y),
        valid_move(BlockID, X, Y)).  % But no valid moves

% Announce game over
% Shows final score and generates results
announce_game_over :-
    nl,
    write('Game Over!'), nl,
    score(FinalScore),
    format('Final Score: ~w~n', [FinalScore]),
    write('Results have been generated in results/game_results.html'), nl.

% ============================================
% Game Interaction
% ============================================

% Main game loop
% Starts the game and handles the main game loop
play :-
    write('Welcome to Color Matching Game!'), nl,
    init_game,
    \+ check_game_over,  % Make sure game isn't over after initialization
    game_loop.

% Game interaction loop
% Handles player moves until game over
game_loop :-
    (check_game_over ->
        announce_game_over,
        generate_results
    ;
        write('Enter your move:'), nl,
        % Get move details from player
        write('Object No = '),
        read_line_to_string(user_input, BlockIDStr),
        atom_string(BlockID, BlockIDStr),
        write('X = '),
        read_line_to_string(user_input, XStr),
        atom_number(XStr, X),
        write('Y = '),
        read_line_to_string(user_input, YStr),
        atom_number(YStr, Y),
        % Try to make the move
        (place_block(BlockID, X, Y) ->
            game_loop
        ;
            write('Invalid move! Try again.'), nl,
            game_loop
        )
    ).

% Predicate to validate moves
% Checks if a move is valid based on color matching
valid_move(BlockID, X, Y) :-
    block(BlockID, Color),
    cell(X, Y, Color),
    \+ cell_occupied(X, Y).

% Helper predicates for testing
% Creates a test grid with predefined colors
test_grid :-
    retractall(cell(_, _, _)),
    assertz(cell(1, 1, red)),
    assertz(cell(1, 2, red)),
    assertz(cell(2, 1, blue)),
    assertz(cell(2, 2, blue)),
    assertz(cell(3, 1, green)),
    assertz(cell(3, 2, yellow)),
    assertz(cell(1, 3, yellow)),
    assertz(cell(2, 3, green)),
    assertz(cell(3, 3, red)). 