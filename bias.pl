% Language bias for Popper to learn color matching game rules

% Mode declarations
:- modeh(1, valid_move(+block_id, +coord, +coord))?
:- modeb(1, valid_coord(+coord, +coord))?
:- modeb(1, cell_has_color(+coord, +coord, #color))?
:- modeb(1, cell_occupied(+coord, +coord))?
:- modeb(1, block(+block_id, #color))?
:- modeb(1, color(#color))?

% Type definitions
block_id(b1).
block_id(b2).
block_id(b3).
block_id(b4).
block_id(b5).

coord(0).
coord(1).
coord(2).

color(yellow).
color(blue).
color(orange).
color(red).
color(magenta).

% Example format 
:- begin_in.
valid_move(BlockID, X, Y) :-
    valid_coord(X, Y),
    block(BlockID, Color),
    cell(X, Y, Color),
    \+ cell_occupied(X, Y).
:- end_in.

% Positive examples
:- begin_pos.
valid_move(b1, 0, 0).  % Place yellow block on yellow cell
valid_move(b2, 0, 1).  % Place blue block on blue cell
valid_move(b3, 1, 1).  % Place orange block on orange cell
valid_move(b4, 1, 2).  % Place red block on red cell
valid_move(b5, 2, 0).  % Place magenta block on magenta cell
:- end_pos.

% Negative examples
:- begin_neg.
valid_move(b1, 1, 1).  % Try to place yellow block on orange cell
valid_move(b2, 2, 2).  % Try to place blue block on red cell
valid_move(b3, 0, 0).  % Try to place orange block on yellow cell
valid_move(b4, 2, 0).  % Try to place red block on magenta cell
valid_move(b5, 1, 2).  % Try to place magenta block on red cell
:- end_neg. 