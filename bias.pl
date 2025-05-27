head_pred(valid_move,3).
body_pred(valid_coord,2).
body_pred(cell_has_color,3).
body_pred(cell_occupied,2).
body_pred(block,2).
body_pred(color,1).

type(valid_move,(block_id,coord,coord)).
type(valid_coord,(coord,coord)).
type(cell_has_color,(coord,coord,color)).
type(cell_occupied,(coord,coord)).
type(block,(block_id,color)).
type(color,(color)).

direction(valid_move,(in,in,in)).
direction(valid_coord,(in,in)).
direction(cell_has_color,(in,in,in)).
direction(cell_occupied,(in,in)).
direction(block,(in,in)).
direction(color,(in)).

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

max_vars(6).
max_body(6).