% Language bias for Popper to learn color matching game rules

% Predicate declarations
head_pred(valid_move,3).

% Direction declarations
direction(valid_move,(in,in,in)).
direction(color,(in)).

% Constants
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

% Bias settings
max_vars(6).
max_body(6).