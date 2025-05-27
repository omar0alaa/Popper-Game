head_pred(valid_place_action,4).
body_pred(object_color,2).
body_pred(cell,3).
body_pred(object_available,2).
body_pred(cell_available,3).

% Types
type(valid_place_action,(object,coord,coord,state)).
type(object_color,(object,color)).
type(cell,(coord,coord,color)).
type(object_available,(object,state)).
type(cell_available,(coord,coord,state)).

% Directions
direction(valid_place_action,(in,in,in,in)).
direction(object_color,(in,out)).
direction(cell,(in,in,out)).
direction(object_available,(in,in)).
direction(cell_available,(in,in,in)).

% Constants
object(o1).
object(o2).
object(o3).
object(o4).
coord(0).
coord(1).
color(red).
color(blue).
color(green).
color(yellow).

max_vars(6).
max_body(6).