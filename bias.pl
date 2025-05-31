head_pred(valid_place_action,4).
head_pred(grab,2).
head_pred(release,2).
body_pred(object_color,2).
body_pred(select,3).
body_pred(member,2).
body_pred(object,1).
body_pred(cell,3).
body_pred(color,1).
body_pred(cell/1,1).

% Types
type(valid_place_action,(object,coord,coord,state)).
type(object_color,(object,color)).
type(cell,(coord,coord,color)).
type(object_available,(object,state)).
type(cell_available,(coord,coord,state)).
type(grab,(state,state)).
type(release,(state,state)).
type(state,(any)).
type(object,(object)).
type(color,(color)).

% Directions
direction(valid_place_action,(in,in,in,in)).
direction(object_color,(in,out)).
direction(cell,(in,in,out)).
direction(object_available,(in,in)).
direction(cell_available,(in,in,in)).
direction(grab,(in,out)).
direction(release,(in,out)).
direction(select,(in,in,out)).
direction(member,(in,in)).
direction(object,(in)).
direction(color,(in)).

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