head_pred(grab,2).
head_pred(release,2).
body_pred(select,3).
body_pred(member,2).
body_pred(object,1).
body_pred(cell,3).
body_pred(color,1).
body_pred(cell/1,1).

% Types
type(grab,(state,state)).
type(release,(state,state)).
type(state,(any)).
type(object,(object)).
type(cell,(coord,coord,color)).
type(color,(color)).

% Directions
direction(grab,(in,out)).
direction(release,(in,out)).
direction(select,(in,in,out)).
direction(member,(in,in)).
direction(object,(in)).
direction(cell,(in,in,out)).
direction(color,(in)).

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