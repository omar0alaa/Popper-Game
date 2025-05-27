% ============================================
% Examples for Color Matching Game
% ============================================
% This file contains positive and negative examples
% of valid moves in the Color Matching Game.

% Positive examples: valid placements (object and cell colors match, both available)
pos(valid_place_action(o1,0,0,[])).
pos(valid_place_action(o2,0,1,[])).
pos(valid_place_action(o3,1,0,[])).
pos(valid_place_action(o4,1,1,[])).

% Negative examples: color mismatch, object/cell not available
neg(valid_place_action(o1,0,1,[])). % o1 is red, cell is blue
neg(valid_place_action(o2,1,0,[])). % o2 is blue, cell is green
neg(valid_place_action(o3,1,1,[])). % o3 is green, cell is yellow
neg(valid_place_action(o4,0,0,[])). % o4 is yellow, cell is red

% Negative: object already placed
neg(valid_place_action(o1,0,0,[placed(o1,0,0)])).

% Negative: cell already occupied
neg(valid_place_action(o2,0,1,[placed(_,0,1)])).