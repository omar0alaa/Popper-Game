% ============================================
% Examples for Color Matching Game
% ============================================
% This file contains positive and negative examples
% of valid moves in the Color Matching Game.

% Positive examples (valid moves)
pos(valid_move(b1,0,0)).  % Place yellow block on yellow cell
pos(valid_move(b2,0,1)).  % Place blue block on blue cell
pos(valid_move(b3,1,1)).  % Place orange block on orange cell
pos(valid_move(b4,1,2)).  % Place red block on red cell
pos(valid_move(b5,2,0)).  % Place magenta block on magenta cell

% Negative examples (invalid moves)
neg(valid_move(b1,1,1)).  % Try to place yellow block on orange cell
neg(valid_move(b2,2,2)).  % Try to place blue block on red cell
neg(valid_move(b3,0,0)).  % Try to place orange block on yellow cell
neg(valid_move(b4,2,0)).  % Try to place red block on magenta cell
neg(valid_move(b5,1,2)).  % Try to place magenta block on red cell

% Additional edge cases
neg(valid_move(b1,-1,0)).  % Invalid coordinates
neg(valid_move(b2,0,-1)).  % Invalid coordinates
neg(valid_move(b3,5,5)).   % Out of bounds
neg(valid_move(b6,0,0)).   % Non-existent block
% ============================================
% Examples for Color Matching Game
% ============================================
% This file contains positive and negative examples
% of valid moves in the Color Matching Game.

% Positive examples (valid moves)
pos(valid_move(b1,0,0)).  % Place yellow block on yellow cell
pos(valid_move(b2,0,1)).  % Place blue block on blue cell
pos(valid_move(b3,1,1)).  % Place orange block on orange cell
pos(valid_move(b4,1,2)).  % Place red block on red cell
pos(valid_move(b5,2,0)).  % Place magenta block on magenta cell

% Negative examples (invalid moves)
neg(valid_move(b1,1,1)).  % Try to place yellow block on orange cell
neg(valid_move(b2,2,2)).  % Try to place blue block on red cell
neg(valid_move(b3,0,0)).  % Try to place orange block on yellow cell
neg(valid_move(b4,2,0)).  % Try to place red block on magenta cell
neg(valid_move(b5,1,2)).  % Try to place magenta block on red cell

% Additional edge cases
neg(valid_move(b1,-1,0)).  % Invalid coordinates
neg(valid_move(b2,0,-1)).  % Invalid coordinates
neg(valid_move(b3,5,5)).   % Out of bounds
neg(valid_move(b6,0,0)).   % Non-existent block 