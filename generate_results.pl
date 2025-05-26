% ============================================
% Results Generation for Color Matching Game
% ============================================
% This file generates an HTML visualization of the game state.
% It creates a 3D interactive visualization of the game grid,
% available blocks, and current score.

:- use_module(library(http/html_write)).

% Generate results HTML file
% Creates and writes the HTML file to results/game_results.html
generate_results :-
    open('results/game_results.html', write, Stream),
    write_html(Stream),
    close(Stream).

% Write the complete HTML structure
% Organizes the HTML document into head, styles, and body sections
write_html(Stream) :-
    html_begin(Stream),
    write_html_head(Stream),
    write_css_styles(Stream),
    write_html_body(Stream),
    html_end(Stream).

% Write HTML head section
% Creates the document head with meta tags and title
write_html_head(Stream) :-
    format(Stream, '<!DOCTYPE html>~n', []),
    format(Stream, '<html lang="en">~n', []),
    format(Stream, '<head>~n', []),
    format(Stream, '    <meta charset="UTF-8">~n', []),
    format(Stream, '    <meta name="viewport" content="width=device-width, initial-scale=1.0">~n', []),
    format(Stream, '    <title>Color Matching Game Results</title>~n', []),
    format(Stream, '</head>~n', []).

% Write CSS styles
% Defines all the styling for the game visualization
write_css_styles(Stream) :-
    format(Stream, '<style>~n', []),
    % Global styles
    format(Stream, '    * { margin: 0; padding: 0; box-sizing: border-box; }~n', []),
    format(Stream, '    body { font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif; }~n', []),
    % Game container - Main wrapper for the game interface
    format(Stream, '    .game-container {~n', []),
    format(Stream, '        display: flex;~n', []),
    format(Stream, '        justify-content: space-around;~n', []),
    format(Stream, '        align-items: center;~n', []),
    format(Stream, '        padding: 40px;~n', []),
    format(Stream, '        background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);~n', []),
    format(Stream, '        min-height: 100vh;~n', []),
    format(Stream, '        margin: 0;~n', []),
    format(Stream, '    }~n', []),
    % Grid section - Container for the game grid
    format(Stream, '    .grid-section {~n', []),
    format(Stream, '        background: rgba(255, 255, 255, 0.1);~n', []),
    format(Stream, '        padding: 30px;~n', []),
    format(Stream, '        border-radius: 20px;~n', []),
    format(Stream, '        box-shadow: 0 10px 30px rgba(0,0,0,0.3);~n', []),
    format(Stream, '        backdrop-filter: blur(10px);~n', []),
    format(Stream, '        border: 1px solid rgba(255,255,255,0.1);~n', []),
    format(Stream, '    }~n', []),
    % Grid - The actual game grid container
    format(Stream, '    .grid {~n', []),
    format(Stream, '        display: grid;~n', []),
    format(Stream, '        gap: 15px;~n', []),
    format(Stream, '        padding: 20px;~n', []),
    format(Stream, '        background: rgba(0,0,0,0.2);~n', []),
    format(Stream, '        border-radius: 15px;~n', []),
    format(Stream, '        perspective: 1000px;~n', []),
    format(Stream, '    }~n', []),
    % Cell - Individual grid cells
    format(Stream, '    .cell {~n', []),
    format(Stream, '        width: 80px;~n', []),
    format(Stream, '        height: 80px;~n', []),
    format(Stream, '        transform-style: preserve-3d;~n', []),
    format(Stream, '        transition: transform 0.6s cubic-bezier(0.4, 0, 0.2, 1);~n', []),
    format(Stream, '        cursor: pointer;~n', []),
    format(Stream, '        position: relative;~n', []),
    format(Stream, '    }~n', []),
    % Cube faces - The six faces of each 3D cube
    format(Stream, '    .cube-face {~n', []),
    format(Stream, '        position: absolute;~n', []),
    format(Stream, '        width: 100%;~n', []),
    format(Stream, '        height: 100%;~n', []),
    format(Stream, '        border: 2px solid rgba(255,255,255,0.2);~n', []),
    format(Stream, '        border-radius: 10px;~n', []),
    format(Stream, '        display: flex;~n', []),
    format(Stream, '        align-items: center;~n', []),
    format(Stream, '        justify-content: center;~n', []),
    format(Stream, '        font-weight: bold;~n', []),
    format(Stream, '        color: white;~n', []),
    format(Stream, '        text-shadow: 1px 1px 2px rgba(0,0,0,0.5);~n', []),
    format(Stream, '        box-shadow: inset 0 0 20px rgba(0,0,0,0.2);~n', []),
    format(Stream, '    }~n', []),
    % Cube face positions - 3D positioning for each face
    format(Stream, '    .front { transform: translateZ(40px); }~n', []),
    format(Stream, '    .back { transform: rotateY(180deg) translateZ(40px); }~n', []),
    format(Stream, '    .right { transform: rotateY(90deg) translateZ(40px); }~n', []),
    format(Stream, '    .left { transform: rotateY(-90deg) translateZ(40px); }~n', []),
    format(Stream, '    .top { transform: rotateX(90deg) translateZ(40px); }~n', []),
    format(Stream, '    .bottom { transform: rotateX(-90deg) translateZ(40px); }~n', []),
    % Blocks section - Container for available blocks
    format(Stream, '    .blocks-section {~n', []),
    format(Stream, '        background: rgba(255, 255, 255, 0.1);~n', []),
    format(Stream, '        padding: 30px;~n', []),
    format(Stream, '        border-radius: 20px;~n', []),
    format(Stream, '        box-shadow: 0 10px 30px rgba(0,0,0,0.3);~n', []),
    format(Stream, '        backdrop-filter: blur(10px);~n', []),
    format(Stream, '        border: 1px solid rgba(255,255,255,0.1);~n', []),
    format(Stream, '    }~n', []),
    % Blocks container - Grid for available blocks
    format(Stream, '    .blocks {~n', []),
    format(Stream, '        display: flex;~n', []),
    format(Stream, '        flex-wrap: wrap;~n', []),
    format(Stream, '        gap: 15px;~n', []),
    format(Stream, '        justify-content: center;~n', []),
    format(Stream, '        padding: 20px;~n', []),
    format(Stream, '        background: rgba(0,0,0,0.2);~n', []),
    format(Stream, '        border-radius: 15px;~n', []),
    format(Stream, '    }~n', []),
    % Block - Individual available blocks
    format(Stream, '    .block {~n', []),
    format(Stream, '        width: 70px;~n', []),
    format(Stream, '        height: 70px;~n', []),
    format(Stream, '        transform-style: preserve-3d;~n', []),
    format(Stream, '        transition: transform 0.6s cubic-bezier(0.4, 0, 0.2, 1);~n', []),
    format(Stream, '        cursor: grab;~n', []),
    format(Stream, '        position: relative;~n', []),
    format(Stream, '    }~n', []),
    % Score section - Container for score display
    format(Stream, '    .score-section {~n', []),
    format(Stream, '        background: rgba(255, 255, 255, 0.1);~n', []),
    format(Stream, '        padding: 30px;~n', []),
    format(Stream, '        border-radius: 20px;~n', []),
    format(Stream, '        box-shadow: 0 10px 30px rgba(0,0,0,0.3);~n', []),
    format(Stream, '        backdrop-filter: blur(10px);~n', []),
    format(Stream, '        border: 1px solid rgba(255,255,255,0.1);~n', []),
    format(Stream, '        text-align: center;~n', []),
    format(Stream, '        color: white;~n', []),
    format(Stream, '    }~n', []),
    % Score display - The actual score number
    format(Stream, '    .score {~n', []),
    format(Stream, '        font-size: 48px;~n', []),
    format(Stream, '        font-weight: bold;~n', []),
    format(Stream, '        text-shadow: 0 0 10px rgba(255,255,255,0.3);~n', []),
    format(Stream, '        margin: 20px 0;~n', []),
    format(Stream, '    }~n', []),
    % Section titles - Headers for each section
    format(Stream, '    h2 {~n', []),
    format(Stream, '        color: white;~n', []),
    format(Stream, '        font-size: 24px;~n', []),
    format(Stream, '        margin-bottom: 20px;~n', []),
    format(Stream, '        text-align: center;~n', []),
    format(Stream, '        text-shadow: 0 2px 4px rgba(0,0,0,0.3);~n', []),
    format(Stream, '    }~n', []),
    % Color classes with gradients - Different colors for blocks
    format(Stream, '    .yellow { background: linear-gradient(135deg, #f1c40f 0%, #f39c12 100%); }~n', []),
    format(Stream, '    .blue { background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); }~n', []),
    format(Stream, '    .orange { background: linear-gradient(135deg, #e67e22 0%, #d35400 100%); }~n', []),
    format(Stream, '    .red { background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%); }~n', []),
    format(Stream, '    .magenta { background: linear-gradient(135deg, #9b59b6 0%, #8e44ad 100%); }~n', []),
    format(Stream, '    .placed { background: linear-gradient(135deg, #2ecc71 0%, #27ae60 100%); }~n', []),
    format(Stream, '</style>~n', []).

% Write HTML body
% Creates the main content of the game visualization
write_html_body(Stream) :-
    format(Stream, '<body>~n', []),
    format(Stream, '    <div class="game-container">~n', []),
    write_grid_section(Stream),
    write_blocks_section(Stream),
    write_score_section(Stream),
    format(Stream, '    </div>~n', []),
    write_3d_script(Stream),
    format(Stream, '</body>~n', []).

% Write grid section
% Creates the game grid with all cells
write_grid_section(Stream) :-
    format(Stream, '        <div class="grid-section">~n', []),
    format(Stream, '            <h2 style="color: white;">Game Grid</h2>~n', []),
    format(Stream, '            <div class="grid">~n', []),
    forall(cell(X, Y, Color),
           (format(Stream, '                <div class="cell" data-x="~w" data-y="~w">~n', [X, Y]),
            write_cube_faces(Stream, Color),
            format(Stream, '                </div>~n', []))),
    format(Stream, '            </div>~n', []),
    format(Stream, '        </div>~n', []).

% Write cube faces
% Creates the six faces of each 3D cube
write_cube_faces(Stream, Color) :-
    format(Stream, '                    <div class="cube-face front ~w"></div>~n', [Color]),
    format(Stream, '                    <div class="cube-face back ~w"></div>~n', [Color]),
    format(Stream, '                    <div class="cube-face right ~w"></div>~n', [Color]),
    format(Stream, '                    <div class="cube-face left ~w"></div>~n', [Color]),
    format(Stream, '                    <div class="cube-face top ~w"></div>~n', [Color]),
    format(Stream, '                    <div class="cube-face bottom ~w"></div>~n', [Color]).

% Write blocks section
% Creates the section showing available blocks
write_blocks_section(Stream) :-
    format(Stream, '        <div class="blocks-section">~n', []),
    format(Stream, '            <h2 style="color: white;">Available Blocks</h2>~n', []),
    format(Stream, '            <div class="blocks">~n', []),
    forall(block(BlockID, Color),
           (format(Stream, '                <div class="block" data-id="~w">~n', [BlockID]),
            write_cube_faces(Stream, Color),
            format(Stream, '                </div>~n', []))),
    format(Stream, '            </div>~n', []),
    format(Stream, '        </div>~n', []).

% Write score section
% Creates the section showing the current score
write_score_section(Stream) :-
    format(Stream, '        <div class="score-section">~n', []),
    score(Score),
    format(Stream, '            <h2>Score</h2>~n', []),
    format(Stream, '            <div class="score">~w</div>~n', [Score]),
    format(Stream, '        </div>~n', []).

% Write 3D rotation script
% Creates the JavaScript for 3D cube rotation
write_3d_script(Stream) :-
    format(Stream, '    <script>~n', []),
    format(Stream, '        document.querySelectorAll(".cell, .block").forEach(cube => {~n', []),
    format(Stream, '            let isDragging = false;~n', []),
    format(Stream, '            let startX, startY;~n', []),
    format(Stream, '            let currentRotation = { x: 0, y: 0 };~n', []),
    format(Stream, '~n', []),
    format(Stream, '            cube.addEventListener("mousedown", e => {~n', []),
    format(Stream, '                isDragging = true;~n', []),
    format(Stream, '                startX = e.clientX;~n', []),
    format(Stream, '                startY = e.clientY;~n', []),
    format(Stream, '                cube.style.cursor = "grabbing";~n', []),
    format(Stream, '            });~n', []),
    format(Stream, '~n', []),
    format(Stream, '            document.addEventListener("mousemove", e => {~n', []),
    format(Stream, '                if (!isDragging) return;~n', []),
    format(Stream, '~n', []),
    format(Stream, '                const deltaX = e.clientX - startX;~n', []),
    format(Stream, '                const deltaY = e.clientY - startY;~n', []),
    format(Stream, '~n', []),
    format(Stream, '                currentRotation.y += deltaX * 0.5;~n', []),
    format(Stream, '                currentRotation.x -= deltaY * 0.5;~n', []),
    format(Stream, '~n', []),
    format(Stream, '                cube.style.transform = `rotateX(${currentRotation.x}deg) rotateY(${currentRotation.y}deg)`;~n', []),
    format(Stream, '~n', []),
    format(Stream, '                startX = e.clientX;~n', []),
    format(Stream, '                startY = e.clientY;~n', []),
    format(Stream, '            });~n', []),
    format(Stream, '~n', []),
    format(Stream, '            document.addEventListener("mouseup", () => {~n', []),
    format(Stream, '                isDragging = false;~n', []),
    format(Stream, '                cube.style.cursor = "grab";~n', []),
    format(Stream, '            });~n', []),
    format(Stream, '~n', []),
    format(Stream, '            cube.addEventListener("mouseleave", () => {~n', []),
    format(Stream, '                if (!isDragging) {~n', []),
    format(Stream, '                    currentRotation = { x: 0, y: 0 };~n', []),
    format(Stream, '                    cube.style.transform = "rotateX(0) rotateY(0)";~n', []),
    format(Stream, '                }~n', []),
    format(Stream, '            });~n', []),
    format(Stream, '        });~n', []),
    format(Stream, '    </script>~n', []).

% HTML begin/end
% Empty predicates for HTML structure
html_begin(_).
html_end(_). 