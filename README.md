# Color Matching Game & Popper ILP Project

## Overview
This project demonstrates a simple color-matching placement game and uses [Popper](https://github.com/logic-and-learning-lab/Popper) (an Inductive Logic Programming system) to learn the rule for valid placements. 

The goal: **place n objects into n specific cells, where each object and cell has a color, and only matching colors are allowed.**

- **Game logic and background knowledge**: Prolog (`bk.pl`)
- **Learning task**: Popper with examples (`exs.pl`) and bias (`bias.pl`)
- **Interactive game**: Playable in SWI-Prolog (`game.pl`)

---

## Installation & Setup

### 1. Requirements
- [SWI-Prolog](https://www.swi-prolog.org/) (for running the game)
- [Python 3](https://www.python.org/) (for Popper)
- [Popper](https://github.com/logic-and-learning-lab/Popper) (see below)
- (Optional) [Docker](https://www.docker.com/) or [WSL](https://docs.microsoft.com/en-us/windows/wsl/) for Linux-based Popper runs

### 2. Clone Popper
```
git clone https://github.com/logic-and-learning-lab/Popper.git
```

### 3. Project Files
Place the following files in your working directory:
- `bk.pl` (background knowledge)
- `exs.pl` (examples)
- `bias.pl` (Popper bias)
- `game.pl` (interactive game)

---

## Running Popper (Learning the Rule)

1. **Open a terminal in your project directory.**
2. **Run Popper:**
   ```
   python ../popper.py .
   ```
   (Or use the full path to `popper.py` if needed.)

3. **Expected Output:**
   Popper will learn a rule like:
   ```prolog
   valid_place_action(O,X,Y,State) :-
       cell_available(X,Y,State),
       object_color(O,Color),
       cell(X,Y,Color).
   ```
   Precision and recall should be 1.00 if your examples are correct.

---

## Running the Interactive Game

1. **Open SWI-Prolog in your project directory:**
   ```
swipl
   ```
2. **Load the game:**
   ```prolog
   ['game.pl'].
   ```
3. **Start playing:**
   ```prolog
   play.
   ```
4. **Follow the prompts:**
   - Enter the object (e.g., `o1`).
   - Enter the X and Y coordinates (e.g., `0`, `1`).
   - The game will only allow valid moves (matching color, available cell/object).

---

## How It Works
- **Background knowledge** (`bk.pl`): Defines objects, cells, colors, and helper predicates for availability.
- **Examples** (`exs.pl`): Positive and negative examples of valid placements.
- **Bias** (`bias.pl`): Tells Popper what predicates and types to use.
- **Game** (`game.pl`): Lets you play the game interactively, using the same logic Popper learns.

---

## Customization
- To change the grid or objects, edit `bk.pl`.
- To add more test cases, edit `exs.pl`.
- To change the learning bias, edit `bias.pl`.

---

## Troubleshooting
- If you get `SIGALRM` errors, run Popper in a Linux environment (Docker, WSL, or Linux VM).
- If Popper can't find a solution, check your examples and bias for consistency.

---

## License
This project is for educational purposes. Popper is licensed under the MIT License.
