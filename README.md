# Blutentanz_7
#### Group

| Members | Up | Participations |
|---------|----|----------------|
|Dinis Afonso Cerqueira Galvão | up202207217| 50% |
|Joana Luís Sá Marques Pimenta | up202206120 | 50% |

#### Participations
In this project the tasks were well divided since both members had the opportunity to work togetther in all the tasks. To ease the distribution of the work we decided to make one of the members the responsible of every task, even if we work on them together.

Dinis was more responsible for Board.pl, display and visuals, turn and rotation in game and the Bot modes in the game, this include functions like display_game(+GameState), value(+GameState, +Player, -Value) and choose_move(+GameState, +Level, -Move).

Joana was more responsible for Move.pl, Menu.pl, Game Over and Settings of game configurations, this include functions like initial_state(+GameConfig, -GameState), move(+Gamestate, +Move,-NewGameState), valid_moves(+GameState,-ListOfMoves) and game_over(+GameState, -Winner).

#### Execution
After having installed the SICStus Prolog 4.9 and entered this project, to execute you just need reach the `/src` directory and run `[game].`  to compile the code and `play.` to run it.

#### Description of the game
In this game, you play in a board composed by rows and columns of tiles, (5 by 5 in the original game). These Tiles have 4 diffent position, one for each quarter with 3 different colors (grey,blue,orange) and one empty. At the start of each of your turns you have to rotate all of the Tiles in row or column and then you can start moving your pieces. In a turn you can move up to 3 different times but only rotate once, and this rotation must be at the start of the turn. To win you must reach a certain number of pieces (4 out of 5 in the original version) to the opposite side of the board until they leave the board. The first player to reach their 4 pieces out of the board on the opposing side wins.

![image](image_board.png)


