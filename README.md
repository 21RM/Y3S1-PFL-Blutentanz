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
In this game, you play in a board composed by rows and columns of tiles (5 by 5 in the original game). These Tiles have 4 diffent position, one for each quarter with 3 different colors (grey,blue,orange) and one empty. At the start of each of your turns you have to rotate all of the Tiles in a row or column 90 degrees clockwise, and then you can start moving your pieces. In a turn you can move up to 3 different times but only rotate once, and this rotation must be at the start of the turn. To move, you can only go to 1 space up, down, left or right in the board if the color of the space is the same as your piece or grey. To win you must reach a certain number of pieces (4 out of 5 in the original version) to the opposite side of the board until they leave the board. The first player to reach their 4 pieces out of the board on the opposing side wins.

![image](image_board.png)

#### Considerations
We changed the original game so players can decide settings like board size, number of pieces and pieces necessary to win. If the player does not choose any setting the game will just play with the original settings. The limitations in chosing your setting is that in board size width and height must be lower then 9, so the board doesn't get to big, the maximum number of pieces per player should be less or equal to the double of the Width of the Board, and the pieces to win cannot be greater then the number of pieces per player. These limitations aim to keep the game playable in any scenario, if any invalid input is inserted in the settings then the default settings can also be automatically chosen. In addition we also have the option to set the board to have the width different from the height, which is different from the original game.

#### Links for Rules
1 - https://boardgamegeek.com/boardgame/428363/blutentanz



