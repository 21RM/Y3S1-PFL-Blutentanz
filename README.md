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
