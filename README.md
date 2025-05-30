# Blütentanz
#### Group Blütentanz_7

| Members | Up | Participations |
|---------|----|----------------|
|Dinis Afonso Cerqueira Galvão | up202207217| 50% |
|Joana Luís Sá Marques Pimenta | up202206120 | 50% |

#### Participations
In this project, the tasks were well divided since both members had the opportunity to work together on all the tasks. To ease the distribution of the work, we decided to make one of the members responsible for every task, even if we work on them together.

Dinis was more responsible for Board.pl, display and visuals, turn and rotation in the game and the Bot modes in the game, this includes functions like display_game(+GameState), value(+GameState, +Player, -Value) and choose_move(+GameState, +Level, -Move).

Joana was more responsible for Move.pl, Menu.pl, Game Over and Settings of game configurations, this includes functions like initial_state(+GameConfig, -GameState), move(+Gamestate, +Move,-NewGameState), valid_moves(+GameState,-ListOfMoves) and game_over(+GameState, -Winner).
#### Execution
After having installed SICStus Prolog 4.9 and entered this project, to execute you just need to reach the `/src` directory and run `sicstus` then `[game].`  to compile the code and `play.` to run it.

#### Description of the game
In this game, you play on a board composed by rows and columns of tiles (4 by 4 in the original game). These Tiles have 4 different positions, one for each quarter with 3 different colors (gray, blue, orange) and one empty. At the start of each of your turns, you have to rotate all the Tiles in a row or column 90 degrees clockwise, and then you can start moving your pieces. In a turn, you can move up to 3 different times but only rotate once, and this rotation must be at the start of the turn. To move, you can only go to 1 space up, down, left or right in the board if the color of the space is the same as your piece or gray. To win, you must reach a certain number of pieces (4 out of 5 in the original version) to the opposite side of the board until they leave the board. The first player to reach their 4 pieces out of the board on the opposing side wins.

![image](image_board.png)

#### Links for Rules
1 - https://boardgamegeek.com/boardgame/428363/blutentanz

#### Considerations for game extensions
We changed the original game so players can decide settings like board size, number of pieces and pieces necessary to win. If the player does not choose any setting, the game will just play with the original settings. The limitations in choosing your setting are that in board size, the width and height must be lower than 9, so the board doesn't get too big, the maximum number of pieces per player should be less or equal to the double of the Width of the Board, and the pieces to win cannot be greater than the number of pieces per player. These limitations aim to keep the game playable in any scenario. If any invalid input is inserted in the settings, then the default settings can also be automatically chosen. In addition, we also have the option to set the board to have the width different from the height, which is different from the original game.

#### Game Logic
#### Game Configuration
  
In the Menu.pl, we have a `default_config(config([5, 5], [4, 4], [human, human], [4, 4])). ` that defines the default configuration, which can be changed in the settings menu. The function main_menu outputs the final configuration (default or not) to the Game.pl.
Here, that configuration will be initialized by initial_state/2 before the game starts. Our initial_state will receive a configuration like `config([Player1PiecesCount,Player2PiecesCount],[Rows, Columns], [Player1Type, Player2Type],PiecesToWin)` that defines the Number of Pieces per player, the Board Size, PlayerType and PiecesToWin. Player type isn't decided in the settings. Instead, there is a menu where the player can decide which type of game to play, and that will change the player type. Player types can be `human`, `dumbbot` and `smartbot`. The output of initial_state is the GameState. 

#### GameState

A GameState is `state(Board, Players, CurrentPlayer, PiecesToWin)`. In a GameState the Board is represented by a matrix of Tiles, in initial_state the Tiles are randomized, and the matrix follows the desired size. A tile is represented by `tile([empty,orange,blue,gray],rotation)` where the rotation can be 0, 90, 180 or 270 degrees. Here is an example of a Board:
```
[[tile([empty,orange,blue,gray],0),tile([empty,orange,blue,gray],270),tile([empty,orange,blue,gray],90),tile([empty,orange,blue,gray],270)],
[tile([empty,orange,blue,gray],0),tile([empty,orange,blue,gray],0),tile([empty,orange,blue,gray],0),tile([empty,orange,blue,gray],0)],
[tile([empty,orange,blue,gray],0),tile([empty,orange,blue,gray],0),tile([empty,orange,blue,gray],180),tile([empty,orange,blue,gray],0)],
[tile([empty,orange,blue,gray],180),tile([empty,orange,blue,gray],0),tile([empty,orange,blue,gray],90),tile([empty,orange,blue,gray],270)]]
```
In a GameState, Players are represented by a list `[player( player1 , PlayerType, color, PlayerPieces),player( player2 , PlayerType, color, PlayerPieces)]`. Players have their player number, their type (`human`, `dumbbot` and `smartbot`), the color, for player1 is always orange and for player2 it is blue, and their pieces. PlayerPieces are represented by a list of the player's pieces. A piece is `piece(Position,Id)`. 

In a GameState, CurrentPlayer is just the player( playerNum , PlayerType, color, PlayerPieces) of the player who is currently playing it's turn. At initialization, the current player is always orange (player1) and so orange always starts first.

Finally, in GameState we also have PiecesToWin, which is simply a list of the number of pieces necessary to win, for example, `[5,5]` means player 1 needs to score 5 as well as player 2.

#### Move Representation

In Move.pl, we can check all functions related to movements in the game.
Here we have a function `display_possible_moves(Round,GameState,NewGameState, NewRound) ` that is called in Game.pl and that makes sure to receive the input needed, Round (used to control how many moves are left (1, 2 or 3))  and GameState, and returns NewGameState and NewRound. This function will call all the other functions that make sure we are able to move pieces around, functions like `valid_move/2` and `move/3`. 

In `move(+GameState, +Move, -NewGameState)` Move is a `(Idx, Piece)` where Idx is the Index in which the Piece is inserted in the list of the Player Pieces. This value (Idx) is the same as the Piece Id since the Pieces are ordered. Piece is of type `piece(Position,Id)` where position is `[[BoardX,BoardY],[TileX,TileY]] `, for example, the lower left space in the lower left tile of the board is `[[1,1],[1,1]]`. 

In `move/2` we take the selected move made `Move` and update the position of that Piece in the Board by changing the previous values of that Piece in PlayerPieces to the new value when creating a NewGameState `state(Board, [player( player1 , PlayerType, color, PlayerPieces),player( player2 , PlayerType, color, PlayerPieces)], CurrentPlayer, PiecesToWin)`. We have to change the moved piece in the list PlayerPieces in the CurrentPlayer for the desired value in Move, and then change the CurrentPlayer in the PlayersList, so both places can have the updated version of that player. By making these changes to the GameState we get the NewGameState.

#### User Interaction

We start with a Main Menu where the user can select numbered options (1. Start Game. 2. Settings, 3. Exit). It is not mandatory to go to Settings. Without going to this Menu the game will start with default values, but, if the user goes to the Settings, a phrase explaining the first input expected (Board Size) will show up, and if the player enters a valid input, for example `[5,5].` or chooses `d.` for default, the Board Size will be assigned. If the input is invalid, a message will show, and the default will be selected.

Then the following settings will show up one by one and wait for a number input, one number for the number of pieces per player and then one for the number of pieces to win. If the inputs are wrong, it will allow new tries, and `d.` still works to select default values.

After writing the valid input to the last setting (Pieces to Win), the user will automatically be directed to the Main Menu. If he wants, he can still change Settings again.

If the user selects 1. Start Game by entering input 1. he will be directed to a different Menu. In this Menu he will have to select from the following options (1. Human vs Human, 2. Human vs Bot, 3. Bot vs Bot), if 1. is selected, the last configuration `PlayerType` is defined `[human,human]` and the game will start. If other options are selected a New Menu will show up, so the user can choose the bot difficulty level (dumb or smart), and after that the game will start.

In the game, the inputs taken are: first, the number of the row, or, letter of the column, the player wants to rotate, and after that, the number of the piece the player wants to move. After choosing a piece to move, numbered options of the possible moves will show up, and the player will choose a number to select a movement, and the piece will move. In those numbered options, the last one will let you choose a different piece instead, so you can see the moves that piece has and move it. When selecting a piece, the player also has the option to write p. to end its turn and pass it to the next player. If all the maximum moves per turn (3 moves) are done without passing, then the game will automatically end the turn and pass it to the next player.

#### Conclusions

In this project, we were able to successfully implement all of the Blütentanz game logic and have dynamic settings for a configurable game experience. We also were able to have different modes (Human vs. Human, Human vs. Bot, Bot vs. Bot) and more than one level of difficulty for the bots.

In this project, the biggest difficulty we found was the inability to use the arrows as input on Windows using only Prolog, since those were our desired keys to manipulate the game and menus. We also wanted to improve the display of the game, which was a bit challenging using Prolog, but we made it work.

#### Bibliography

We used the tool ChatGPT to help us identify different libraries we could use and, if those libraries weren't available, to help us create functions with the same effect. Some cases in our project of these uses of ChatGPT are functions like busy_wait and some functions in List.pl.
We also used this tool to help us with errors and debug.





  







