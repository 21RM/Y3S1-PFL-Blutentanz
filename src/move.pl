
:- module(move, [display_possible_moves/1]).

:- use_module('board.pl').
:- use_module('list.pl').
:- use_module('menu.pl').
:- use_module(library(lists), [reverse/2]).

%-----------------------------------------Display  moves-----------------------------------------------------

% Reads an integer, retrieves the sublist at that index, and prints the directions.
display_possible_moves(GameState) :-
    write('displaying possible moves...'), nl,
    valid_moves(GameState, ListOfMoves), % Get all valid moves
    write('Valid moves: '), write(ListOfMoves), nl,
    write('Enter the number of the piece you want to move: '),
    read(Index), % Read the integer index
    idx(Index, ListOfMoves, MovesForPiece), % Retrieve the sublist at the given index (1-based indexing)
    findall(Direction, member((Direction, _), MovesForPiece), Directions), % Extract directions
    display_menu_options(Directions, 1), % Display the directions
    write('Enter the number of the direction you want to move: '),
    read(DirectionIndex), % Read the direction index
    idx(DirectionIndex, MovesForPiece, (Direction, DestPosition)),
    Move = (Index,DestPosition), % Create the Move variable
    move(GameState, Move, NewGameState). % Move the piece

%------------------------------------------------------------------------------------------------------------

%---------------------------------Create GameState after motion----------------------------------------------

move(GameState, Move, NewGameState):-
    GameState = (Board, Players, CurrentPlayer),
    change_pieces(Move,CurrentPlayer,ListOfMoves, NewPlayer),
    change_players(Players, NewPlayer, NewPlayers),
    NewGameState = (Board, NewPlayers, NewPlayer).

change_players(Players, NewPlayer, NewPlayers):-
    NewPlayers = (Player1, Player2 ),
    NewPlayer = (PlayerNum, PlayerType, PlayerColor, PlayerPieces),
    PlayerColor = orange,
    NewPlayers = [NewPlayer, Player2 ].

change_players(Players, NewPlayer, NewPlayers):-
    NewPlayers = (Player1, Player2 ),
    NewPlayer = (PlayerNum, PlayerType, PlayerColor, PlayerPieces),
    PlayerColor = blue,
    NewPlayers = [Player1, NewPlayer ].

change_pieces(Move,CurrentPlayer,ListOfMoves, NewPlayer):-
    Move = (Index, Position),
    CurrentPlayer = (PlayerNum, PlayerType, PlayerColor, PlayerPieces),
    replace_index(Index, Position, PlayerPieces, NewPieces),
    NewPlayer=(PlayerNum, PlayerType, PlayerColor, NewPieces).

%-----------------------------------------------------------------------------------------------------------

%---------------------------------Calculate all possible moves----------------------------------------------

valid_moves(GameState, ListOfMoves) :-
    GameState = state(Board, Players, player(PlayerNum, PlayerType, Color, Pieces)), % Extract the pieces
    transform_board(Board, TransformedBoard), % Transform the board
    NewGameState = state(TransformedBoard, Players, player(PlayerNum, PlayerType, Color, Pieces)), % Create a new game state
    all_moves(NewGameState, Pieces, ListOfMoves).

% Iterates through the list of Pieces and calculates all possible moves with directions and destination indices.
all_moves(_, [], []). % Base case: No pieces left to process.
all_moves(GameState, [[[0, 0], [0, 0]] | RestPieces], [MovesForPiece | RemainingMoves]) :-

    possible_entry_position(GameState, EntryPositions), % Find valid entry positions
    can_place(GameState, EntryPositions, MovesForPiece).


    % Recurse for the rest of the pieces
    all_moves(GameState, RestPieces, RemainingMoves).
all_moves(GameState, [Piece | RestPieces], [MovesForPiece | RemainingMoves]) :-
    % Calculate moves in all four directions for the current piece
    findall(
        (Direction, DestPosition),
        (
            member(Direction, [up, down, left, right]), % Test each direction
            move_in_direction(Piece, Direction, DestPosition), % Calculate destination
            can_place(GameState, DestPosition) % Check if move is valid
        ),
        MovesForPiece
    ),
    % Recurse for the rest of the pieces
    all_moves(GameState, RestPieces, RemainingMoves).


% Calculates the new position based on the direction.
move_in_direction([[BoardX,BoardY],[TileX,TileY]], up, [[BoardX,NewBoardY],[TileX,NewTileY]]) :-
    NewTileY is (3 mod (TileY + 1) + 1),
    NewBoardY is BoardY -1 * (TileY mod 2 - 1).

move_in_direction([[BoardX,BoardY],[TileX,TileY]], down, [[BoardX,NewBoardY],[TileX,NewTileY]]) :-
    NewTileY is (3 mod (TileY + 1) + 1),
    NewBoardY is BoardY +1 * ((TileY+1) mod 2 - 1).

move_in_direction([[BoardX,BoardY],[TileX,TileY]], right, [[NewBoardX,BoardY],[NewTileX,TileY]]) :-
    NewTileX is (3 mod (TileX + 1) + 1),
    NewBoardX is BoardX -1 * (TileX mod 2 - 1).

move_in_direction([[BoardX,BoardY],[TileX,TileY]], left, [[NewBoardX,BoardY],[NewTileX,TileY]]) :-
    NewTileX is (3 mod (TileX + 1) + 1),
    NewBoardX is BoardX +1 * ((TileX+1) mod 2 - 1).

%---------------------------------------------------------------------------------------------------
%----------------------------------------Validation-------------------------------------------------

% Check if the destination position is valid for pieces that are off the board.
can_place(GameState, EntryPositions, MovesForPiece) :-
    findall(
        EntryPosition,
        (
            member(EntryPosition, EntryPositions), % Iterate through all entry positions
            can_move(GameState, EntryPosition)
          
        ),
        MovesForPiece % Collect valid entry positions
    ).
   

%check if the move is valid
can_move(GameState, DestInd) :-
    write('Entry position: '), write(DestInd), nl,
    GameState = state(Board, Players, player(_, _, PlayerColor, _)),
    get_dest_color(Board, DestInd, DestColor),
    validate_color(PlayerColor,DestColor),
    check_for_pieces(Players,DestInd),
    check_board_limits(Board,PlayerColor,DestInd).

validate_color(PlayerColor,DestColor) :-
    PlayerColor = orange,
    DestColor \= blue,
    DestColor \= empty,
    write('Player color: '), write(PlayerColor), write(' Destination color: '), write(DestColor), nl.
validate_color(PlayerColor,DestPiece) :-
    PlayerColor = blue,
    DestColor \= orange,
    DestColor \= empty.

%check if there are pieces in the way  
check_for_pieces(Players, Dest):-
    get_Player_Pieces(Players, 1, Player1Pieces),
    get_Player_Pieces(Players, 2, Player2Pieces),
    append(Player1Pieces, Player2Pieces, AllPieces),
    \+ member(Dest, AllPieces).

%check for board limits according to the player
check_board_limits(Board, PlayerColor, [[X,Y],TileCord]) :-
    length(Board, Height), 
    idx(1, Board, FirstRow),
    length(FirstRow, Width),
    X >= 0,
    Y >= 0,
    X =< Width,
    check_color_limitis(Height,PlayerColor,Y).

check_color_limitis(Height,PlayerColor,Y):-
    PlayerColor=orange,
    Y > 0,
    Y =< Height+1.
check_color_limitis(Height,PlayerColor,Y):-
    PlayerColor=blue,
    Y >= 0,
    Y =< Height.
%----------------------------------------------------------------------------------------
%---------------------------------Getters------------------------------------------------
get_Player_Pieces(Players, PlayerNum, Pieces) :-
    idx(PlayerNum, Players, Player),
    Player = player(_, _, _, Pieces). 

get_dest_color(Board, [[BoardX,BoardY],[TileX, TileY]], DestColor):-
    idx(BoardY, Board, DestLine),
    idx(BoardX, DestLine, DestTile),
    get_color( TileX, TileY, ColorInd),
    DestTile = tile(Sides, Rotation),
    rotated_sides(Sides, Rotation, RotatedSides),
    idx(ColorInd, RotatedSides, DestColor).

get_color( TileX, TileY, ColorInd):-
    TileX=1,
    TileY=1,
    ColorInd=3.
get_color( TileX, TileY, ColorInd):-
    TileX=1,
    TileY=2,
    ColorInd=1.
get_color( TileX, TileY, ColorInd):-
    TileX=2,
    TileY=1,
    ColorInd=4.
get_color( TileX, TileY, ColorInd):-
    TileX=2,
    TileY=2,
    ColorInd=2.

possible_entry_position(GameState, EntryPositions) :-
    GameState = state(Board, _, _),
    length(Board, Height), % Get the height of the board
    idx(1, Board, FirstRow), % Get the first row
    length(FirstRow, Width), % Get the width of the board
    findall(
        [[X, 1], [TileX, TileY]],
        (
            inbetween(1, Height, X), % Generate column indices
            member([TileX, TileY], [[1, 1], [2, 1]]) % Positions within the tile
        ),
        EntryPositions
    ).
    
%-------------------------------------------------------------------------------------

%---------------------------------Auxiliar--------------------------------------------

% Transform the board so that the bottom-left corner starts at [0,0].
transform_board(Board, TransformedBoard) :-
    lists:reverse(Board, TransformedBoard). % Simply reverse the rows.

%-------------------------------------------------------------------------------------