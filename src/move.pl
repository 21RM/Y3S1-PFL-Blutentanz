
:- module(move, [display_possible_moves/1]).

:- use_module('board.pl').
:- use_module('list.pl').
:- use_module('menu.pl').

%-----------------------------------------Display  moves-----------------------------------------------------

% Reads an integer, retrieves the sublist at that index, and prints the directions.
display_possible_moves(GameState) :-
    valid_moves(GameState, ListOfMoves), % Get all valid moves
    write('Enter the number of the piece you want to move: '),
    read(Index), % Read the integer index
    idx(Index, ListOfMoves, MovesForPiece), % Retrieve the sublist at the given index (1-based indexing)
    findall(Direction, member((Direction, _), MovesForPiece), Directions), % Extract directions
    display_menu_options(Directions, 1), % Display the directions
    write('Enter the number of the direction you want to move: '),
    read(DirectionIndex), % Read the direction index
    Move = (Index, DirectionIndex), % Create the Move variable
    move(GameState, Move, NewGameState). % Move the piece

%------------------------------------------------------------------------------------------------------------

%---------------------------------Create GameState after motion----------------------------------------------

move(GameState, Move, NewGameState):-
    GameState = (Board, Players, CurrentPlayer),
    valid_moves(GameState, ListOfMoves),
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
    Move = (PieceIndex, DirectionIndex),
    CurrentPlayer = (PlayerNum, PlayerType, PlayerColor, PlayerPieces),
    idx(PieceIndex, ListOfMoves, PieceMoves),
    idx(DirectionIndex, PieceMoves, (Direction, DestPosition)),
    replace_index(PieceIndex, DestPosition, Pieces, NewPieces)
    NewPlayer=(PlayerNum, PlayerType, PlayerColor, NewPieces).

%-----------------------------------------------------------------------------------------------------------

%---------------------------------Calculate all possible moves----------------------------------------------

valid_moves(GameState, ListOfMoves) :-
    idx(3, GameState, CurrentPlayer),
    idx(4 ,CurrentPlayer ,Pieces),
    all_moves(GameState, Pieces, ListOfMoves).


% Iterates through the list of Pieces and calculates all possible moves with directions and destination indices.
all_moves(_, [], []). % Base case: No pieces left to process.
all_moves(GameState, [Piece | RestPieces], [MovesForPiece | RemainingMoves]) :-
    % Calculate moves in all four directions for the current piece
    findall(
        (Direction, DestPosition),
        (
            member(Direction, [up, down, left, right]), % Test each direction
            move_in_direction(Piece, Direction, DestPosition), % Calculate destination
            can_move(GameState, DestPosition) % Check if move is valid
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
%check if the move is valid
can_move( GameState, DestInd) :-
    idx(1, GameState, Board),
    get_dest_color(Board, DestInd, DestColor),
    idx(3, GameState, Player),
    idx(3, Player, PlayerColor),
    validate_color(PlayerColor,DestColor),
    idx(2, GameState, Players),
    check_for_pieces(Players,DestInd)
    check_board_limits(Board,Player,DestInd).

validate_color(PlayerColor,DestPiece) :-
    PlayerColor = orange,
    DestPiece \= blue,
    DestPiece \= empty.
validate_color(PlayerColor,DestPiece) :-
    PlayerColor = blue,
    DestPiece \= orange,
    DestPiece \= empty.

%check if there are pieces in the way  
check_for_pieces(Players, Dest):-
    get_Player_Pieces(Players, 1, Player1Pieces),
    get_Player_Pieces(Players, 2, Player2Pieces),
    append(Player1Pieces, Player2Pieces, AllPieces),
    not(member(Dest, AllPieces)).

%check for board limits according to the player
check_board_limits(Board, Player, [[X,Y],TileCord]):-
    length(Board, height), 
    idx(1, Board, FirstRow),
    length(FirstRow, width),
    X>=0,
    Y>=0,
    X<=width,
    idx(3, Player, PlayerColor),
    check_color_limitis(height,PlayerColor,Y).

check_color_limitis(height,PlayerColor,Y):-
    PlayerColor=orange,
    Y>0
    Y<=height+1.
check_color_limitis(height,PlayerColor,Y):-
    PlayerColor=blue,
    Y>=0
    Y<=height.
%----------------------------------------------------------------------------------------
%---------------------------------Getters------------------------------------------------
get_Player_Pieces(Players, Player_num, Pieces) :-
    idx(Player_num, Players, Player),
    idx(4, Player, Pieces).

get_dest_color(Board, [[BoardX,BoardY],[TileX, TileY]], DestPiece):-
    transform_board(Board, TransformedBoard),
    idx(BoardX, TransformedBoard, DestLine),
    idx(BoardY, DestLine, DestTile),
    get_color( TileX, TileY, ColorInd),
    idx(1,DestTile, Sides), 
    idx(2,DestTile, Rotation),
    rotated_sides(Sides, Rotation, RotatedSides),
    idx(ColorInd, RotatedSides, DestPiece).

get_color( TileX, TileY, ColorInd):-
    Tilex=1,
    TileY=1,
    ColorInd=3.
get_color( TileX, TileY, ColorInd):-
    Tilex=1,
    TileY=2,
    ColorInd=1.
get_color( TileX, TileY, ColorInd):-
    Tilex=2,
    TileY=1,
    ColorInd=4.
get_color( TileX, TileY, ColorInd):-
    Tilex=2,
    TileY=2,
    ColorInd=2.
%-------------------------------------------------------------------------------------

%---------------------------------Auxiliar--------------------------------------------

% Transform the board so that the bottom-left corner starts at [0,0].
transform_board(Board, TransformedBoard) :-
    reverse(Board, TransformedBoard). % Simply reverse the rows.

%-------------------------------------------------------------------------------------