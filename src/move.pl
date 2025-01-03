:- use_module('board.pl').
:- use_module('list.pl').

:- module(move, [moving_options/7]).

can_move( Board, Players, Player_num, DestInd) :-
    get_Player_Pieces(Players, Player_num,Pieces),
    get_dest_color(Board, DestInd, DestPiece),
    validate_color(Player_num,DestPiece),
    check_for_pieces(Players,DestInd)
    check_board_limits(Board,Player_num,DestInd).

%check for board limits according to the player
check_board_limits(Board, Player_num, [[X,Y],TileCord]):-
    length(Board, height), 
    idx(1, Board, FirstRow),
    length(FirstRow, width),
    X>=0,
    Y>=0,
    X<=width,
    check_color_limitis(height,Player_num,Y).


check_color_limitis(height,Player_num,Y):-
    Player_num=1,
    Y>0
    Y<=height+1.
check_color_limitis(height,Player_num,Y):-
    Player_num=2,
    Y>=0
    Y<=height.

moving_options(Board, Players, Player_num,Position,up, Options, NewOptions):-
    move_in_direction(Position, up, DestPosition),
    can_move( Board, Players, Player_num, DestPosition),
    append(["Move up"],Options,NewOptions).

moving_options(Board, Players, Player_num,Position,down, Options, NewOptions):-
    move_in_direction(Position, down, DestPosition),
    can_move( Board, Players, Player_num, DestPosition),
    append(["Move down"],Options,NewOptions).

moving_options(Board, Players, Player_num,Position,right, Options, NewOptions):-
    move_in_direction(Position, right, DestPosition),
    can_move( Board, Players, Player_num, DestPosition),
    append(["Move right"],Options,NewOptions).

moving_options(Board, Players, Player_num,Position,left, Options, NewOptions):-
    move_in_direction(Position, left, DestPosition),
    can_move( Board, Players, Player_num, DestPosition),
    append(["Move left"],Options,NewOptions).

% move_in_direction(+CurrentPosition, +Direction, -NewPosition)
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

% Transform the board so that the bottom-left corner starts at [0,0].
transform_board(Board, TransformedBoard) :-
    reverse(Board, TransformedBoard). % Simply reverse the rows.

get_Player_Pieces(Players, Player_num,Pieces) :-
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

get_piece(Players, Player_num, Ind, Player,Piece) :-
    get_Player_Pieces(Players, Player_num, Pieces),
    idx(Ind, Pieces, Piece).

validate_color(Player_num,DestPiece) :-
    Player_num = 1,
    DestPiece \= blue,
    DestPiece \= empty.

validate_color(Player_num,DestPiece) :-
    Player_num = 2,
    DestPiece \= orange,
    DestPiece \= empty.

%check if there are pieces in the way  
check_for_pieces(Players, Dest):-
    get_Player_Pieces(Players, 1, Player1Pieces),
    get_Player_Pieces(Players, 2, Player2Pieces),
    append(Player1Pieces, Player2Pieces, AllPieces),
    not(member(Dest, AllPieces)).

