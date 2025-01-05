
:- module(move, [display_possible_moves/2]).

:- use_module('board.pl').
:- use_module('list.pl').
:- use_module('menu.pl').
:- use_module('random.pl').
:- use_module(library(lists), [reverse/2, maplist/3]).

%-----------------------------------------Display  moves-----------------------------------------------------


display_possible_moves(GameState,NewGameState) :-
    valid_moves(GameState, ListOfMoves), % Get all valid moves
    no_moves(ListOfMoves, GameState, NewGameState).



%------------------------------------------------------------------------------------------------------------

no_moves(ListOfMoves,GameState,NewGameState):-
    length(ListOfMoves, Len),
    generate_empty_lists(Len, Result),
    ListOfMoves = Result,
    write('You can\'t move any pieces after the rotation you just made. Your turn as ended.'), nl,
    write('Skipping turn...'), nl,
    busy_wait(300000000),
    NewGameState = GameState.

no_moves(ListOfMoves,GameState, NewGameState):-
    length(ListOfMoves, Len),
    generate_empty_lists(Len, Result),
    ListOfMoves \= Result,
    validate_index(ListOfMoves,Index),
    idx(Index, ListOfMoves, MovesForPiece), % Retrieve the sublist at the given index (1-based indexing)
    findall(Direction, member((Direction, _), MovesForPiece), Directions), % Extract directions
    append(Directions,['Choose other piece'],  Options), % Add an option to change the piece
    display_menu_options(Options, 1), % Display the directions
    validate_input(Options,DirectionIndex),
    length(Options,NumOptions),
    move_or_back(NumOptions,Index, DirectionIndex,MovesForPiece,GameState,NewGameState). % Check the user input


move_or_back(NumOptions,Index, DirectionIndex,MovesForPiece,GameState,NewGameState):-
    DirectionIndex < NumOptions, % Check if the direction index is valid
    idx(DirectionIndex, MovesForPiece, (Direction, DestPosition)),
    Move = (Index,DestPosition), % Create the Move variable
    move(GameState, Move, NewGameState). % Move the piece 
move_or_back(NumOptions,Index, DirectionIndex,MovesForPiece,GameState,NewGameState):-
    DirectionIndex = NumOptions, % Check if the direction index is valid
    display_possible_moves(GameState,NewGameState). % Display the possible moves again


%---------------------------------Create GameState after motion----------------------------------------------

move(GameState, Move, NewGameState):-
    GameState = state(Board, Players, CurrentPlayer, PiecesToWin),
    change_pieces(Move,CurrentPlayer,ListOfMoves, NewPlayer),
    change_players(Players, NewPlayer, NewPlayers),
    NewGameState = state(Board, NewPlayers, NewPlayer, PiecesToWin).

change_players(Players, NewPlayer, NewPlayers):-
    Players = [Player1, Player2],
    NewPlayer = player(_, _, PlayerColor, _),
    PlayerColor = orange,
    NewPlayers = [NewPlayer, Player2].
change_players(Players, NewPlayer, NewPlayers):- 
    Players = [Player1, Player2],
    NewPlayer = player(_, _, PlayerColor, _),
    PlayerColor = blue,
    NewPlayers = [Player1, NewPlayer].

change_pieces((Index, piece(Position, _)),CurrentPlayer,ListOfMoves, NewPlayer):-
    CurrentPlayer = player(PlayerNum, PlayerType, PlayerColor, PlayerPieces),
    idx(Index, PlayerPieces, piece(_,Id)),
    NewPiece = piece(Position, Id),
    replace_index(Index, NewPiece, PlayerPieces, NewPieces),
    NewPlayer= player(PlayerNum, PlayerType, PlayerColor, NewPieces).

%-----------------------------------------------------------------------------------------------------------

%---------------------------------Calculate all possible moves----------------------------------------------

valid_moves(GameState, ListOfMoves) :-
    GameState = state(Board, Players, player(PlayerNum, PlayerType, Color, Pieces),PiecesToWin), % Extract the pieces
    transform_board(Board, TransformedBoard), % Transform the board
    NewGameState = state(TransformedBoard, Players, player(PlayerNum, PlayerType, Color, Pieces),PiecesToWin), % Create a new game state
    all_moves(NewGameState, Pieces, ListOfMoves).

% Iterates through the list of Pieces and calculates all possible moves with directions and destination indices.
all_moves(_, [], []). % Base case: No pieces left to process.
all_moves(GameState, [piece([[0, 0], [0, 0]], Id) | RestPieces], [FormattedPositions | RemainingMoves]) :-
    possible_entry_position(GameState, EntryPositions), % Get all possible entry positions
    findall(
        (FormattedPosition,piece(Position,Id)),
        (
            member(Position, EntryPositions), % Iterate through all entry positions
            can_move(GameState, piece(Position,Id)), % Check if move is valid
            format_position(GameState,Position, FormattedPosition) % Format the position
        ),
        FormattedPositions
    ),
    all_moves(GameState, RestPieces, RemainingMoves). % Recurse for the rest of the pieces

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

% Format the position with column letters and row numbers
format_position(state(Board,_,_,_),[[ColumnIndex, RowIndex], TilePosition], FormattedPosition) :-
    get_dest_color(Board, [[ColumnIndex, RowIndex], TilePosition], Color),
    column_letter(ColumnIndex, Letter), % Convert column index to letter
    number_codes(RowIndex, Codes), % Converts the number to a list of character codes
    atom_codes(RowAtom, Codes),
    atom_concat(Letter, RowAtom, LetterNumber),% Combine letter and row index as an atom
    atom_concat('-', Color, NewColor), % Combine letter and row index as an atom
    atom_concat(LetterNumber, NewColor, FormattedPosition). % Combine letter and row index as an atom

column_letter(Index, Letter) :-
    Code is Index + 64, % Convert 1-based index to ASCII ('A' = 65)
    char_code(Letter, Code).

% Calculates the new position based on the direction.
move_in_direction(piece([[BoardX,BoardY],[TileX,TileY]], Id), up, piece([[BoardX,NewBoardY],[TileX,NewTileY]],Id)) :-
    NewTileY is (3 mod (TileY + 1) + 1),
    NewBoardY is BoardY -1 * (TileY mod 2 - 1).

move_in_direction(piece([[BoardX,BoardY],[TileX,TileY]], Id), down, piece([[BoardX,NewBoardY],[TileX,NewTileY]],Id)) :-
    NewTileY is (3 mod (TileY + 1) + 1),
    NewBoardY is BoardY +1 * ((TileY+1) mod 2 - 1).

move_in_direction(piece([[BoardX,BoardY],[TileX,TileY]], Id), right, piece([[NewBoardX,BoardY],[NewTileX,TileY]],Id)) :-
    NewTileX is (3 mod (TileX + 1) + 1),
    NewBoardX is BoardX -1 * (TileX mod 2 - 1).

move_in_direction(piece([[BoardX,BoardY],[TileX,TileY]], Id), left, piece([[NewBoardX,BoardY],[NewTileX,TileY]],Id)) :-
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

can_move(GameState, piece(DestInd,Id)) :-
    GameState = state(Board, Players, player(_, _, PlayerColor, _),_),
    get_dest_color(Board, DestInd, DestColor),
    validate_color(PlayerColor,DestColor),
    check_for_pieces(Players,piece(DestInd,Id)),
    check_board_limits(Board,PlayerColor,DestInd).

validate_color(PlayerColor,DestColor) :-
    PlayerColor = orange,
    DestColor \= blue,
    DestColor \= empty.
validate_color(PlayerColor,DestColor) :-
    PlayerColor = blue,
    DestColor \= orange,
    DestColor \= empty.

%check if there are pieces in the way  
check_for_pieces(Players, piece(Position, _)) :-
    get_Player_Pieces(Players, 1, Player1Pieces),
    get_Player_Pieces(Players, 2, Player2Pieces),
    append(Player1Pieces, Player2Pieces, AllPieces),
    maplist(get_position, AllPieces, AllPositions), % Extract all positions
    \+ member(Position, AllPositions). % Check if Position is not in AllPositions

get_position(piece(Position, _), Position). % Helper predicate to extract position

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
    NewHeight is Height + 1,
    Y =< NewHeight.
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
    length(Board, Height), 
    NewHeight is Height + 1,
    BoardY\=0,
    BoardY\= NewHeight,
    idx(BoardY, Board, DestLine),
    idx(BoardX, DestLine, DestTile),
    get_color( TileX, TileY, ColorInd),
    DestTile = tile(Sides, Rotation),
    rotated_sides(Sides, Rotation, RotatedSides),
    idx(ColorInd, RotatedSides, DestColor).
get_dest_color(Board, [[BoardX,BoardY],[TileX, TileY]], DestColor):-
    BoardY=0,
    DestColor = score.
get_dest_color(Board, [[BoardX,BoardY],[TileX, TileY]], DestColor):- 
    length(Board, Height), 
    NewHeight is Height + 1,
    BoardY= NewHeight,
    DestColor = score.

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
    GameState = state(Board, _, player(_, _, Color, _),_), % Extract the board and player color
    length(Board, Height), % Get the height of the board
    idx(1, Board, FirstRow), % Get the first row
    length(FirstRow, Width), % Get the width of the board
    Color = orange, % Orange player
    findall(
        [[X, 1], [TileX, TileY]],
        (
            inbetween(1, Width, X), % Generate column indices
            member([TileX, TileY], [[1, 1], [2, 1]]) % Positions within the tile
        ),
        EntryPositions
    ).
possible_entry_position(GameState, EntryPositions) :-
    GameState = state(Board, _, player(_, _, Color, _),_), % Extract the board and player color
    length(Board, Height), % Get the height of the board
    idx(1, Board, FirstRow), % Get the first row
    length(FirstRow, Width), % Get the width of the board
    Color = blue, % blue player
    findall(
        [[X, Height], [TileX, TileY]],
        (
            inbetween(1, Width, X), % Generate column indices
            member([TileX, TileY], [[1, 2], [2, 2]]) % Positions within the tile
        ),
        EntryPositions
    ).
    
%-------------------------------------------------------------------------------------

validate_index(Options, Choice):-
    write('Enter the number of the piece you want to move '),
    read(Number),
    verify_index(Number,Choice,Options),
    valid_index(Choice,Options).

verify_index(Number, Choice,_) :-
    integer(Number),
    Choice = Number.
verify_index(Number, Choice,Options) :-
    \+ integer(Number),
    write('Invalid input. Please enter a number. '),
    validate_index(Options,Choice).

valid_index(Choice, Options) :-
    length(Options,Len),
    Choice > 0,
    Choice =< Len.
valid_index(Choice,Options) :-
    write('Invalid choice. Try again.\n'),
    validate_index(Options,Choice).

validate_input(Options, Choice):-
    write('Enter the number of the direction you want to move '),
    read(Number),
    verify_input(Number,Choice,Options),    
    valid_input(Choice,Options).

verify_input(Number, Choice,_) :-
    integer(Number),
    Choice = Number.
verify_input(Number, Choice,Options) :-
    \+ integer(Number),
    write('Invalid input. Please enter a number. '),
    validate_input(Options,Choice).

valid_input(Choice, Options) :-
    length(Options,Len),
    Choice > 0,
    Choice =< Len.
valid_input(Choice,Options) :-
    write('Invalid input. Try again.\n'),
    validate_input(Options, Choice).
%---------------------------------Auxiliar--------------------------------------------

% Transform the board so that the bottom-left corner starts at [0,0].
transform_board(Board, TransformedBoard) :-
    lists:reverse(Board, TransformedBoard). % Simply reverse the rows.
    

%-------------------------------------------------------------------------------------