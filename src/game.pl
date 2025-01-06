% ========================================================================================== %
% =====        THIS FILE CONTAINS THE PREDICATES RELATED TO THE MAIN GAME LOOP         ===== %
% ========================================================================================== %

% ---------------------------------------- INCLUDES --------------------------------------------- %
:- use_module('board.pl').
:- use_module('random.pl').
:- use_module('display.pl').
:- use_module('list.pl').
:- use_module('menu.pl').
:- use_module('move.pl').
% ----------------------------------------------------------------------------------------------- %

% ---------------------------------------- Definitions ------------------------------------------ %
% --> Define the standard tile.
standard_tile([empty, orange, blue, gray]).
% ----------------------------------------------------------------------------------------------- %


% --------------------------------------- MAIN PREDICATE ---------------------------------------- %
% --> Main predicate for the game.
play :-

    main_menu(GameConfig),
    initial_state(GameConfig, GameState), % Initialize the game state.
    display_game(GameState), % Display the board
    game_loop(GameState). % Start the game loop.
% ----------------------------------------------------------------------------------------------- %

% ------------------------------------ INITIALIZE GAME STATE ------------------------------------ %
% --> Initialize the game state with a 4x4 board.
initial_state(config([Player1PiecesCount,Player2PiecesCount],[Rows, Columns], [Player1Type, Player2Type], PiecesToWin), GameState) :-
    write('Initializing game state...'), nl,
    % -> Initialize pieces.
    initialize_pieces(Player1PiecesCount, Player2PiecesCount, Player1Pieces, Player2Pieces),
    write('Pieces initialized...'), nl,
    
    % -> Generate the board.
    generate_random_board(Rows, Columns, Board),
    write('Board generated...'), nl,

    % -> Initialize players.
    initialize_players(Player1Type, Player2Type, Player1Pieces, Player2Pieces, Players),
    write('Players initialized...'), nl,

    % -> Set player 1 has starter, orange starts first.
    idx(1, Players, CurrentPlayer),


    % -> Return value GameState, contains Board, Players and the player who starts
    GameState = state(Board, Players, CurrentPlayer, PiecesToWin).
% ----------------------------------------------------------------------------------------------- %




% ----------------------------------------- Helpers --------------------------------------------- %
% --> Assign a random rotation to a tile.
randomize_tile(tile(Sides, Rotation)) :-
    standard_tile(Sides),                         % - Use the fixed tile colors.
    my_random_member(Rotation, [0, 90, 180, 270]).  % - Randomly select a rotation.

% --> Generate a lxc board with random rotations.
generate_random_board(Rows, Columns, Board) :-
    TotalTiles is Rows * Columns,
    length(Tiles, TotalTiles),                     % - Create a list of l*c placeholders.
    maplist(randomize_tile, Tiles),        % - Randomize the rotation of each tile.
    partition_board(Columns, Tiles, Board),        % - Partition the tiles into rows.
    write(Board).

% --> Partition a flat list into rows of l elements each. (TODO -> for now is 4 as a placeholder)
partition_board(_, [], []).
partition_board(Columns, Tiles, [Row | Rest]) :-
    length(Row, Columns),                       
    append(Row, Remaining, Tiles),        
    partition_board(Columns, Remaining, Rest).

% --> mapList, aplies a predicate to every member of a list
maplist(_, []) :- !.  % Base case: Empty list
maplist(Predicate, [Head | Tail]) :-
    call(Predicate, Head),  % Call the predicate on the current element
    maplist(Predicate, Tail).  % Recurse for the rest of the list

% --> Initialize Players, with types and colors.
initialize_pieces(Player1PiecesCount, Player2PiecesCount, Player1Pieces,Player2Pieces):-
    create_pieces(1, Player1PiecesCount, Player1Pieces),
    create_pieces(1, Player2PiecesCount, Player2Pieces).


% Base case: When Count is 0, return an empty list.
create_pieces(_, 0, []) :- !.
% Recursive case: Add [[0,0], [0,0]] to the result list.
create_pieces(Id, Count, [Piece | Rest]) :-
    Count > 0,
    Piece = piece([[0, 0], [0, 0]], Id),
    NextCount is Count - 1,
    NextId is Id + 1,
    create_pieces(NextId, NextCount, Rest).

% --> Initialize Players, with types and colors.
initialize_players(Player1Type, Player2Type, Player1Pieces, Player2Pieces, [Player1, Player2]) :-
    Player1 = player(player1, Player1Type, orange, Player1Pieces),
    Player2 = player(player2, Player2Type, blue, Player2Pieces).

% Converts a single character atom (letter) to uppercase.
to_uppercase(Lower, Upper) :-
    char_code(Lower, CodeLower),
    char_code('a', CodeA),
    char_code('z', CodeZ),
    char_code('A', CodeAUpper),
    to_uppercase_helper(CodeLower, CodeA, CodeZ, CodeAUpper, Upper).

% Helper predicate for conversion logic.
to_uppercase_helper(CodeLower, CodeA, CodeZ, CodeAUpper, Upper) :-
    CodeLower >= CodeA,
    CodeLower =< CodeZ,
    CodeUpper is CodeAUpper + (CodeLower - CodeA),
    char_code(Upper, CodeUpper).

to_uppercase_helper(CodeLower, CodeA, CodeZ, _, Upper) :-
    (CodeLower < CodeA; CodeLower > CodeZ), % Not a lowercase letter
    char_code(Upper, CodeLower).
% ----------------------------------------------------------------------------------------------- %


% ---------------------------------------- DISPLAY GAME ----------------------------------------- %
% --> Main display game predicate.
display_game(state(Board, Players, CurrentPlayer, PiecesToWin)) :-
    clear_screen,
    print_title,
    get_score(Board, Players, ScorePlayer1, ScorePlayer2),
    display_score(Players, CurrentPlayer, ScorePlayer1, ScorePlayer2, PiecesToWin),
    display_board(Board, Players),
    display_bench(Board, CurrentPlayer), nl.

display_game_over_menu(Winner) :-
    clear_screen,
    print_title,
    display_winner(Winner),
    write('Give any input to go return to Main Menu '),
    read(RI),
    play.
% ----------------------------------------------------------------------------------------------- %



% ---------------------------------------- GAME LOOP -------------------------------------------- %
% --> Main game loop.
check_end_game(GameState) :-
    game_over(GameState, Winner),
    handle_game_over(Winner).

game_loop(GameState) :-
    take_turn(GameState, UpdatedGameState),
    switch_player(UpdatedGameState, NewGameState),
    display_game(NewGameState),
    game_loop(NewGameState).

game_over(GameState, Winner) :-
    GameState = state(Board, Players, CurrentPlayer, [PiecesToWinPlayer1, PiecesToWinPlayer2]),
    get_score(Board,Players,ScorePlayer1,ScorePlayer2),
    check_winner(ScorePlayer1, ScorePlayer2, PiecesToWinPlayer1, PiecesToWinPlayer2, Winner).
    

check_winner(ScorePlayer1, ScorePlayer2, PiecesToWinPlayer1, PiecesToWinPlayer2, Winner) :-
    ScorePlayer1 < PiecesToWinPlayer1,
    ScorePlayer2 = PiecesToWinPlayer2,
    Winner = player2.
check_winner(ScorePlayer1, ScorePlayer2, PiecesToWinPlayer1, PiecesToWinPlayer2, Winner) :-
    ScorePlayer1 = PiecesToWinPlayer1,
    ScorePlayer2 < PiecesToWinPlayer2,
    Winner = player1.
check_winner(ScorePlayer1, ScorePlayer2, PiecesToWinPlayer1, PiecesToWinPlayer2, Winner) :-
    ScorePlayer1 < PiecesToWinPlayer1,
    ScorePlayer2 < PiecesToWinPlayer2,
    Winner = none.

handle_game_over(Winner) :-
    Winner = none,
    !.
handle_game_over(Winner) :-
    Winner \= none,
    display_game_over_menu(Winner).



% ----------------------------------------------------------------------------------------------- %

% ------------------------------------ GAME LOOP HELPERS ---------------------------------------- %
take_turn(GameState, NewGameState) :-
    GameState = state(_,_,player(_,PlayerType,_,_),_),
    PlayerType = human,
    rotation_phase(GameState, RotatedGameState),
    round(1, RotatedGameState,NewGameState).
take_turn(GameState, NewGameState) :-
    GameState = state(_,_,player(_,PlayerType,_,_),_),
    PlayerType = dumbbot.
    %TODO CALL BOT PROCESSING.
    

round(Round,GameState,FinalGameState):-
    Round<3,
    Round>0,
    NewRound is (Round +1),
    RoundCount is 4 - Round,
    display_game(GameState),
    write('You can move any piece '), write(RoundCount), write(' more times.'), nl,
    display_possible_moves(NewRound,GameState,NewGameState,FinalRound),
    check_end_game(NewGameState),
    round(FinalRound,NewGameState, FinalGameState).
round(Round,GameState,FinalGameState):-
    Round=3,
    display_game(GameState),
    write('Last move...'), nl,
    display_possible_moves(Round,GameState,NewGameState, NewRound),
    check_end_game(NewGameState),
    FinalGameState=NewGameState.
round(4,GameState,GameState).
   
% --> Rotation Phase.
rotation_phase(GameState, RotatedGameState) :-
    write('Rotate a row (numbers), or a column (lowercase letters) '),
    read(Selection),
    handle_rotation_choice(Selection, GameState, RotatedGameState).

% --> Handle the rotation choice.
handle_rotation_choice(Selection, state(Board, [player(Player1,Player1Type,Player1Color,Player1Pieces), player(Player2,Player2Type,Player2Color,Player2Pieces)], CurrentPlayer, PiecesToWin), state(RotatedBoard, [player(Player1,Player1Type,Player1Color,NewPlayer1Pieces), player(Player2,Player2Type,Player2Color,NewPlayer2Pieces)], NewCurrentPlayer, PiecesToWin)) :-
    integer(Selection),
    rotate_row(Selection, Board, RotatedBoard, Player1Pieces, Player2Pieces, NewPlayer1Pieces, NewPlayer2Pieces),
    change_current_player_pieces(CurrentPlayer, NewPlayer1Pieces, NewPlayer2Pieces, NewCurrentPlayer).

handle_rotation_choice(Selection, state(Board, [player(Player1,Player1Type,Player1Color,Player1Pieces), player(Player2,Player2Type,Player2Color,Player2Pieces)], CurrentPlayer, PiecesToWin), state(RotatedBoard, [player(Player1,Player1Type,Player1Color,NewPlayer1Pieces), player(Player2,Player2Type,Player2Color,NewPlayer2Pieces)], NewCurrentPlayer, PiecesToWin)) :-
    atom(Selection),
    column_letter_to_index(Selection, ColumnIndex),
    rotate_column(ColumnIndex, Board, RotatedBoard, Player1Pieces, Player2Pieces, NewPlayer1Pieces, NewPlayer2Pieces),
    change_current_player_pieces(CurrentPlayer, NewPlayer1Pieces, NewPlayer2Pieces, NewCurrentPlayer).

handle_rotation_choice(_, GameState, RotatedGameState) :-
    write('Invalid input. Please enter a valid row number or column letter.\n'),
    rotation_phase(GameState, RotatedGameState).

change_current_player_pieces(player(CP, CPType, CPColor, _), NewPlayer1Pieces, NewPlayer2Pieces, player(CP, CPType, CPColor, NewCPPieces)) :-
    CPColor = orange,
    NewCPPieces = NewPlayer1Pieces.
change_current_player_pieces(player(CP, CPType, CPColor, _), NewPlayer1Pieces, NewPlayer2Pieces, player(CP, CPType, CPColor, NewCPPieces)) :-
    CPColor = blue,
    NewCPPieces = NewPlayer2Pieces.

% --> Convert a column letter to an index.
column_letter_to_index(ColumnLetter, ColumnIndex) :-
    to_uppercase(ColumnLetter, UppercaseLetter),
    char_code('A', ACode),
    char_code(UppercaseLetter, LetterCode),
    ColumnIndex is LetterCode - ACode + 1.

% --> helper to switch player.
switch_player(state(Board, [Player1, Player2], CurrentPlayer, PiecesToWin), state(Board, [Player1, Player2], NextPlayer,PiecesToWin)) :-
    CurrentPlayer = Player1,
    NextPlayer = Player2.
switch_player(state(Board, [Player1, Player2], CurrentPlayer, PiecesToWin), state(Board, [Player1, Player2], NextPlayer, PiecesToWin)) :-
    CurrentPlayer = Player2,
    NextPlayer = Player1.
% ----------------------------------------------------------------------------------------------- %

%-------------------------------------------- Score ----------------------------------------------%
get_score(Board,Players, ScorePlayer1, ScorePlayer2) :-
    Players = [player(_, _, _, Player1Pieces), player(_, _, _, Player2Pieces)],
    length(Board, Height), % Get the height of the board
    idx(1, Board, FirstRow), % Get the first row
    length(FirstRow, Width), % Get the width of the board
    NewHeight is (Height + 1),
    count_pieces([[1, NewHeight ],[1,1]], Player1Pieces, ScorePlayer1),
    count_pieces([[1,0],[1,1]], Player2Pieces, ScorePlayer2).


% Counts how many pieces in the Pieces list are equal to the given Position.
count_pieces(_, [], 0). % Base case: No pieces left to process.
count_pieces(Position, [piece([[X,Y],Tyle], _)| Rest], Count) :-
    [[X,Y],Tyle] \= [[0,0],[0,0]], % Check if the current piece matches Position.
    Position = [[_,Y],_],
    count_pieces(Position, Rest, RestCount), % Count in the remaining list
    Count is RestCount + 1. % Increment count if the current piece matches Position.
count_pieces(Position, [piece([[_,Y],_], _) | Rest], Count) :-
    Position \= [[_,Y],_],
    count_pieces(Position, Rest, Count). % Skip if the current piece does not match. 
count_pieces(Position, [piece([[X,Y],Tyle], _) | Rest], Count) :-
    [[X,Y],Tyle] = [[0,0],[0,0]],
    count_pieces(Position, Rest, Count). % Skip if the current piece does not match. 

% ------------------------------------------------------------------------------------------------- %



% ------------------------------------- DUMB BOT HELPERS ------------------------------------------ %
random_rotate_select(GameState, RotatedGameState) :-
    GameState = state(Board,_,_,_),
    length(Board, RowCount),
    idx(1, Board, Row),
    length(Row, CoulmnCount),
    numlist(1, RowCount, RowList),
    letterlist(1, ColumnCount, ColumnList).

% ------------------------------------------------------------------------------------------------- %
