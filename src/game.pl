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
    write('Give any input to return to Main Menu '),
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
    GameState = state(Board, Players, _, [PiecesToWinPlayer1, PiecesToWinPlayer2]),
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
take_turn(GameState, Move) :-
    GameState = state(_,_,player(_,PlayerType,_,_),_),
    PlayerType = dumbbot,
    choose_move(GameState, dumbbot, Move).

take_turn(GameState, Move) :-
    GameState = state(_,_,player(_,PlayerType,_,_),_),
    PlayerType = smartbot,
    choose_move(GameState, smartbot, Move).
    

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
    display_possible_moves(Round,GameState,NewGameState, _),
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

change_current_player_pieces(player(CP, CPType, CPColor, _), NewPlayer1Pieces, _, player(CP, CPType, CPColor, NewCPPieces)) :-
    CPColor = orange,
    NewCPPieces = NewPlayer1Pieces.
change_current_player_pieces(player(CP, CPType, CPColor, _), _, NewPlayer2Pieces, player(CP, CPType, CPColor, NewCPPieces)) :-
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



% ------------------------------------- SMART BOT HELPERS ----------------------------------------- %

smart_bot_turn(GameState, NewGameState) :-
    busy_wait(50000000),
    GameState = state(_,_,CurrentPlayer,_),
    get_all_rotations(GameState, RotatedGameStates),
    all_valid_moves(RotatedGameStates, Moves),
    simulate_moves(RotatedGameStates, Moves, SimulatedGameStates),
    evaluate_states(SimulatedGameStates, CurrentPlayer, EvaluatedStates),
    flatten(EvaluatedStates, FlatEvaluatedState),
    index_of_max(FlatEvaluatedState, IndexOfMax),
    get_chosen_game_state(SimulatedGameStates, IndexOfMax, NewGameState),
    check_end_game(NewGameState),
    display_game(NewGameState),
    smart_bot_round(2, NewGameState, FinalGameState).

calculate_move_no_rotation(GameState, NewGameState) :-
    valid_moves(GameState, Moves),
    get_simulated_game_states(GameState, Moves, SimulatedGameStates),
    evaluate_states_rotation(SimulatedGameStates, CurrentPlayer, EvaluatedStates),
    flatten(EvaluatedStates, FlatEvaluatedState),
    index_of_max(FlatEvaluatedState, IndexOfMax),
    get_chosen_game_state(SimulatedGameStates, IndexOfMax, NewGameState).

value(GameState, Player, Value) :-
    GameState = state(Board, Players, CurrentPlayer, _),
    length(Board, RowCount),
    Player = player(_, _, PlayerColor, _),
    CurrentPlayer = player(_, _, _, CurrentPlayerPieces),
    length(CurrentPlayerPieces, NumberOfPieces),
    goal_row(PlayerColor, RowCount, GoalRow),
    calculate_progress(CurrentPlayerPieces, RowCount, GoalRow, Progress),
    SelfValue is Progress,
    get_other_player(Players, Player, player(_,_,OponentColor,OponentPieces)),
    goal_row(OponentColor, RowCount, OponentGoalRow),
    calculate_progress(OponentPieces, RowCount, OponentGoalRow, OponentProgress),
    OponentValue is OponentProgress,
    Value is SelfValue-OponentValue.

goal_row(PlayerColor, RowCount, GoalRow) :-
    PlayerColor = orange,
    GoalRow is RowCount*2 + 1.
goal_row(PlayerColor, _, GoalRow) :-
    PlayerColor = blue,
    GoalRow is 0.

calculate_progress([], _, _, 0).
calculate_progress([Piece | Rest], RowCount, GoalRow, Progress) :-
    Piece = piece(Position, _),
    Position = [[0,0],[0,0]],
    PieceProgress is 0,
    calculate_progress(Rest, RowCount, GoalRow, RestProgress),
    Progress is PieceProgress + RestProgress.
calculate_progress([Piece | Rest], RowCount, GoalRow, Progress) :-
    Piece = piece(Position, _),
    Position \= [[0,0],[0,0]],
    Position = [[_, CurrentRow], [_, TileRow]],
    GoalRow \= 0,
    PieceProgress is (CurrentRow-1)*2 + TileRow,
    calculate_progress(Rest, RowCount, GoalRow, RestProgress),
    Progress is PieceProgress + RestProgress.
calculate_progress([Piece | Rest], RowCount, GoalRow, Progress) :-
    Piece = piece(Position, _),
    Position \= [[0,0],[0,0]],
    Position = [[_, CurrentRow], [_, TileRow]],
    GoalRow = 0,
    PieceProgress is RowCount*2 - ((CurrentRow-1)*2) - TileRow + 1,
    calculate_progress(Rest, RowCount, GoalRow, RestProgress),
    Progress is PieceProgress + RestProgress.

get_other_player([Player1, Player2], Player, OtherPlayer) :-
    Player1 = player(_,_,Color1,_),
    Player = player(_,_,Color,_),
    Color = Color1,
    OtherPlayer = Player2.
get_other_player([Player1, Player2], Player, OtherPlayer) :-
    Player2 = player(_,_,Color2,_),
    Player = player(_,_,Color,_),
    Color = Color2,
    OtherPlayer = Player1.


get_all_rotations(GameState, RotatedStates) :-
    GameState = state(Board,_,_,_),
    length(Board, RowCount),
    idx(1, Board, Row),
    length(Row, ColumnCount),
    numlist(1, RowCount, RowList),
    letterlist(1, ColumnCount, ColumnList),
    append(RowList, ColumnList, ResList),
    get_rotated_game_states(ResList, GameState, RotatedStates).


get_rotated_game_states([], _, []). % Base case: No more selections to process.
get_rotated_game_states([Selection|RestOfSelections], GameState, [RotatedGameState|Rest]) :-
    handle_rotation_choice(Selection, GameState, RotatedGameState),
    get_rotated_game_states(RestOfSelections, GameState, Rest).

all_valid_moves([], []).
all_valid_moves([RotatedGameState|RotatedGameStates], [Moves|Rest]) :-
    valid_moves(RotatedGameState, Moves),
    all_valid_moves(RotatedGameStates, Rest).

simulate_moves(_, [], []).
simulate_moves([RotatedGameState|RotatedGameStates], [MovesOnRotation|RestMoves], [SimulatedGameStates|Rest]) :-
    get_simulated_game_states(RotatedGameState, MovesOnRotation, SimulatedGameStates),
    simulate_moves(RotatedGameStates, RestMoves, Rest).

get_simulated_game_states(_, [], []).
get_simulated_game_states(RotatedGameState, [MovesPerPiece|MovesOnRotation], [ListOfSimulatedGameStatesPerPiece|Rest]) :-
    get_simulated_game_states_per_piece(RotatedGameState, MovesPerPiece, ListOfSimulatedGameStatesPerPiece),
    get_simulated_game_states(RotatedGameState, MovesOnRotation, Rest).

get_simulated_game_states_per_piece(_, [], []).
get_simulated_game_states_per_piece(RotatedGameState, [Move|MovesPerPiece], [SimulatedGameState|Rest]) :-
    Move = (_, piece(Position, Id)),
    Final = (Id, piece(Position, Id)),
    move(RotatedGameState, Final, SimulatedGameState),
    get_simulated_game_states_per_piece(RotatedGameState, MovesPerPiece, Rest).

evaluate_states([], _, []).
evaluate_states([SimulatedGameState|SimulatedGameStates], CurrentPlayer, [EvaluatedState|Rest]) :-
    evaluate_states_rotation(SimulatedGameState, CurrentPlayer, EvaluatedState),
    evaluate_states(SimulatedGameStates, CurrentPlayer, Rest).

evaluate_states_rotation([], _, []).
evaluate_states_rotation([SimulatedGameStatePerPiece|SimulatedGameState], CurrentPlayer, [EvaluatedStatePerPiece|Rest]) :-
    evaluate_states_rotation_per_piece(SimulatedGameStatePerPiece, CurrentPlayer, EvaluatedStatePerPiece),
    evaluate_states_rotation(SimulatedGameState, CurrentPlayer, Rest).

evaluate_states_rotation_per_piece([],_,[]).
evaluate_states_rotation_per_piece([SimulatedGameState|SimulatedGameStates], CurrentPlayer, [Value|Rest]) :-
    value(SimulatedGameState, CurrentPlayer, Value),
    evaluate_states_rotation_per_piece(SimulatedGameStates, CurrentPlayer, Rest).

get_chosen_game_state(GameStates, Index, NewGameState) :-
    flatten(GameStates, FlatGameStates),
    length(FlatGameStates, Len),
    NewIndex is Index + 1,
    idx(NewIndex, FlatGameStates, NewGameState).

% ------------------------------------------------------------------------------------------------- %

% ------------------------------------- DUMB BOT HELPERS ------------------------------------------ %
dumb_bot_turn(GameState, NewGameState) :-

    random_rotate(GameState, RotatedGameState),
    busy_wait(50000000),
    display_game(RotatedGameState),
    busy_wait(100000000),
    bot_round(1, RotatedGameState,NewGameState).
    

random_rotate(GameState, RotatedGameState) :-
    GameState = state(Board,_,_,_),
    length(Board, RowCount),
    idx(1, Board, Row),
    length(Row, ColumnCount),
    numlist(1, RowCount, RowList),
    letterlist(1, ColumnCount, ColumnList),
    append(RowList, ColumnList, ResList),
    my_random_member(Target, ResList),
    handle_rotation_choice(Target, GameState, RotatedGameState).

select_random_move(GameState, NewGameState) :-
    valid_moves(GameState, PossibleMoves),
    length(PossibleMoves, Len),
    generate_empty_lists(Len, Result),
    PossibleMoves \= Result,
    my_random_member(MovePerPiece, PossibleMoves),
    my_random_member(Target, MovePerPiece),
    Target = (_, piece(Position, Idx)),
    Final = (Idx, piece(Position, Idx)),
    move(GameState, Final, NewGameState).
select_random_move(GameState, NewGameState) :-
    valid_moves(GameState, PossibleMoves),
    length(PossibleMoves, Len),
    generate_empty_lists(Len, Result),
    PossibleMoves = Result,
    NewGameState = GameState.

% ------------------------------------------------------------------------------------------------- %
% ---------------------------------------- BOT HELPERS -------------------------------------------- %
bot_round(Round, GameState, FinalGameState) :-
    Round<3,
    Round>0,
    NewRound is (Round + 1),
    display_game(GameState),
    select_random_move(GameState, NewGameState),
    check_end_game(NewGameState),
    bot_round(NewRound, NewGameState, FinalGameState).
bot_round(Round, GameState, FinalGameState) :-
    Round = 3,
    display_game(GameState),
    select_random_move(GameState, NewGameState),
    check_end_game(NewGameState),
    FinalGameState = NewGameState.
bot_round(4, GameState, GameState).

smart_bot_round(Round, GameState, FinalGameState) :-
    Round<3,
    Round>1,
    NewRound is (Round + 1),
    busy_wait(400000000),
    calculate_move_no_rotation(GameState, NewGameState),
    display_game(NewGameState),
    busy_wait(400000000),
    check_end_game(NewGameState),
    smart_bot_round(NewRound, NewGameState, FinalGameState).
smart_bot_round(Round, GameState, FinalGameState) :-
    Round = 3,
    calculate_move_no_rotation(GameState, NewGameState),
    display_game(NewGameState),
    busy_wait(400000000),
    check_end_game(NewGameState),
    FinalGameState = NewGameState.
smart_bot_round(4, GameState, GameState).
% ------------------------------------------------------------------------------------------------- %

choose_move(GameState, Level, Move) :-
    Level = dumbbot,
    dumb_bot_turn(GameState, Move).
choose_move(GameState, Level, Move) :-
    Level = smartbot,
    smart_bot_turn(GameState, Move).
