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
    main_menu,
    write('Starting the game, and out of the menu...'), nl,
    TemporaryGameConfig = config(
        [5,5],
        [4, 4],
        [human, human]
    ),
    initial_state(TemporaryGameConfig, GameState), % Initialize the game state (TODO::Dynamic configurations).
    write('state initialized...'), nl,
    display_game(GameState), % Display the board
    game_loop(GameState). % Start the game loop.
% ----------------------------------------------------------------------------------------------- %

% ------------------------------------ INITIALIZE GAME STATE ------------------------------------ %
% --> Initialize the game state with a 4x4 board.
initial_state(config([Player1PiecesCount,Player2PiecesCount],[Rows, Columns], [Player1Type, Player2Type]), GameState) :-
    write('Initializing game state...'), nl,
    % -> Initialize pieces.
    initialize_pieces(Player1PiecesCount, Player2PiecesCount, Player1Pieces,Player2Pieces),
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
    GameState = state(Board, Players, CurrentPlayer).
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
    create_pieces(Player1PiecesCount, Player1Pieces),
    create_pieces(Player2PiecesCount, Player2Pieces),
    write('Pieces initialized...'), nl,
    write(Player1Pieces), nl,
    write(Player2Pieces), nl.


% Base case: When Count is 0, return an empty list.
create_pieces(0, []) :- !.
% Recursive case: Add [[0,0], [0,0]] to the result list.
create_pieces(Count, [[[0, 0], [0, 0]] | Rest]) :-
    Count > 0,
    NextCount is Count - 1,
    create_pieces(NextCount, Rest).

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
display_game(state(Board, Players, CurrentPlayer)) :-
    clear_screen,
    %display_current_player(CurrentPlayer),
    %display_player_info(Players),
    display_board(Board).
% ----------------------------------------------------------------------------------------------- %



% ---------------------------------------- GAME LOOP -------------------------------------------- %
% --> Main game loop.
game_loop(GameState) :-
    game_over(GameState, Winner),
    handle_game_over(Winner, GameState),
    !.

game_loop(GameState) :-
    take_turn(GameState, UpdatedGameState),
    write('looping...'), nl,
    game_loop(UpdatedGameState).

game_over(_, none).
game_over(GameState, Winner) :-
    GameState = state(Board, Players, CurrentPlayer),
    %check_win_condition(Board, Players, true). TODO: Implement the win condition.
    Winner = none.

handle_game_over(Winner, _) :-
    Winner \= none.
    %TODO: Implement the game over handling.



% ----------------------------------------------------------------------------------------------- %

% ------------------------------------ GAME LOOP HELPERS ---------------------------------------- %
take_turn(GameState, UpdatedGameState) :-
    rotation_phase(GameState, RotatedGameState),
    UpdatedGameState = RotatedGameState,
    display_game(UpdatedGameState),
    display_possible_moves(UpdatedGameState).

% --> Rotation Phase.
rotation_phase(GameState, RotatedGameState) :-        % TODO: Bug in Capital Letter handling.
    GameState = state(Board, Players, CurrentPlayer),
    write('Rotate a row (numbers), or a column (letters): '),
    read(Selection),
    handle_rotation_choice(Selection, GameState, RotatedGameState).

% --> Handle the rotation choice.
handle_rotation_choice(Selection, state(Board, Players, CurrentPlayer), state(RotatedBoard, Players, CurrentPlayer)) :-
    integer(Selection),
    rotate_row(Selection, Board, RotatedBoard).

handle_rotation_choice(Selection, state(Board, Players, CurrentPlayer), state(RotatedBoard, Players, CurrentPlayer)) :-
    atom(Selection),
    column_letter_to_index(Selection, ColumnIndex),
    rotate_column(ColumnIndex, Board, RotatedBoard).

handle_rotation_choice(_, GameState, RotatedGameState) :-
    write('Invalid input. Please enter a valid row number or column letter.\n'),
    rotation_phase(GameState, RotatedGameState).

% --> Convert a column letter to an index.
column_letter_to_index(ColumnLetter, ColumnIndex) :-
    to_uppercase(ColumnLetter, UppercaseLetter),
    char_code('A', ACode),
    char_code(UppercaseLetter, LetterCode),
    ColumnIndex is LetterCode - ACode + 1.
% ----------------------------------------------------------------------------------------------- %

