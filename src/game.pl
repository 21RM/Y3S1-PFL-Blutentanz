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
    TemporaryGameConfig = config(
        [5,5],
        [4, 4],
        [human, human]
    ),
    initial_state(TemporaryGameConfig, GameState), % Initialize the game state (TODO::Dynamic configurations).
    display_game(GameState), % Display the board
    game_play(GameState). % Start the game loop.
% ----------------------------------------------------------------------------------------------- %

% ------------------------------------ INITIALIZE GAME STATE ------------------------------------ %
% --> Initialize the game state with a 4x4 board.
initial_state(config([Player1Pieces,Player2Pieces],[Rows, Columns], [Player1Type, Player2Type]), GameState) :-
    % -> Initialize pieces.
    initialize_pieces(Player1PiecesCount, Player2PiecesCount, Player1Pieces,Player2Pieces),
    
    % -> Generate the board.
    generate_random_board(Rows, Columns, Board),

    % -> Initialize players.
    initialize_players(Player1Type, Player2Type,Player1Pieces,Player2Pieces, Players),

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
    create_pieces(Player2PiecesCount, Player2Pieces).


% Base case: When Count is 0, return an empty list.
create_pieces(0, []) :- !.

% Recursive case: Add [[0,0], [0,0]] to the result list.
create_pieces(Count, [[[0, 0], [0, 0]] | Rest]) :-
    Count > 0,
    NextCount is Count - 1,
    create_pieces(NextCount, Rest).

% --> Initialize Players, with types and colors.
initialize_players(Player1Type, Player2Type, Pieces, [Player1, Player2]) :-
    Player1 = player(player1, Player1Type, orange, Player1Pieces),
    Player2 = player(player2, Player2Type, blue, Player2Pieces).
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
game_loop(GameState) :-
    display_game(GameState), % - presented up, in its own comment encapsulation.
    GameState = state(_,_, CurrentPlayer),

    % -> Execute the current player's turn
    take_turn(CurrentPlayer, GameState, UpdatedGameState).
% ----------------------------------------------------------------------------------------------- %

% ------------------------------------ GAME LOOP HELPERS ---------------------------------------- %
take_turn(CurrentPlayer, GameState, UpdatedGameState) :-
    write('Rotation Phase for '), % display_current_player(CurrentPlayer), nl, (TODO::Display player name)
    rotation_phase(GameState, RotatedGameState),
    UpdatedGameState = RotatedGameState.

% --> Rotation Phase.
rotation_phase(GameState, RotatedGameState) :-
    GameState = state(Board, Players, CurrentPlayer),
    write('Select a row to rotate: '),
    read(RowIndex),
    write('Rotating row...'), nl,
    rotate_row(RowIndex, Board, RotatedBoard),
    write('Row index: '), write(RowIndex), nl,
    RotatedGameState = state(RotatedBoard, Players, CurrentPlayer),
    write(RotatedBoard), nl,
    display_game(RotatedGameState),
    write('Tile rotated successfully!'), nl.
% ----------------------------------------------------------------------------------------------- %

