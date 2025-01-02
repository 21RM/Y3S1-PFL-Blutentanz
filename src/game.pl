% ========================================================================================== %
% =====        THIS FILE CONTAINS THE PREDICATES RELATED TO THE MAIN GAME LOOP         ===== %
% ========================================================================================== %

% ---------------------------------------- INCLUDES --------------------------------------------- %
:- use_module('board.pl').
:- use_module('random.pl').
:- use_module('display.pl').
:- use_module('list.pl').
% ----------------------------------------------------------------------------------------------- %

% ---------------------------------------- Definitions ------------------------------------------ %
% --> Define the standard tile.
standard_tile([empty, orange, blue, gray]).
% ----------------------------------------------------------------------------------------------- %



% --------------------------------------- MAIN PREDICATE ---------------------------------------- %
% --> Main predicate for the game.
play :-
    TemporaryGameConfig = config(
        [4, 4],
        [human, human]
    ),
    initial_state(TemporaryGameConfig, GameState), % Initialize the game state (TODO::Dynamic configurations).
    display_game(GameState). % Display the board
% ----------------------------------------------------------------------------------------------- %



% ------------------------------------ INITIALIZE GAME STATE ------------------------------------ %
% --> Initialize the game state with a 4x4 board.
initial_state(config([Rows, Columns], [Player1Type, Player2Type]), GameState) :-
    % -> Generate the board.
    generate_random_board(Rows, Columns, Board),

    % -> Initialize players.
    initialize_players(Player1Type, Player2Type, Players),

    % -> Set player 1 has starter, orange starts first.
    idx(1, Players, CurrentPlayer),

    % -> Return value GameState, contains Board, Players and the player who starts
    GameState = state(Board, Players, CurrentPlayer).
% ----------------------------------------------------------------------------------------------- %



% ---------------------------------------- DISPLAY GAME ----------------------------------------- %
% --> Main display game predicate.
display_game(state(Board, Players, CurrentPlayer)) :-
    clear_screen,
    %display_current_player(CurrentPlayer),
    %display_player_info(Players),
    display_board(Board).
% ----------------------------------------------------------------------------------------------- %



% ----------------------------------------- Helpers --------------------------------------------- %
% --> Assign a random rotation to a tile.
randomize_tile(tile(Sides, Rotation)) :-
    standard_tile(Sides),                         % - Use the fixed tile colors.
    my_random_member(Rotation, [0, 90, 180, 270]).  % - Randomly select a rotation.

% --> Generate a lxc board with random rotations. (TODO -> for now 4*4 is placeholder)
generate_random_board(Rows, Columns, Board) :-
    TotalTiles is Rows * Columns,
    length(Tiles, TotalTiles),                     % - Create a list of l*c placeholders.
    maplist(randomize_tile, Tiles),        % - Randomize the rotation of each tile.
    partition_board(Columns, Tiles, Board).         % - Partition the tiles into rows.

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
initialize_players(Player1Type, Player2Type, [Player1, Player2]) :-
    Player1 = player(player1, Player1Type, orange, pieces([])),
    Player2 = player(player2, Player2Type, blue, pieces([])).
% ----------------------------------------------------------------------------------------------- %
