% ========================================================================================== %
% =====        THIS FILE CONTAINS THE PREDICATES RELATED TO THE MAIN GAME LOOP         ===== %
% ========================================================================================== %

% ---------------------------------------- INCLUDES --------------------------------------------- %
:- use_module('board.pl').
:- use_module('random.pl').
:- use_module('display.pl').
:- use_module('menu.pl').
% ----------------------------------------------------------------------------------------------- %

% ---------------------------------------- Definitions ------------------------------------------ %
% --> Define the standard tile.
standard_tile([empty, orange, blue, gray]).
% ----------------------------------------------------------------------------------------------- %


% ---------------------------------------- MAIN LOOP -------------------------------------------- %
% --> Main predicate for the game.
play :-
    main_menu,  % Start the main menu
    initial_state(Board),  % Initialize the game state
    display_board(Board).  % Display the board

% ----------------------------------------------------------------------------------------------- %

% ------------------------------------ INITIALIZE GAME STATE ------------------------------------ %
% --> Initialize the game state with a 4x4 board.
initial_state(Board) :-
    generate_random_board(Board).
% ----------------------------------------------------------------------------------------------- %



% ----------------------------------------- Helpers --------------------------------------------- %
% --> Assign a random rotation to a tile.
randomize_tile(tile(Sides, Rotation)) :-
    standard_tile(Sides),                         % - Use the fixed tile colors.
    my_random_member(Rotation, [0, 90, 180, 270]).  % - Randomly select a rotation.

% --> Generate a lxc board with random rotations. (TODO -> for now 4*4 is placeholder)
generate_random_board(Board) :-
    length(Tiles, 16),                     % - Create a list of l*c placeholders.
    maplist(randomize_tile, Tiles),        % - Randomize the rotation of each tile.
    partition_board(Tiles, Board).         % - Partition the tiles into rows.

% --> Partition a flat list into rows of l elements each. (TODO -> for now is 4 as a placeholder)
partition_board([], []).
partition_board(Tiles, [Row | Rest]) :-
    length(Row, 4),                        % - Each row has 4 tiles.
    append(Row, Remaining, Tiles),         % - Extract the first 4 tiles.
    partition_board(Remaining, Rest).      % - Recurse for the rest.

% --> mapList, aplies a predicate to every member of a list
maplist(_, []) :- !.  % Base case: Empty list
maplist(Predicate, [Head | Tail]) :-
    call(Predicate, Head),  % Call the predicate on the current element
    maplist(Predicate, Tail).  % Recurse for the rest of the list
% ----------------------------------------------------------------------------------------------- %
