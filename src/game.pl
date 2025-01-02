% ========================================================================================== %
% =====        THIS FILE CONTAINS THE PREDICATES RELATED TO THE MAIN GAME LOOP         ===== %
% ========================================================================================== %

:- use_module('board.pl').
:- use_module('menu.pl').


% ---------------------------------------- MAIN LOOP -------------------------------------------- %
% --> Main predicate for the game.
play :-
    main_menu.  % Start the main menu

% ----------------------------------------------------------------------------------------------- %

% ------------------------------------ INITIALIZE GAME STATE ------------------------------------ %
% --> Initialize the game state with a 4x4 board.
initial_state([
    [tile([orange, blue, gray, empty], 0), tile([blue, orange, empty, gray], 90), tile([gray, empty, blue, orange], 180), tile([empty, gray, orange, blue], 270)],
    [tile([blue, gray, orange, empty], 0), tile([orange, empty, gray, blue], 90), tile([empty, orange, blue, gray], 180), tile([gray, blue, empty, orange], 270)],
    [tile([gray, orange, empty, blue], 0), tile([blue, empty, orange, gray], 90), tile([orange, gray, blue, empty], 180), tile([empty, blue, gray, orange], 270)],
    [tile([empty, gray, blue, orange], 0), tile([blue, orange, empty, gray], 90), tile([orange, empty, gray, blue], 180), tile([gray, blue, orange, empty], 270)]
]).
% ----------------------------------------------------------------------------------------------- % 