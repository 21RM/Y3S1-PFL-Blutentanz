
:- use_module('board.pl').

:- module(menu, [main_menu/0]).

% ------------------------------------ MENU  ------------------------------------ %
% Main menu logic
main_menu :-
    Options = ['Start Game', 'Settings', 'Exit'],
    Title = 'Main Menu',
    menu_loop(Options, Title).

menu_loop(Options,Title) :-
    display_menu(Options, Title),
    write('Enter the number of your choice: '),
    read_choice(Choice),
    process_choice(Choice, Options, Title).

% Display menu with numbered options
display_menu(Options, Title) :-
    nl,
    write('======== ') ,
    write(Title),
    write(' ========\n'),
    display_menu_options(Options, 1),
    nl.

display_menu_options([], _).
display_menu_options([Option | Rest], Index) :-
    format('~d. ~w\n', [Index, Option]),
    NextIndex is Index + 1,
    display_menu_options(Rest, NextIndex).

% Read and validate user input
read_choice(Choice) :-
    read(Number),
    validate_choice(Number, Choice).

validate_choice(Number, Choice) :-
    integer(Number),
    Choice = Number.

validate_choice(_, Choice) :-
    write('Invalid input. Please enter a number.\n'),
    read_choice(Choice).

% Custom nth1/3 to avoid using cuts
nth1_custom(1, [Element | _], Element).
nth1_custom(N, [_ | Rest], Element) :-
    N > 1,
    N1 is N - 1,
    nth1_custom(N1, Rest, Element).

% Process the selected option
process_choice(Choice, Options, Title) :-
    length(Options, Len),
    valid_choice(Choice, Len, Options, Title).

valid_choice(Choice, Len, Options, Title) :-
    Choice > 0,
    Choice =< Len,
    nth1_custom(Choice, Options, SelectedOption),
    handle_selection(SelectedOption).

valid_choice(_, _, Options, Title) :-
    write('Invalid choice. Try again.\n'),
    menu_loop(Options, Title).

% Handle each menu option
handle_selection('Start Game') :-
    write('Let\'s go choose your match...\n'),
    start_game_menu.

handle_selection('Settings') :-
    write('Opening settings...\n'),
    settings_menu.

handle_selection('Exit') :-
    write('Exiting the game. Goodbye!\n'),
    halt.
    

% ------------------------------------ SETTINGS MENU ------------------------------------ %
settings_menu :-
    Options = ['Board size', 'Number of pieces', 'Score to Win', 'Back to Main Menu'],
    Title = 'Settings',
    menu_loop(Options, Title).

handle_selection('Board size') :-
    write('Here you can choose the height and width of your board.\n'),
    settings_menu.

handle_selection('Number of pieces') :-
    write('Here you can choose the number of game pieces per player.\n'),
    settings_menu.

handle_selection('Score to Win') :-
    write('Here you can choose the number of pieces necessary to win.\n'),
    settings_menu.

handle_selection('Back to Main Menu') :-
    main_menu.

% ------------------------------------ START GAME MENU ------------------------------------ %

start_game_menu :-
    Options = ['Human vs Human', 'Human vs Bot', 'Bot vs Bot', 'Back to Main Menu'],
    Title = 'Start Game',
    menu_loop(Options, Title).

handle_selection('Human vs Human') :-
    write('Starting the game...\n'),
    initial_state(GameState),  % Initialize the game state
    display_board(GameState).  % Display the board
    

handle_selection('Human vs Bot') :-
    write('Starting Human vs Bot...\n'),
    % Add logic for loading a game
    start_game_menu.

handle_selection('Bot vs Bot') :-
    write('Starting bot vs bot...\n'),
    % Add logic for multiplayer
    start_game_menu.

handle_selection('Back to Main Menu') :-
    main_menu.