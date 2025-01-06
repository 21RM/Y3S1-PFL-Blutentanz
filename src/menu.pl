

:- module(menu, [main_menu/1, display_menu_options/2, read_choice/1]).

:- use_module('board.pl').
:- use_module('list.pl').
:- use_module('display.pl').
:- use_module('game.pl').

default_config(config([5, 5], [4, 4], [human, human], [4, 4])).

% ---------------------------------------------- MENU  ------------------------------------------ %
% Main menu logic
main_menu(GameConfig) :-
    clear_screen,
    Options = ['Start Game', 'Settings', 'Exit'],
    Title = 'Main Menu',
    menu_loop(Options, Title, GameConfig).

menu_loop(Options,Title, GameConfig) :-
    print_title, nl, nl,
    display_menu(Options, Title),
    write('Enter the number of your choice: '),
    read_choice(Choice),
    process_choice(Choice, Options, Title, GameConfig).

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

validate_choice(Number, Choice) :-
    \+ integer(Number),
    write('Invalid input. Please enter a number. '),
    read_choice(Choice).

% Process the selected option
process_choice(Choice, Options, Title, GameConfig) :-
    length(Options, Len),
    valid_choice(Choice, Len, Options, Title, GameConfig).

valid_choice(Choice, Len, Options, Title, GameConfig) :-
    Choice > 0,
    Choice =< Len,
    idx(Choice, Options, SelectedOption),
    handle_selection(SelectedOption, GameConfig).

valid_choice(_, _, Options, Title, GameConfig) :-
    write('Invalid choice. Try again.\n'),
    menu_loop(Options, Title, GameConfig).
% ----------------------------------------------------------------------------------------------- %



% ---------------------------------------- SETTINGS MENU ---------------------------------------- %
settings_menu(GameConfig) :-
    clear_screen,
    Options = ['Board size', 'Number of pieces', 'Score to Win', 'Back to Main Menu'],
    Title = 'Settings',
    menu_loop(Options, Title, GameConfig).
% ----------------------------------------------------------------------------------------------- %




% --------------------------------------- START GAME MENU --------------------------------------- %

start_game_menu(GameConfig) :-
    clear_screen,
    Options = ['Human vs Human', 'Human vs Bot', 'Bot vs Bot', 'Back to Main Menu'],
    Title = 'Start Game',
    menu_loop(Options, Title, GameConfig).
% ----------------------------------------------------------------------------------------------- %



% ------------------------------------ HANDLE MENU SELECTION ------------------------------------ %

% Handle each menu option
handle_selection('Start Game', GameConfig) :-
    start_game_menu(GameConfig).
handle_selection('Settings', GameConfig) :-
    settings_menu(GameConfig).
handle_selection('Exit', _) :-
    clear_screen,
    write('Exiting the game. Goodbye!\n'),
    halt.
handle_selection('Board size', GameConfig) :-
    write('Here you can choose the height and width of your board.\n'),
    settings_menu(GameConfig).
handle_selection('Number of pieces', GameConfig) :-
    write('Here you can choose the number of game pieces per player.\n'),
    settings_menu(GameConfig).
handle_selection('Score to Win', GameConfig) :-
    write('Here you can choose the number of pieces necessary to win.\n'),
    settings_menu(GameConfig).
handle_selection('Back to Main Menu', GameConfig) :-
    main_menu(GameConfig).
handle_selection('Human vs Human', NewGameConfig) :-
    default_config(GameConfig),
    GameConfig = config(Pieces, Size,_,ToWin),
    NewGameConfig = config(Pieces, Size, [human, human], ToWin).
handle_selection('Human vs Bot', NewGameConfig):-
    default_config(GameConfig),
    GameConfig = config(Pieces, Size,_,ToWin),
    NewGameConfig = config(Pieces, Size, [human, bot], ToWin).
handle_selection('Bot vs Bot', NewGameConfig) :-
    default_config(GameConfig),
    GameConfig = config(Pieces, Size,_,ToWin),
    NewGameConfig = config(Pieces, Size, [bot, bot], ToWin).
% ----------------------------------------------------------------------------------------------- %
