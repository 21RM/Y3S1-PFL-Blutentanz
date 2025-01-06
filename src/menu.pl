:- module(menu, [main_menu/1, display_menu_options/2, read_choice/1]).

:- use_module('board.pl').
:- use_module('list.pl').
:- use_module('display.pl').
:- use_module('game.pl').

default_config(config([5, 5], [4, 4], [human, human], [4, 4])).

% ---------------------------------------------- MENU  ------------------------------------------ %
% Main menu logic
main_menu(NewGameConfig) :-
    clear_screen,
    Options = ['Start Game', 'Settings', 'Exit'],
    Title = 'Main Menu',
    menu_loop(Options, Title, GameConfig, NewGameConfig).

menu_loop(Options,Title, GameConfig, NewGameConfig) :-
    print_title, nl, nl,
    display_menu(Options, Title),
    write('Enter the number of your choice: '),
    read_choice(Choice),
    process_choice(Choice, Options, Title, GameConfig, NewGameConfig).

% Display menu with numbered options
display_menu(Options, Title) :-
    nl,
    write('======== '),
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
process_choice(Choice, Options, Title, GameConfig,NewGameConfig) :-
    length(Options, Len),
    valid_choice(Choice, Len, Options, Title, GameConfig, NewGameConfig).

valid_choice(Choice, Len, Options, Title, GameConfig, NewGameConfig) :-
    Choice > 0,
    Choice =< Len,
    idx(Choice, Options, SelectedOption),
    handle_selection(SelectedOption, GameConfig,NewGameConfig).

valid_choice(_, _, Options, Title, GameConfig, NewGameConfig) :-
    write('Invalid choice. Try again.\n'),
    menu_loop(Options, Title, GameConfig, NewGameConfig).
% ----------------------------------------------------------------------------------------------- %


% ---------------------------------------- SETTINGS MENU ---------------------------------------- %
% Settings menu with dynamic updates
% Settings menu with dynamic updates
/*settings_menu(GameConfig) :-
    clear_screen,
    write('Settings Menu\n'),
    update_setting(GameConfig, board_size, 'Enter board size (height, width): ', NewGameConfig1),
    update_setting(NewGameConfig1, number_of_pieces, 'Enter number of pieces per player: ', NewGameConfig2),
    update_setting(NewGameConfig2, score_to_win, 'Enter score to win: ', NewGameConfig),
    write('Settings updated successfully.\n'),
    write_current_settings(NewGameConfig),
    pause_for_input,
    main_menu(NewGameConfig).

% Update a specific setting
update_setting(config(BoardSize, NumPieces, Players, ToWin), SettingKey, Prompt, config(NewBoardSize, NewNumPieces, Players, NewToWin)) :-
    default_config(config(DefaultBoardSize, DefaultNumPieces, _, DefaultToWin)),
    ( SettingKey = board_size ->
        write(Prompt),
        read(Input),
        ( Input = [] ->
            NewBoardSize = DefaultBoardSize
        ; Input = [Height, Width],
          NewBoardSize = [Height, Width]
        ),
        NewNumPieces = NumPieces, NewToWin = ToWin
    ; SettingKey = number_of_pieces ->
        write(Prompt),
        read(Input),
        ( Input = [] ->
            NewNumPieces = DefaultNumPieces
        ; NewNumPieces = [Input, Input]
        ),
        NewBoardSize = BoardSize, NewToWin = ToWin
    ; SettingKey = score_to_win ->
        write(Prompt),
        read(Input),
        ( Input = [] ->
            NewToWin = DefaultToWin
        ; NewToWin = [Input, Input]
        ),
        NewBoardSize = BoardSize, NewNumPieces = NumPieces
    ).

% Display current settings
write_current_settings(config(BoardSize, NumPieces, Players, ToWin)) :-
    write('Current Settings:\n'),
    format('Board Size: ~w\n', [BoardSize]),
    format('Number of Pieces: ~w\n', [NumPieces]),
    format('Score to Win: ~w\n', [ToWin]).

% Pause to allow player to view updated settings
pause_for_input :-
    write('Press Enter to return to the main menu...'),
    get_char(_), !.*/
% ----------------------------------------------------------------------------------------------- %


% --------------------------------------- START GAME MENU --------------------------------------- %

start_game_menu(GameConfig,NewGameConfig) :-
    clear_screen,
    Options = ['Human vs Human', 'Human vs Bot', 'Bot vs Bot', 'Back to Main Menu'],
    Title = 'Start Game',
    menu_loop(Options, Title, GameConfig, NewGameConfig).
% ----------------------------------------------------------------------------------------------- %



% ------------------------------------ COLOR SELECTION MENU ------------------------------------- %

select_color_menu(GameConfig,NewGameConfig) :-
    clear_screen,
    Options = ['Orange (starts first)', 'Blue'],
    Title = 'Select the color you want to play with',
    menu_loop(Options, Title, GameConfig, NewGameConfig).
% ----------------------------------------------------------------------------------------------- %


% ------------------------------------ DIFICULTY SELECTION MENU ---------------------------------- %

select_dificulty_menu(GameConfig,NewGameConfig) :-
    clear_screen,
    Options = ['Dumb Bot', 'Smart Bot'],
    Title = 'Select the difficulty',
    menu_loop(Options, Title, GameConfig, NewGameConfig).
% ----------------------------------------------------------------------------------------------- %


% ------------------------------------ HANDLE MENU SELECTION ------------------------------------ %

% Handle each menu option
handle_selection('Start Game', GameConfig,NewGameConfig) :-
    start_game_menu(GameConfig,NewGameConfig).
%handle_selection('Settings', GameConfig,_) :-
 %   settings_menu(GameConfig).
handle_selection('Exit', _,_) :-
    clear_screen,
    write('Exiting the game. Goodbye!\n'),
    halt.
handle_selection('Back to Main Menu',_,NewGameConfig) :-
    main_menu(NewGameConfig).
handle_selection('Human vs Human',GameConfig, NewGameConfig) :-
    default_config(GameConfig),
    GameConfig = config(Pieces, Size,_,ToWin),
    NewGameConfig = config(Pieces, Size, [human, human], ToWin).
% 1
handle_selection('Human vs Bot',GameConfig, NewGameConfig):-
    default_config(GameConfig),
    select_color_menu(GameConfig, NewGameConfig).
% 2
handle_selection('Orange (starts first)', GameConfig, NewGameConfig) :- 
    GameConfig = config(Pieces, Size, _,ToWin),
    GameConfig2 = config(Pieces, Size, [human, bot], ToWin),
    select_dificulty_menu(GameConfig2, NewGameConfig).
% 3
handle_selection('Dumb Bot', GameConfig, NewGameConfig) :-
    GameConfig = config(Pieces, Size, [human, bot],ToWin),
    NewGameConfig = config(Pieces, Size, [human, dumbbot], ToWin).
% 3
handle_selection('Smart Bot', GameConfig, NewGameConfig) :-
    GameConfig = config(Pieces, Size, [human, bot],ToWin),
    NewGameConfig = config(Pieces, Size, [human, smartbot], ToWin).
% 2
handle_selection('Blue', GameConfig, NewGameConfig) :-
    GameConfig = config(Pieces, Size, _,ToWin),
    GameConfig2 = config(Pieces, Size, [bot, human], ToWin),
    select_dificulty_menu(GameConfig2, NewGameConfig).
% 3
handle_selection('Dumb Bot', GameConfig, NewGameConfig) :-
    GameConfig = config(Pieces, Size, [bot, human],ToWin),
    NewGameConfig = config(Pieces, Size, [dumbbot, human], ToWin).
% 3
handle_selection('Smart Bot', GameConfig, NewGameConfig) :-
    GameConfig = config(Pieces, Size, [bot, human],ToWin),
    NewGameConfig = config(Pieces, Size, [smartbot, human], ToWin).
handle_selection('Bot vs Bot',GameConfig, NewGameConfig) :-
    default_config(GameConfig),
    GameConfig = config(Pieces, Size,_,ToWin),
    NewGameConfig = config(Pieces, Size, [bot, bot], ToWin).

% ----------------------------------------------------------------------------------------------- %