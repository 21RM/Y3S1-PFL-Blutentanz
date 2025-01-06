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
settings_menu(GameConfig, NewGameConfig) :-
    clear_screen,
    write('Settings Menu\n'),
    update_setting(GameConfig, board_size, 'Enter board size in format [height, width], maximum size is [9,9]. \nPress . and Enter to use Default Values: ', NewGameConfig1),
    update_setting(NewGameConfig1, number_of_pieces, 'Enter number of pieces per player, maximum number of pieces can not be bigger then 2 times the width of the board. \nPress . and Enter to use Default Values: ', NewGameConfig2),
    update_setting(NewGameConfig2, score_to_win, 'Enter score to win.Can not be bigger then the number of pieces per player \nPress . and Enter to use Default Values: ', FinalGameConfig),
    write('Settings updated successfully.\n'),
    write_current_settings(FinalGameConfig),
    FinalGameConfig = config(Pieces,Size, _, ToWin),
    ChangedGameConfig = config(Pieces, Size, [human, human], ToWin),
    pause_for_input,
    clear_screen,
    Options = ['Start Game', 'Settings', 'Exit'],
    Title = 'Main Menu',
    menu_loop(Options, Title, ChangedGameConfig, NewGameConfig).

update_setting(config(NumPieces,BoardSize, Players, ToWin), board_size, Prompt, config( NumPieces,NewBoardSize, Players, ToWin)) :-
    default_config(config(_,DefaultBoardSize, _, _)),
    write(Prompt),
    read(Input),
    validate_board_size(Input, DefaultBoardSize, NewBoardSize).
    handle_input_board_size(NewBoardSize, DefaultBoardSize, NewBoardSize).

update_setting(config(NumPieces,BoardSize, Players, ToWin), number_of_pieces, Prompt, config( NewNumPieces,BoardSize, Players, ToWin)) :-
    default_config(config(DefaultNumPieces,_, _, _)),
    idx(1, BoardSize, Width),
    Size is (2 * Width),
    validate_input(Size, Prompt , [d], Input),
    handle_input_number_of_pieces(Input, DefaultNumPieces, NewNumPieces).

update_setting(config(NumPieces,BoardSize, Players, ToWin), score_to_win, Prompt, config( NumPieces,BoardSize, Players, NewToWin)) :-
    default_config(config(_, _, _, DefaultToWin)),
    idx(1, NumPieces, Num),
    validate_input(Num, Prompt , [d], Input),
    handle_input_score_to_win(Input, DefaultToWin, NewToWin).

validate_board_size([], DefaultBoardSize, DefaultBoardSize). % Use default if input is empty
validate_board_size(Input, DefaultBoardSize, DefaultBoardSize):- % Use default if input is empty
    Input = d,
    !.
validate_board_size([Height, Width], _, [Height, Width]) :-
    integer(Height), Height > 0, Height =< 9, % Height must be between 1 and 9
    integer(Width), Width > 0, Width =< 9,   % Width must be between 1 and 9
    !.
validate_board_size(_, DefaultBoardSize, DefaultBoardSize) :- % Invalid input falls back to default
    write('Invalid board size. Using default: '), write(DefaultBoardSize), nl.

% Handle input for board size
handle_input_board_size([], DefaultBoardSize, DefaultBoardSize).
handle_input_board_size([Height, Width], _, [Height, Width]).

% Handle input for number of pieces
handle_input_number_of_pieces([], DefaultNumPieces, DefaultNumPieces).
handle_input_number_of_pieces(d,DefaultNumPieces, DefaultNumPieces).
handle_input_number_of_pieces(Input, _, [Input, Input]):-
    Input\= d.

% Handle input for score to win
handle_input_score_to_win([], DefaultToWin, DefaultToWin).
handle_input_score_to_win(d, DefaultToWin, DefaultToWin).
handle_input_score_to_win(Input, _, [Input, Input]):-
    Input\= d.

% Display current settings
write_current_settings(config(BoardSize, NumPieces, Players, ToWin)) :-
    write('Current Settings:\n'),
    format('Board Size: ~w\n', [BoardSize]),
    format('Number of Pieces: ~w\n', [NumPieces]),
    format('Score to Win: ~w\n', [ToWin]).

% Pause to allow player to view updated settings
pause_for_input :-
    write('Press Enter to return to the main menu...'),
    get_char(_), !.
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
    var(GameConfig),
    default_config(GameConfig),
    start_game_menu(GameConfig,NewGameConfig).
handle_selection('Start Game', GameConfig,NewGameConfig) :-
    \+ var(GameConfig),
    start_game_menu(GameConfig,NewGameConfig).
handle_selection('Settings', GameConfig,NewGameConfig) :-
    settings_menu(GameConfig,NewGameConfig).
handle_selection('Exit', _,_) :-
    clear_screen,
    write('Exiting the game. Goodbye!\n'),
    halt.
handle_selection('Back to Main Menu',GameConfig ,NewGameConfig) :-
    clear_screen,
    Options = ['Start Game', 'Settings', 'Exit'],
    Title = 'Main Menu',
    menu_loop(Options, Title, GameConfig, NewGameConfig).
handle_selection('Human vs Human',GameConfig, NewGameConfig) :-
    GameConfig = config(Pieces, Size,_,ToWin),
    NewGameConfig = config(Pieces, Size, [human, human], ToWin).
% 1
handle_selection('Human vs Bot',GameConfig, NewGameConfig):-
    select_color_menu(GameConfig, NewGameConfig).
handle_selection('Bot vs Bot',GameConfig, NewGameConfig):-
    GameConfig = config(Pieces, Size,_,ToWin),
    GameConfig2 = config(Pieces, Size, [bot,bot], ToWin),
    select_dificulty_menu(GameConfig2, NewGameConfig).
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
handle_selection('Dumb Bot', GameConfig, NewGameConfig) :-
    GameConfig = config(Pieces, Size, Type,ToWin),
    Type = [bot, bot],
    GameConfig2 = config(Pieces, Size, [dumbbot, bot], ToWin),
    select_dificulty_menu(GameConfig2, NewGameConfig).
handle_selection('Smart Bot', GameConfig, NewGameConfig) :-
    GameConfig = config(Pieces, Size, Type,ToWin),
    Type = [bot, bot],
    GameConfig2 = config(Pieces, Size, [smartbot, bot], ToWin),
    select_dificulty_menu(GameConfig2, NewGameConfig).
handle_selection('Dumb Bot', GameConfig, NewGameConfig) :-
    GameConfig = config(Pieces, Size, Type,ToWin),
    Type = [P1,P2],
    Type \= [human, bot],
    Type \= [bot,bot],
    P2 = bot,
    NewGameConfig = config(Pieces, Size, [P1, dumbbot], ToWin).
handle_selection('Smart Bot', GameConfig, NewGameConfig) :-
    GameConfig = config(Pieces, Size, Type,ToWin),
    Type = [P1,P2],
    Type \= [human, bot],
    Type \= [bot,bot],
    P2 = bot,
    NewGameConfig = config(Pieces, Size, [P1, smartbot], ToWin).

% ----------------------------------------------------------------------------------------------- %