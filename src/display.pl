% ========================================================================================== %
% =====         THIS FILE CONTAINS OUR TERMINAL AND DISPLAY RELATED PREDICATES         ===== %
% ========================================================================================== %

% ---------------------------------------- Definitions ------------------------------------------ %
:- module(display, [clear_screen/0, print_title/0, validate_input/4 , display_score/5]).

% --> Define block characters for the "P"
block_full(Char) :- char_code(Char, 0x2588). % - █ Full block.
block_empty(Char) :- char_code(Char, 0x0020). % - Space (empty block).
% ----------------------------------------------------------------------------------------------- %


% ---------------------------------------- INCLUDES --------------------------------------------- %
:- use_module(library(system)).
% ----------------------------------------------------------------------------------------------- %


% ------------------ True color escape sequences for a brown background ------------------------- %
text_orange('\e[38;2;255;165;0m').  % - Orange.
text_blue('\e[38;2;0;0;255m').  % - Blue.
text_dark_grey('\e[38;2;35;35;35m').  % - Dark Grey.
bg_light_brown('\e[48;2;235;235;210m').      % - Light wood.
bg_brown('\e[48;2;189;119;69m').       % - Medium wood.
bg_orange('\e[48;2;225;135;0m').      % - Orange.
bg_blue('\e[48;2;0;0;255m').          % - Blue.
text_brown('\e[38;2;189;119;69m').  % - Text in brown.
reset_color('\e[0m').
% ----------------------------------------------------------------------------------------------- %


% --> Clear the terminal screen
clear_screen :-
    format("\e[3J\e[2J\e[H", []),
    format("\e[2J\e[H", []),
    nl.

validate_input(MaxNumber, Text , AcceptList, ReturnNumber) :-
    write(Text),
    read(Input),
    validate_input_value(Input, MaxNumber, Text, AcceptList, ReturnNumber).


validate_input_value(Input, MaxNumber, _, AcceptList, Input) :-
    member(Input, AcceptList), !.  % If Input is in AcceptList, succeed and cut
validate_input_value(Input, MaxNumber, _, _, Input) :-
    integer(Input),                % Ensure Input is an integer
    Input >= 1,                    % Input is greater than or equal to 1
    Input =< MaxNumber.            % Input is less than or equal to MaxNumber
validate_input_value(_, MaxNumber, Text, AcceptList, ReturnNumber) :-
    write('Invalid input. Please enter a number between 1 and '), write(MaxNumber), nl,
    validate_input(MaxNumber, Text, AcceptList, ReturnNumber).


% --> Print the rows of the letter
print_ascii([]) :- % Base case: no rows left to print
    bg_light_brown(BGLbrown),
    bg_brown(BGbrown),
    text_brown(TxtBrown),
    reset_color(Reset),
    format("     ", []),
    write(BGbrown), write(TxtBrown), format("| ", []), write(Reset),
    write(BGLbrown), format("                                                                           ", []), write(Reset),
    write(BGbrown), write(TxtBrown), format(" |", []), write(Reset),
    nl,
    format("     ", []),
    write(BGbrown), write(TxtBrown), format("+                                                                             +", []), write(Reset).

print_ascii([Row | Rest]) :-
    bg_light_brown(BGLbrown),
    bg_brown(BGbrown),
    text_brown(TxtBrown),
    reset_color(Reset),
    length(Row, Characters),
    format("     ", []),
    write(BGbrown), write(TxtBrown), format("| ", []), write(Reset),
    write(BGLbrown), format("   ", []), write(Reset),
    print_row(Row, Characters), % Print the current row
    write(BGLbrown), format("   ", []), write(Reset),
    write(BGbrown), write(TxtBrown), format(" |", []), write(Reset),
    nl,             % New line after each row
    print_ascii(Rest). % Recurse for the rest of the rows


% Base case: no more blocks in the row
print_row([], _).
% Print in orange for Characters > 53
print_row([Block | Rest], Characters) :-
    Characters > 50,
    text_orange(Orange),
    reset_color(Reset),
    bg_light_brown(BGLbrown),
    write(BGLbrown),
    write(Orange),         % Apply orange color
    write(Block),          % Write the block
    write(Reset),          % Reset to default
    NewCharacters is Characters - 1,
    print_row(Rest, NewCharacters).  % Recurse for the rest of the blocks
% Print in dark grey for 32 < Characters < 54
print_row([Block | Rest], Characters) :-
    Characters > 29,
    Characters =< 50,
    text_dark_grey(Gray),
    reset_color(Reset),
    bg_light_brown(BGLbrown),
    write(BGLbrown),
    write(Gray),           % Apply dark grey color
    write(Block),          % Write the block
    write(Reset),          % Reset to default
    NewCharacters is Characters - 1,
    print_row(Rest, NewCharacters).  % Recurse for the rest of the blocks
% Print in blue for Characters <= 32
print_row([Block | Rest], Characters) :-
    Characters =< 29,
    text_blue(Blue),
    reset_color(Reset),
    bg_light_brown(BGLbrown),
    write(BGLbrown),
    write(Blue),           % Apply blue color
    write(Block),          % Write the block
    write(Reset),          % Reset to default
    NewCharacters is Characters - 1,
    print_row(Rest, NewCharacters).  % Recurse for the rest of the blocks   

print_title :-
    ascii_title(Rows),
    bg_light_brown(BGLbrown),
    bg_brown(BGbrown),
    text_brown(TxtBrown),
    reset_color(Reset),
    format("     ", []),
    write(BGbrown), write(TxtBrown), format("+                                                                             +", []), write(Reset), nl,
    format("     ", []),
    write(BGbrown), write(TxtBrown), format("| ", []), write(Reset),
    write(BGLbrown), format("                                                                           ", []), write(Reset),
    write(BGbrown), write(TxtBrown), format(" |", []), write(Reset), nl,
    print_ascii(Rows), nl.

display_score(Players, player(_,_,Color,_), ScorePlayer1, ScorePlayer2, PiecesToWin) :-
    Color = orange,
    bg_light_brown(BGLbrown),
    bg_brown(BGbrown),
    bg_orange(Orange),
    text_brown(TxtBrown),
    reset_color(Reset),
    format("     ", []), write(BGbrown), write(TxtBrown), format("| ", []), write(Reset),  write(BGLbrown), format("                                                                 ", []), write(Reset), write(Orange), write('          '), write(Reset), write(BGbrown), write(TxtBrown), format(" |", []), write(Reset), nl,
    nl, nl.
display_score(Players, player(_,_,Color,_), ScorePlayer1, ScorePlayer2, PiecesToWin) :-
    Color = blue,
    bg_light_brown(BGLbrown),
    bg_brown(BGbrown),
    bg_blue(Blue),
    text_brown(TxtBrown),
    reset_color(Reset),
    format("     ", []), write(BGbrown), write(TxtBrown), format("| ", []), write(Reset),  write(BGLbrown), format("                                                                 ", []), write(Reset), write(OBlue), write('          '), write(Reset), write(BGbrown), write(TxtBrown), format(" |", []), write(Reset), nl,
    nl, nl.


ascii_title([[Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Full, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full, Empty, Empty, '_', ',', '-', '.', '_', Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full],    [Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '/', Empty, '\\', '_', '/', Empty, '\\', Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty],    [Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '>', '-', '(', '_', ')', '-', '<', Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty],    [Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '\\', '_', '/', Empty, '\\', '_', '/', Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty],    [Full, Full, Empty, Empty, Empty, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, '`', '-', '\'', Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty],    [Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Full, Full, Full, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Empty, '|', Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full]]) :-
    block_full(Full),
    block_empty(Empty).

%ascii_title([[Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Full, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full, Empty, Empty, '_', ',', '-', '.', '_', Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full],    [Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '/', Empty, '\\', '_', '/', Empty, '\\', Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty],    [Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '>', '-', '(', '_', ')', '-', '<', Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty],    [Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '\\', '_', '/', Empty, '\\', '_', '/', Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty],    [Full, Full, Empty, Empty, Empty, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, '`', '-', '\'', Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty],    [Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Full, Full, Full, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Empty, '|', Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full]]) :-
%ascii_title([[Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Full, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full, Empty, Empty, Empty, Empty, '.', '-', '-', '.', Empty, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full],    [Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '|', '\\', Empty, '|', Empty, Empty, Empty, '`', '.', Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty],    [Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '\\', Empty, '|', Empty, Empty, '(', '\\', '_', '_', '\\', Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty],    [Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, '\\', '|', Empty, ':', '=', ')', '_', '_', ')', '>', Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty],    [Full, Full, Empty, Empty, Empty, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, '|', Empty, Empty, '(', '/', Empty, Empty, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty],    [Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Full, Full, Full, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, '|', Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full]]) :-
