% ========================================================================================== %
% =====         THIS FILE CONTAINS OUR TERMINAL AND DISPLAY RELATED PREDICATES         ===== %
% ========================================================================================== %

% ---------------------------------------- Definitions ------------------------------------------ %
:- module(display, [clear_screen/0, print_title/0]).

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
text_brown('\e[38;2;189;119;69m').  % - Text in brown.
reset_color('\e[0m').
% ----------------------------------------------------------------------------------------------- %


% --> Clear the terminal screen
clear_screen :-
    format("\e[3J\e[2J\e[H", []),
    format("\e[2J\e[H", []),
    nl.


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
    print_ascii(Rows),
    nl, nl.

ascii_title([[Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Full, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full, Empty, Empty, '_', ',', '-', '.', '_', Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full],    [Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '/', Empty, '\\', '_', '/', Empty, '\\', Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty],    [Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '>', '-', '(', '_', ')', '-', '<', Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty],    [Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '\\', '_', '/', Empty, '\\', '_', '/', Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty],    [Full, Full, Empty, Empty, Empty, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, '`', '-', '\'', Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty],    [Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Full, Full, Full, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Empty, '|', Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full]]) :-
    block_full(Full),
    block_empty(Empty).

%ascii_title([[Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Full, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full, Empty, Empty, '_', ',', '-', '.', '_', Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full],    [Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '/', Empty, '\\', '_', '/', Empty, '\\', Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty],    [Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '>', '-', '(', '_', ')', '-', '<', Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty],    [Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '\\', '_', '/', Empty, '\\', '_', '/', Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty],    [Full, Full, Empty, Empty, Empty, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, '`', '-', '\'', Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty],    [Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Full, Full, Full, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Empty, '|', Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full]]) :-
%ascii_title([[Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Full, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full, Empty, Empty, Empty, Empty, '.', '-', '-', '.', Empty, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full],    [Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '|', '\\', Empty, '|', Empty, Empty, Empty, '`', '.', Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty],    [Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, '\\', Empty, '|', Empty, Empty, '(', '\\', '_', '_', '\\', Empty, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty],    [Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, '\\', '|', Empty, ':', '=', ')', '_', '_', ')', '>', Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty],    [Full, Full, Empty, Empty, Empty, Full, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, '|', Empty, Empty, '(', '/', Empty, Empty, Empty, Empty, Empty, Full, Full, Empty, Full, Full, Full, Empty, Full, Full, Empty, Empty, Empty, Empty],    [Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Full, Empty, Empty, Full, Full, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Full, Full, Full, Full, Full, Empty, Full, Full, Empty, Empty, Full, Full, Empty, Empty, Empty, Full, Full, Empty, Empty, Empty, Empty, Empty, '|', Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,Empty, Full, Full, Empty, Empty, Full, Full, Empty, Full, Full, Full, Full, Full, Full]]) :-
