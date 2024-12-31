% =============================================================================================== %
% ======     THIS FILE CONTAINS THE PREDICATES RELATED TO TILES AND BOARD GENERATION       ====== %
% =============================================================================================== %

:- module(board, [display_board/1]).

% ------------------ True color escape sequences for a brown background ------------------------- %
bg_light_brown('\e[48;2;235;235;210m').      % Light wood (BurlyWood)
bg_brown('\e[48;2;189;119;69m').       % Medium wood (SaddleBrown)
text_brown('\e[38;2;189;119;69m').  % Text in brown (SaddleBrown)
reset_color('\e[0m').                 % Reset to default
% ----------------------------------------------------------------------------------------------- %

% -------------------- Unicode characters using their character codes --------------------------- %
% --> Corners
unicode_top_left(Char) :- char_code(Char, 0x250C). % ┌
unicode_top_right(Char) :- char_code(Char, 0x2510). % ┐
unicode_bottom_left(Char) :- char_code(Char, 0x2514). % └
unicode_bottom_right(Char) :- char_code(Char, 0x2518). % ┘

% --> Horizontal and vertical lines
unicode_horizontal(Char) :- char_code(Char, 0x2500). % ─
unicode_vertical(Char) :- char_code(Char, 0x2502).   % │

% --> Intersections
unicode_cross(Char) :- char_code(Char, 0x253C).     % ┼
unicode_t_up(Char) :- char_code(Char, 0x252C).      % ┬
unicode_t_down(Char) :- char_code(Char, 0x2534).    % ┴
unicode_t_left(Char) :- char_code(Char, 0x251C).    % ├
unicode_t_right(Char) :- char_code(Char, 0x2524).   % ┤

% Define colored circles
color_symbol(orange, Char) :- char_code(Char, 0x1F7E0). % 🟠
color_symbol(blue, Char) :- char_code(Char, 0x1F535).  % 🔵
color_symbol(gray, Char) :- char_code(Char, 0x26AB).   % ⚫
color_symbol(empty, '  ').                             % Empty (two spaces for alignment)

% Diagonal division characters
upper_semi(Char) :- char_code(Char, 0x2580). % ▀
lower_semi(Char) :- char_code(Char, 0x2584).  % ▄
full(Char) :- char_code(Char, 0x2588). % █
% ----------------------------------------------------------------------------------------------- %



% -------------------------------------- ROTATE A TILE ------------------------------------------ %
% --> Main predicate to rotate a tile.
rotate_tile(tile(Sides, Rotation), tile(RotatedSides, NewRotation)) :-
    NewRotation is (Rotation + 90) mod 360, % - Rotating 90 degrees, this may be changed later to handle both direction rotations.
    rotated_sides(Sides, Rotation, RotatedSides).
% ----------------------------------------------------------------------------------------------- %

% ------------------------------------- GET ROTATED SIDES --------------------------------------- %
% --> Helper to rotate sides of a tile, it calculates de steps needed and then calls its little helper to truly rotate the sides.
rotated_sides(Sides, Rotation, RotatedSides) :-
    Steps is Rotation // 90, % - Calculate the number of 90º steps.
    rotate_list(Sides, Steps, RotatedSides).

% --> Helper that rotates a list on a number of steps.
rotate_list(List, 0, List). % - No rotation needed.
rotate_list([H | T], Steps, RotatedList) :-
    Steps > 0,
    append(T, [H], TempList), % - Moving the head to the end.
    NextSteps is Steps -1, % - Decremeting the number of steps.
    rotate_list(TempList, NextSteps, RotatedList). % - Recursive call to rotate it multiple times.
% ----------------------------------------------------------------------------------------------- %



% -------------------------------------- DISPLAY BOARD ------------------------------------------ %
% --> Main predicate to display the entire board.
display_board(Board) :-
    length(Board, RowCount), % - Determine the number of rows.
    RowWidth is RowCount * 12, % - Width based on the number of tiles.
    print_top_border(RowWidth), % - Top border.
    display_rows(Board), % - Call to Helper function that displays all the rows.
    print_bottom_border(RowWidth). % - Bottom border.


% --> Print the top border.
print_top_border(RowWidth) :-
    bg_brown(Brown),
    reset_color(Reset),
    text_brown(TxtBrown),
    unicode_top_left(TopLeft),
    unicode_horizontal(Horizontal),
    unicode_top_right(TopRight),
    unicode_t_up(Tup),
    write(Brown), write(TxtBrown), write(TopLeft),
    print_row_with_crosses(Horizontal, Tup, RowWidth),
    write(TopRight), write(Reset), % Write the right corner
    nl. % Newline

% --> Print the bottom border.
print_bottom_border(RowWidth) :-
    bg_brown(Brown),
    reset_color(Reset),
    text_brown(TxtBrown),
    unicode_bottom_left(BottomLeft),
    unicode_horizontal(Horizontal),
    unicode_bottom_right(BottomRight),
    unicode_t_down(Tdown),
    write(Brown), write(TxtBrown), write(BottomLeft),
    print_row_with_crosses(Horizontal, Tdown, RowWidth),
    write(BottomRight), write(Reset),% Write the right corner
    nl. % Newline.

% --> Helper predicate to repeat characters.
print_repeated(_, 0).
print_repeated(Char, Count) :-
    Count > 0,
    write(Char),
    NewCount is Count - 1,
    print_repeated(Char, NewCount).

% --> Helper to display all rows.
display_rows([]). % - Base case, no rows left.
display_rows([Row]) :-
    display_row(Row).
display_rows([Row | Rest]) :-
    display_row(Row), % - Helper to display a single row.
    length(Row, NumTiles),
    RowWidth is NumTiles * 12, % - Calculate row width.
    print_row_separator(RowWidth),
    display_rows(Rest). % - Recursive call for the next rows.

% --> Print a row separator with correct Unicode symbols.
print_row_separator(Width) :-
    bg_brown(Brown),
    reset_color(Reset),
    text_brown(TxtBrown),
    unicode_t_left(TLeft),
    unicode_horizontal(Horizontal),
    unicode_cross(Cross),
    unicode_t_right(TRight),
    write(Brown), write(TxtBrown), write(TLeft),
    print_row_with_crosses(Horizontal, Cross, Width),
    write(TRight), write(Reset), nl.

%--> Print a repeated horizontal line with crosses at intervals
print_row_with_crosses(_,_,0). % - Base case, no more width to print.
print_row_with_crosses(Horizontal, _, Width) :- % Base case: if the remaining width is less than the required block size
    Width =< 12,
    print_repeated(Horizontal, Width).
print_row_with_crosses(Horizontal, Cross, Width) :-
    Width > 1,
    print_repeated(Horizontal, 11),
    write(Cross),
    RemainingWidth is Width - 12,
    print_row_with_crosses(Horizontal, Cross, RemainingWidth).

% --> Helper to display a single row of tiles.
display_row(Row) :-
    upper_semi(Us),
    lower_semi(Ls),
    full(F),
    spacer(Row, F, Us),
    display_row_top(Row), % - displays an entire top part of a row.
    spacer(Row, ' ', ' '),
    display_row_bottom(Row), % - displays an entire bottom part of a row.
    spacer(Row, F, Ls).

spacer([], _, _) :-
    unicode_vertical(Vertical),
    bg_brown(Brown),
    text_brown(TxtBrown),
    reset_color(Reset), 
    write(Reset), write(Brown), write(' '), write(TxtBrown), write(Vertical), write(Reset),
    nl.
spacer([Tile | Rest], F1, F2) :-
    bg_brown(Brown),
    bg_light_brown(LBrown),
    text_brown(TxtBrown),
    reset_color(Reset), 
    color_symbol(BottomLeft, LeftSymbol),
    color_symbol(BottomRight, RightSymbol),
    unicode_vertical(Vertical),
    write(Brown), write(TxtBrown), write(Vertical), write(' '), write(Reset), write(LBrown), write(TxtBrown), 
    write(F1), write(F2), write(Reset), write(LBrown), write('      '), write(TxtBrown), write(F2), write(F1), write(Reset),        % - Display the symbols.
    spacer(Rest, F1, F2).

% --> Helper to display the top part of tiles in a row.
display_row_top([]) :-
    unicode_vertical(Vertical),
    bg_brown(Brown),
    text_brown(TxtBrown),
    reset_color(Reset), 
    write(Reset), write(Brown), write(' '), write(TxtBrown), write(Vertical), write(Reset),
    nl. % - Base case: No more tiles, end the row.
display_row_top([tile(Sides, Rotation) | Rest]) :- % - Capture first tile on the list of tiles.
    rotated_sides(Sides, Rotation, [TopLeft, TopRight, _, _]), % - Extract top sides.
    bg_brown(Brown),
    bg_light_brown(LBrown),
    text_brown(TxtBrown),
    reset_color(Reset),
    color_symbol(TopLeft, LeftSymbol),
    color_symbol(TopRight, RightSymbol),
    unicode_vertical(Vertical),  
    write(Brown), write(TxtBrown), write(Vertical), write(' '), write(Reset), write(LBrown), write('  '), write(LeftSymbol), write('  '), write(RightSymbol), write('  '),% - Display the symbols.
    display_row_top(Rest). % - Recurse for the rest of the tiles

% --> Helper to display the bottom part of tiles in a row.
display_row_bottom([]) :-
    unicode_vertical(Vertical),
    bg_brown(Brown),
    text_brown(TxtBrown),
    reset_color(Reset), 
    write(Reset), write(Brown), write(' '), write(TxtBrown), write(Vertical), write(Reset),
    nl. % - Base case: No more tiles, end the row.
display_row_bottom([tile(Sides, Rotation) | Rest]) :- % - Process each tile.
    rotated_sides(Sides, Rotation, [_, _, BottomLeft, BottomRight]), % - Extract bottom sides.
    bg_brown(Brown),
    bg_light_brown(LBrown),
    text_brown(TxtBrown),
    reset_color(Reset), 
    color_symbol(BottomLeft, LeftSymbol),
    color_symbol(BottomRight, RightSymbol),
    unicode_vertical(Vertical),
    write(Brown), write(TxtBrown), write(Vertical), write(' '), write(Reset), write(LBrown), write('  '), write(LeftSymbol), write('  '), write(RightSymbol), write('  '), % - Display the symbols.
    display_row_bottom(Rest). % - Recurse for the rest of the tiles.
% ----------------------------------------------------------------------------------------------- %
