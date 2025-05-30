% =============================================================================================== %
% ======     THIS FILE CONTAINS THE PREDICATES RELATED TO TILES AND BOARD GENERATION       ====== %
% =============================================================================================== %

:- module(board, [display_board/2, rotate_row/7, rotate_column/7, rotated_sides/3, display_bench/2]).
% ----------------------------------------------------------------------------------------------- %


% ---------------------------------------- INCLUDES --------------------------------------------- %
:- use_module('display.pl').
:- use_module('list.pl').
% ----------------------------------------------------------------------------------------------- %


% ------------------ True color escape sequences for a brown background ------------------------- %
bg_light_brown('\e[48;2;235;235;210m').      % Light wood (BurlyWood)
bg_lightest_brown('\e[48;2;255;255;230m').   % Lightest wood (Beige)
bg_brown('\e[48;2;189;119;69m').       % Medium wood (SaddleBrown)
bg_dark_brown('\e[48;2;119;49;0m').    % Dark wood (Sienna)
bg_orange('\e[48;2;225;135;0m').      % - Orange.
bg_blue('\e[48;2;0;0;255m').          % - Blue.
text_dark_brown('\e[38;2;119;49;0m').  % Text in dark brown (Sienna)
text_brown('\e[38;2;189;119;69m').  % Text in brown (SaddleBrown)
text_orange('\e[38;2;225;135;0m').  % - Orange.
text_blue('\e[38;2;0;0;255m').  % - Blue.
text_dark_grey('\e[38;2;35;35;35m').  % - Dark Grey.
text_white('\e[38;2;255;255;255m').  % - White.
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



% -------------------------------------- ROTATE A ROW ------------------------------------------- %
% --> Main predicate to rotate a row.
rotate_row(RowIndex, Board, RotatedBoard, Player1Pieces, Player2Pieces, NewPlayer1Pieces, NewPlayer2Pieces) :-
    length(Board, RowCount), % - Get the number of rows.
    RowToRotate is RowCount - RowIndex + 1, % - Calculate the row to rotate.
    idx(RowToRotate, Board, Row), % - Get the row to rotate.
    rotate_tile_list(Row, RotatedRow), % - Rotate the row.
    rotate_piece_in_row(RowIndex, Player1Pieces, Player2Pieces, NewPlayer1Pieces, NewPlayer2Pieces),
    replace_index(RowToRotate, RotatedRow, Board, RotatedBoard). % - Replace the row in the board. 

% --> Helper to rotate a list of tiles.
rotate_tile_list([], []).
rotate_tile_list([Tile | Rest], [RotatedTile | RotatedRest]) :-
    rotate_tile(Tile, RotatedTile), % - Rotate the tile.
    rotate_tile_list(Rest, RotatedRest). % - Recurse for the rest of the tiles.
% ----------------------------------------------------------------------------------------------- %

% ------------------------------------- ROTATE A COLUMN ----------------------------------------- %
% --> Main predicate to rotate a column.
rotate_column(ColumnIndex, Board, RotatedBoard, Player1Pieces, Player2Pieces, NewPlayer1Pieces, NewPlayer2Pieces) :-
    rotate_piece_in_column(ColumnIndex, Player1Pieces, Player2Pieces, NewPlayer1Pieces, NewPlayer2Pieces),
    extract_column(ColumnIndex, Board, Column), % - Extract the column.
    rotate_tile_list(Column, RotatedColumn), % - Rotate the column.
    replace_column(ColumnIndex, RotatedColumn, Board, RotatedBoard). % - Replace the column in the board.

extract_column(_, [], []).
extract_column(ColumnIndex, [Row | RestRows], [Element | RestElements]) :-
    idx(ColumnIndex, Row, Element), % - Extract the tile.
    extract_column(ColumnIndex, RestRows, RestElements). % - Recurse for the rest of the rows.

% --> Helper to replace a column in the board.
replace_column(_, [], _, []).
replace_column(ColumnIndex, [NewElement | RestNewElements], [Row | RestRows], [NewRow | RestNewRows]) :-
    replace_index(ColumnIndex, NewElement, Row, NewRow), % - Replace the element in the row.
    replace_column(ColumnIndex, RestNewElements, RestRows, RestNewRows). % - Recurse for the rest of the columns.
% ----------------------------------------------------------------------------------------------- %

% -------------------------------------- ROTATE A TILE ------------------------------------------ %
% --> Main predicate to rotate a tile.
rotate_tile(tile(Sides, Rotation), tile(Sides, NewRotation)) :-
    NewRotation is (Rotation + 90) mod 360. % - Rotating 90 degrees, this may be changed later to handle both direction rotations.

% --> Rotate pieces in a tile.
rotate_piece_in_row(RowToRotate, Player1Pieces, Player2Pieces, NewPlayer1Pieces, NewPlayer2Pieces) :-
    rotate_pieces_r(RowToRotate, Player1Pieces, NewPlayer1Pieces),
    rotate_pieces_r(RowToRotate, Player2Pieces, NewPlayer2Pieces).

rotate_pieces_r(_, [], []). % Base case: No more pieces to process.
rotate_pieces_r(RowToRotate, [piece([[BX, BY], [TX, TY]], ID) | Rest], [piece([[BX, BY], [NewTX, NewTY]], ID) | NewRest]) :-
    BY = RowToRotate, % Check if the bottom row matches RowToRotate
    rotate_piece_intile_coords(TX, TY, NewTX, NewTY),
    rotate_pieces_r(RowToRotate, Rest, NewRest). % Recurse for the rest of the pieces.
rotate_pieces_r(RowToRotate, [piece(Position, ID) | Rest], [piece(Position, ID) | NewRest]) :-
    rotate_pieces_r(RowToRotate, Rest, NewRest). % Recurse without changes if BY != RowToRotate.


rotate_piece_in_column(ColumnIndex, Player1Pieces, Player2Pieces, NewPlayer1Pieces, NewPlayer2Pieces) :-
    rotate_pieces_c(ColumnIndex, Player1Pieces, NewPlayer1Pieces),
    rotate_pieces_c(ColumnIndex, Player2Pieces, NewPlayer2Pieces).

rotate_pieces_c(_, [], []). % Base case: No more pieces to process.
rotate_pieces_c(ColumnIndex, [piece([[BX, BY], [TX, TY]], ID) | Rest], [piece([[BX, BY], [NewTX, NewTY]], ID) | NewRest]) :-
    BX = ColumnIndex, % Check if the bottom column matches column to rotate
    rotate_piece_intile_coords(TX, TY, NewTX, NewTY),
    rotate_pieces_c(ColumnIndex, Rest, NewRest). % Recurse for the rest of the pieces.
rotate_pieces_c(ColumnIndex, [piece(Position, ID) | Rest], [piece(Position, ID) | NewRest]) :-
    rotate_pieces_c(ColumnIndex, Rest, NewRest). % Recurse without changes if BX != COLUMNINDEX.

% ----------------------------------------------------------------------------------------------- %

% ------------------------------------- GET ROTATED SIDES --------------------------------------- %
% --> Helper to rotate sides of a tile, it calculates de steps needed and then calls its little helper to truly rotate the sides.
rotated_sides(Sides, Rotation, RotatedSides) :-
    Steps is Rotation // 90, % - Calculate the number of 90º steps.
    rotate_list(Sides, Steps, RotatedSides).

% Rotate a list following the custom order: 
rotate_list([A, B, C, D], 1, [C, A, D, B]). % Single-step rotation
rotate_list(List, 0, List).
rotate_list(List, Steps, RotatedList) :-
    Steps > 1,                              % If more steps are needed
    rotate_list(List, 1, TempList),         % Perform one rotation
    NextSteps is Steps - 1,                 % Decrement the number of steps
    rotate_list(TempList, NextSteps, RotatedList). % Recur for the remaining steps

rotate_piece_intile_coords(1, 1, 1, 2). % Case 1: (1,1) -> (1,2)
rotate_piece_intile_coords(1, 2, 2, 2). % Case 2: (1,2) -> (2,2)
rotate_piece_intile_coords(2, 2, 2, 1). % Case 3: (2,2) -> (2,1)
rotate_piece_intile_coords(2, 1, 1, 1). % Case 4: (2,1) -> (1,1)
% ----------------------------------------------------------------------------------------------- %






% -------------------------------------- DISPLAY BOARD ------------------------------------------ %
% --> Main predicate to display the entire board.
display_board(Board, Players) :-
    bg_dark_brown(DBrown),
    bg_brown(Brown),
    text_dark_brown(TxtDBrown),
    text_brown(TxtBrown),
    idx(1, Board, Row), % - Get the first row.
    length(Board, RowCount), % - Determine the number of rows.
    length(Row, ColumnCount), % - Determine the number of columns.
    RowWidth is ColumnCount * 12, % - Width based on the number of tiles.
    write('     '), print_top_border(RowWidth, 0, DBrown, TxtDBrown), % - Top border.
    write('     '), print_single_space(DBrown), % - Print a single brown space.
    print_top_literation(ColumnCount, ColumnCount), % - Print the top numeration.
    write('     '), print_top_border(RowWidth, 4, Brown, TxtBrown), % - Top border.
    display_rows(Board, RowCount, RowCount, Players), % - Call to Helper function that displays all the rows.
    write('     '), print_bottom_border(RowWidth, 4). % - Bottom border.

% --> Print top letter-columns assignement.
print_top_literation(0, _) :- % - Base case, no more columns to print.
    bg_dark_brown(DBrown),
    reset_color(Reset),
    write(DBrown), write(' '), write(Reset), nl.
print_top_literation(ColumnCount, TotalColumns) :-
    bg_dark_brown(DBrown),
    reset_color(Reset),
    text_dark_brown(TxtBrown),
    bg_lightest_brown(LBrown),
    text_orange(Orange),

    StartCol is 97, % - ASCII code for 'A'.
    ReverseIndex is TotalColumns - ColumnCount, % - Calculate the reverse index.
    LetterCode is StartCol + ReverseIndex, % - Calculate the ASCII code for the letter.
    char_code(Letter, LetterCode),

    write(DBrown), write(TxtBrown), write(' '),
    write(Reset), write(LBrown), write('     '),
    write(Orange), write(Letter),
    write('    '), write(Reset),
    write(DBrown), write(TxtBrown), write(' '), 

    NewColumnCount is ColumnCount - 1, % - Decrement the column count.
    print_top_literation(NewColumnCount, TotalColumns). % - Recurse for the next column.
    

% --> Print the top border.
print_top_border(RowWidth, ExtraSpaces, BGColor, TxtColor) :-
    reset_color(Reset),
    bg_dark_brown(DBrown),
    unicode_top_left(TopLeft),
    unicode_horizontal(Horizontal),
    unicode_top_right(TopRight),
    unicode_t_up(Tup),
    write(BGColor), write(TxtColor), write(TopLeft),
    print_row_with_crosses(Horizontal, Tup, RowWidth),
    write(TopRight), write(Reset), write(DBrown),
    print_repeated(' ', ExtraSpaces),
    write(Reset), 
    nl.

% --> Print the bottom border.
print_bottom_border(RowWidth, ExtraSpaces) :-
    bg_brown(Brown),
    bg_dark_brown(DBrown),
    reset_color(Reset),
    text_brown(TxtBrown),
    unicode_bottom_left(BottomLeft),
    unicode_horizontal(Horizontal),
    unicode_bottom_right(BottomRight),
    unicode_t_down(Tdown),
    write(Brown), write(TxtBrown), write(BottomLeft),
    print_row_with_crosses(Horizontal, Tdown, RowWidth),
    write(BottomRight), write(Reset), write(DBrown),
    print_repeated(' ', ExtraSpaces), % - Print extra spaces.
    write(Reset),% Write the right corner
    nl. % Newline.

% --> Helper predicate to repeat characters.
print_repeated(_, 0).
print_repeated(Char, Count) :-
    Count > 0,
    write(Char),
    NewCount is Count - 1,
    print_repeated(Char, NewCount).

% --> Helper to display all rows.
display_rows([], _, _, _). % - Base case, no rows left.
display_rows([Row], RowCount, TotalRows, Players) :-
    display_row(Row, RowCount, TotalRows, Players).
display_rows([Row | Rest], RowCount, TotalRows, Players) :-
    display_row(Row, RowCount, TotalRows, Players), % - Helper to display a single row.
    length(Row, NumTiles),
    RowWidth is NumTiles * 12, % - Calculate row width.
    write('     '), print_row_separator(RowWidth),
    NewRowCount is RowCount - 1, % - Decrement the row count.
    display_rows(Rest, NewRowCount, TotalRows, Players). % - Recursive call for the next rows.

% --> Print a row separator with correct Unicode symbols.
print_row_separator(Width) :-
    bg_brown(Brown),
    bg_dark_brown(DBrown),
    reset_color(Reset),
    text_brown(TxtBrown),
    unicode_t_left(TLeft),
    unicode_horizontal(Horizontal),
    unicode_cross(Cross),
    unicode_t_right(TRight),
    write(Brown), write(TxtBrown), write(TLeft),
    print_row_with_crosses(Horizontal, Cross, Width),
    write(TRight), write(Reset),
    write(DBrown), write('    '), write(Reset),
    nl.

%--> Print a repeated horizontal line with crosses at intervals
print_row_with_crosses(_,_,0).
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
display_row(Row, RowCount, TotalRows, Players) :-
    upper_semi(Us),
    lower_semi(Ls),
    full(F),
    write('     '), spacer(Row, F, Us, RowCount),
    write('     '), display_row_top(Row, Players, RowCount, 1), % - displays an entire top part of a row.
    write('     '), spacer(Row, ' ', ' ', RowCount),
    write('     '), display_row_bottom(Row, Players, RowCount, 1), % - displays an entire bottom part of a row.
    write('     '), spacer(Row, F, Ls, RowCount).

spacer([], _, F2, RowNumber) :-
    \+ F2 = ' ',
    unicode_vertical(Vertical),
    bg_brown(Brown),
    bg_dark_brown(DBrown),
    text_brown(TxtBrown),
    bg_lightest_brown(LBrown),
    reset_color(Reset), 
    write(Reset), write(Brown), write(' '), write(TxtBrown), write(Vertical), write(Reset),
    write(LBrown), write('  '), write(Reset), write(DBrown), write('  '), write(Reset),
    nl.
spacer([], _, F2, RowNumber) :-
    F2 = ' ',
    unicode_vertical(Vertical),
    bg_brown(Brown),
    bg_dark_brown(DBrown),
    text_brown(TxtBrown),
    bg_lightest_brown(LBrown),
    reset_color(Reset),
    text_orange(Orange), 
    write(Reset), write(Brown), write(' '), write(TxtBrown), write(Vertical), write(Reset),
    write(LBrown), write(Orange), write(RowNumber), write(' '), write(Reset), write(DBrown), write('  '), write(Reset),
    nl. 
spacer([_ | Rest], F1, F2, RowNumber) :-
    bg_brown(Brown),
    bg_light_brown(LBrown),
    text_brown(TxtBrown),
    reset_color(Reset), 
    unicode_vertical(Vertical),
    write(Brown), write(TxtBrown), write(Vertical), write(' '), write(Reset), write(LBrown), write(TxtBrown), 
    write(F1), write(F2), write(Reset), write(LBrown), write('      '), write(TxtBrown), write(F2), write(F1), write(Reset),        % - Display the symbols.
    spacer(Rest, F1, F2, RowNumber).

% --> Helper to display the top part of tiles in a row.
display_row_top([], _, _, _) :-
    unicode_vertical(Vertical),
    bg_brown(Brown),
    bg_dark_brown(DBrown),
    bg_lightest_brown(LBrown),
    text_brown(TxtBrown),
    reset_color(Reset), 
    write(Reset), write(Brown), write(' '), write(TxtBrown), write(Vertical), write(Reset),
    write(LBrown), write('  '), write(Reset), write(DBrown), write('  '), write(Reset),
    nl. % - Base case: No more tiles, end the row.
display_row_top([tile(Sides, Rotation) | Rest], Players, RowNumber, ColumnNumber) :-
    Players = [player(_,_,_, Player1Pieces), player(_,_,_, Player2Pieces)],
    rotated_sides(Sides, Rotation, [TopLeft, TopRight, _, _]), % - Extract top sides.
    bg_brown(Brown),
    bg_light_brown(LBrown),
    text_brown(TxtBrown),
    reset_color(Reset),
    color_symbol(TopLeft, LeftSymbol),
    color_symbol(TopRight, RightSymbol),
    unicode_vertical(Vertical),  
    write(Brown), write(TxtBrown), write(Vertical), write(' '), write(Reset), write(LBrown), write('  '), 
    check_and_print_piece(Player1Pieces, Player2Pieces, [[ColumnNumber, RowNumber],[1, 2]], LeftSymbol), write(Reset), write(LBrown), write('  '), check_and_print_piece(Player1Pieces, Player2Pieces, [[ColumnNumber, RowNumber],[2, 2]], RightSymbol), write(Reset), write(LBrown), write('  '),% - Display the symbols.
    ColumnNumber1 is ColumnNumber + 1,
    display_row_top(Rest, Players, RowNumber, ColumnNumber1). % - Recurse for the rest of the tiles

% --> Helper to display the bottom part of tiles in a row.
display_row_bottom([], _, _, _) :-
    unicode_vertical(Vertical),
    bg_brown(Brown),
    bg_dark_brown(DBrown),
    bg_lightest_brown(LBrown),
    text_brown(TxtBrown),
    reset_color(Reset), 
    write(Reset), write(Brown), write(' '), write(TxtBrown), write(Vertical), write(Reset),
    write(LBrown), write('  '), write(Reset), write(DBrown), write('  '), write(Reset),
    nl. % - Base case: No more tiles, end the row.
display_row_bottom([tile(Sides, Rotation) | Rest], Players, RowNumber, ColumnNumber) :- % - Process each tile.
    Players = [player(_,_,_, Player1Pieces), player(_,_,_, Player2Pieces)],
    rotated_sides(Sides, Rotation, [_, _, BottomLeft, BottomRight]), % - Extract bottom sides.
    bg_brown(Brown),
    bg_light_brown(LBrown),
    text_brown(TxtBrown),
    reset_color(Reset), 
    color_symbol(BottomLeft, LeftSymbol),
    color_symbol(BottomRight, RightSymbol),
    unicode_vertical(Vertical),
    write(Brown), write(TxtBrown), write(Vertical), write(' '), write(Reset), write(LBrown), write('  '),
    check_and_print_piece(Player1Pieces, Player2Pieces, [[ColumnNumber, RowNumber],[1, 1]], LeftSymbol), write(Reset), write(LBrown), write('  '), check_and_print_piece(Player1Pieces, Player2Pieces, [[ColumnNumber, RowNumber],[2, 1]], RightSymbol), write(Reset), write(LBrown), write('  '), % - Display the symbols.
    ColumnNumber1 is ColumnNumber + 1,
    display_row_bottom(Rest, Players, RowNumber, ColumnNumber1). % - Recurse for the rest of the tiles.

check_and_print_piece(Player1Pieces, Player2Pieces, Position, Symbol) :-
    text_white(TxtWhite),
    bg_orange(Orange),
    member(piece(Position, PieceId), Player1Pieces), % - Check if the piece is in the player's pieces.
    print_piece(Orange, TxtWhite, PieceId). % - Print the piece.

check_and_print_piece(Player1Pieces, Player2Pieces, Position, Symbol) :-
    text_white(TxtWhite),
    bg_blue(Blue),
    member(piece(Position, PieceId), Player2Pieces), % - Check if the piece is in the player's pieces.
    print_piece(Blue, TxtWhite, PieceId). % - Print the piece

check_and_print_piece(_, _, _, Symbol) :-
    % If no piece exists, print the default symbol
    reset_color(Reset),
    bg_light_brown(LBrown),
    write(Reset), write(LBrown), write(Symbol).
    


% --> Print single brown space.
print_single_space(BGColor) :-
    reset_color(Reset),
    write(BGColor), write(' '), write(Reset).

% --> Print player piece
print_piece(BGColor, TxtColor, PieceId) :-
    reset_color(Reset),
    write(Reset), write(BGColor), write(TxtColor), write(PieceId), write('.'), write(Reset).
% ----------------------------------------------------------------------------------------------- %




% ---------------------------------------- DISPLAY BENCH ----------------------------------------- %
display_bench(Board, player(_, _, Color, Pieces)) :-
    Color = orange,
    idx(1, Board, Row),
    length(Row, ColumnCount),
    CharCount is ColumnCount * 12 - 2, 
    bg_orange(Orange),
    bg_dark_brown(DBrown),
    bg_light_brown(LBrown),
    text_white(TxtWhite),
    reset_color(Reset),
    length(Pieces, PieceCount),
    write('     '), write(DBrown), write('  '), write(Reset), write(LBrown), print_repeated(' ', CharCount), write(Reset), write(DBrown), write('  '), write(Reset), nl,
    write('     '), write(DBrown), write('  '), write(Reset), print_bench_pieces_row(Pieces, CharCount, Orange, TxtWhite), write(Reset), write(DBrown), write('  '), write(Reset), nl,
    write('     '), write(DBrown), write('  '), write(Reset), write(LBrown), print_repeated(' ', CharCount), write(Reset), write(DBrown), write('  '), write(Reset), nl,
    write('     '), write(DBrown), write('  '), print_repeated(' ', CharCount), write('  '), write(Reset), nl.

display_bench(Board, player(_, _, Color, Pieces)) :-
    Color = blue,
    idx(1, Board, Row),
    length(Row, ColumnCount),
    CharCount is ColumnCount * 12 - 2, 
    bg_blue(Blue),
    bg_dark_brown(DBrown),
    bg_light_brown(LBrown),
    text_white(TxtWhite),
    reset_color(Reset),
    length(Pieces, PieceCount),
    write('     '), write(DBrown), write('  '), write(Reset), write(LBrown), print_repeated(' ', CharCount), write(Reset), write(DBrown), write('  '), write(Reset), nl,
    write('     '), write(DBrown), write('  '), write(Reset), print_bench_pieces_row(Pieces, CharCount, Blue, TxtWhite), write(Reset), write(DBrown), write('  '), write(Reset), nl,
    write('     '), write(DBrown), write('  '), write(Reset), write(LBrown), print_repeated(' ', CharCount), write(Reset), write(DBrown), write('  '), write(Reset), nl,
    write('     '), write(DBrown), write('  '), print_repeated(' ', CharCount), write('  '), write(Reset), nl.

print_bench_pieces_row([], 0, _, _).
print_bench_pieces_row([], CharCount, _, _) :-
    CharCount > 0,
    bg_light_brown(LBrown),
    reset_color(Reset), 
    write(LBrown), write(' '), write(Reset),
    NewCharCount is CharCount - 1,
    print_bench_pieces_row([], NewCharCount, _, _).
print_bench_pieces_row([piece(Position, PieceId) | Rest], CharCount, BGColor, TxtColor) :-
    Position = [[0,0],[0,0]],
    bg_light_brown(LBrown),
    reset_color(Reset),
    write(LBrown), write(' '), write(Reset), print_piece(BGColor, TxtColor, PieceId), write(LBrown), write(' '), write(Reset),
    NewCharCount is CharCount - 4,
    print_bench_pieces_row(Rest, NewCharCount, BGColor, TxtColor).
print_bench_pieces_row([piece(Position, PieceId) | Rest], CharCount, BGColor, TxtColor) :-
    Position \= [[0,0],[0,0]],
    print_bench_pieces_row(Rest, CharCount, BGColor, TxtColor).

    