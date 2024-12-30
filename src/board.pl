% =============================================================================================== %
% ======     THIS FILE CONTAINS THE PREDICATES RELATED TO TILES AND BOARD GENERATION       ====== %
% =============================================================================================== %

:- module(board, [display_board/1]).



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

% --> Helper(2) that rotates a list on a number of steps.
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
    format("+-----------------------+\n", []), % Top border.
    display_rows(Board), % Call to Helper function that displays all the rows.

% --> Helper to display all rows.
display_rows([]). % - Base case, no rows left.
display_rows([Row | Rest]) :-
    display_row(Row), % - Helper to display a single row.
    format("+-----------------------+\n", []), % Row separator.
    display_rows(Rest). % Recursive call for the next rows.

% --> Helper to display a single row of tiles.
display_row(Row) :-
    display_row_top(Row), % - displays an entire top part of a row.
    display_row_bottom(Row). % - displays an entire bottom part of a row.

% --> Helper to display the top part of tiles in a row.
display_row_top([]) :- format("|\n", []). % - Base case, no tiles left and end of the row.
display_row_top([tile(Sides, Rotation) | Rest]) :- % - Capture first tile on the list of tiles.
    rotated_sides(Sides, Rotation, [TopLeft, TopRight, _, _]), % - Extract top sides.
    format("| ~w   ~w ", [TopLeft, TopRight]),  % - Print top-left and top-right sides.
    display_row_top(Rest). % Recurse for the rest of the tiles

% --> Helper to display the bottom part of tiles in a row.
display_row_bottom([]) :- format("|\n", []). % - Base case: No more tiles, end the row.
display_row_bottom([tile(Sides, Rotation) | Rest]) :- % - Process each tile.
    rotated_sides(Sides, Rotation, [_, _, BottomLeft, BottomRight]), % - Extract bottom sides.
    format("| ~w   ~w ", [BottomLeft, BottomRight]), % - Print bottom-left and bottom-right sides.
    display_row_bottom(Rest). % - Recurse for the rest of the tiles.
% ----------------------------------------------------------------------------------------------- %
