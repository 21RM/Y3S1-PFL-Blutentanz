% ========================================================================================== %
% =====         THIS FILE CONTAINS OUR LIST LIBRARY, CAREFULY CRAFTED WITH LOVE        ===== %
% ========================================================================================== %

% ---------------------------------------- Definitions ------------------------------------------ %
:- module(myList, [idx/3, replace_index/4]).
% ----------------------------------------------------------------------------------------------- %



% Retrieves the Element at position Index (1-based) from the List.
idx(1, [Element | _], Element).  % Base case: If Index is 1, take the head of the list.
idx(Index, [_ | Tail], Element) :-
    Index > 1,                    % Ensure Index is greater than 1.
    NextIndex is Index - 1,       % Decrement the Index.
    idx(NextIndex, Tail, Element). % Recurse on the tail with the decremented Index.

replace_index(1, NewElement, [_|Tail], [NewElement|Tail]).
replace_index(Index, NewElement, [Head|Tail], [Head|NewTail]) :-
    Index > 1,
    NextIndex is Index - 1,
    replace_index(NextIndex, NewElement, Tail, NewTail). 
