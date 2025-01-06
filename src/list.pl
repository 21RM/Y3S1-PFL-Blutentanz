% ========================================================================================== %
% =====         THIS FILE CONTAINS OUR LIST LIBRARY, CAREFULY CRAFTED WITH LOVE        ===== %
% ========================================================================================== %

% ---------------------------------------- Definitions ------------------------------------------ %
:- module(myList, [idx/3, replace_index/4, inbetween/3,generate_empty_lists/2,custom_include/3, numlist/3, letterlist/3]).
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

% between(+Low, +High, -Value)
inbetween(Low, High, Value) :-
    Low =< High,
    Value = Low.
inbetween(Low, High, Value) :-
    Low < High,
    NextLow is Low + 1,
    inbetween(NextLow, High, Value).

generate_empty_lists(0, []). % Base case: If length is 0, return an empty list.
generate_empty_lists(Len, [[] | Rest]) :-
    Len > 0,
    NewLen is Len - 1, % Decrement length
    generate_empty_lists(NewLen, Rest). % Recurse to build the rest of the list

% Base case: empty list results in an empty filtered list
custom_include(_, [], []).
% Recursive case: check if the predicate succeeds for the head of the list
custom_include(Predicate, [Head|Tail], [Head|FilteredTail]) :-
    call(Predicate, Head),
    include(Predicate, Tail, FilteredTail).
% Recursive case: skip the head if the predicate fails
custom_include(Predicate, [_|Tail], FilteredTail) :-
    include(Predicate, Tail, FilteredTail).


% --> Generate a List of Numbers from Min to Max.
numlist(Min, Max, List) :-
    Min > Max,
    List = [].
numlist(Min, Max, [Min | Rest]) :-
    NextMin is Min + 1,
    numlist(NextMin, Max, Rest).


% --> Generate a List of Letters from Min to Max.
letterlist(Min, Max, Letters) :-
    Min > Max,
    Letters = [].
letterlist(Min, Max, [Letter|Rest]) :-
    idx(Min, "abcdefghijklmnopqrstuvwxyz", Letter),
    NextMin is Min + 1,
    letterlist(Min, Max, Rest).

