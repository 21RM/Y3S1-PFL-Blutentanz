% ========================================================================================== %
% =====         THIS FILE CONTAINS OUR LIST LIBRARY, CAREFULY CRAFTED WITH LOVE        ===== %
% ========================================================================================== %

% ---------------------------------------- Definitions ------------------------------------------ %
:- module(myList, [idx/3, replace_index/4, inbetween/3,generate_empty_lists/2,custom_include/3, numlist/3, letterlist/3, flatten/2, index_of_max/2]).
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
    idx(Min, "abcdefghijklmnopqrstuvwxyz", Code),
    char_code(Letter, Code),
    NextMin is Min + 1,
    letterlist(NextMin, Max, Rest).

flatten([], []). % Base case: An empty list flattens to an empty list.
flatten([Head|Tail], FlatList) :-
    flatten(Head, FlatHead),       % Recursively flatten the head.
    flatten(Tail, FlatTail),       % Recursively flatten the tail.
    append(FlatHead, FlatTail, FlatList). % Combine flattened head and tail.
flatten(Element, [Element]) :-
    \+ is_list(Element). 

is_list([]). % An empty list is a list.
is_list([_|Tail]) :- is_list(Tail).


index_of_max(List, Index) :-
    index_of_max(List, 0, -1, -999999, Index). % Initialize with index `-1` and value `-inf`.

% Base case: When the list is empty, return the index of the max value.
index_of_max([], _, MaxIndex, _, MaxIndex).

% Case 1: Current element is greater than the current max value.
index_of_max([Head|Tail], CurrentIndex, _, MaxValue, Index) :-
    Head > MaxValue,
    NewMaxIndex is CurrentIndex,
    NewMaxValue is Head,
    NextIndex is CurrentIndex + 1,
    index_of_max(Tail, NextIndex, NewMaxIndex, NewMaxValue, Index).

% Case 2: Current element is not greater than the current max value.
index_of_max([Head|Tail], CurrentIndex, MaxIndex, MaxValue, Index) :-
    Head =< MaxValue,
    NextIndex is CurrentIndex + 1,
    index_of_max(Tail, NextIndex, MaxIndex, MaxValue, Index).