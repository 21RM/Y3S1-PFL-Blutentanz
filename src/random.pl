% ========================================================================================== %
% =====       THIS FILE CONTAINS OUR RANDOM LIBRARY, CAREFULY CRAFTED WITH LOVE        ===== %
% ========================================================================================== %

% ---------------------------------------- Definitions ------------------------------------------ %
:- module(myRandom, [my_random_member/2]).
% ----------------------------------------------------------------------------------------------- %


% ---------------------------------------- INCLUDES --------------------------------------------- %
:- use_module('list.pl').
:- use_module(library(random)).
% ----------------------------------------------------------------------------------------------- %



% --> Randomly select an element from a list
my_random_member(Element, List) :-
    length(List, Length),             % - Get the length of the list.
    Length > 0, 
    random_between(1, Length, Index),     % - Generate a random index (1-based)
    idx(Index, List, Element).            % - Ensure the list is not empty.
    

% --> Randomly generate an integer between Low and High (inclusive)
random_between(Low, High, Value) :-
    Upper is High + 1,                  % Adjust the upper bound for inclusiveness
    random(Low, Upper, Value).          % Use `random/3` from the random library