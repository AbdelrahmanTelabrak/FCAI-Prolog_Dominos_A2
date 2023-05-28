set_width(Value) :-
    retractall(width(_)),
    asserta(width(Value)).

get_width(Result) :-
    width(Result).

set_length(Value) :-
    retractall(length(_)),
    asserta(length(Value)).

get_length(Result) :-
    length(Result).

search(Open, Closed, Goal):-
    getState(Open, [CurrentState,Parent], _), % Step 1
    CurrentState = Goal, !, % Step 2
    write("Search is complete!"), nl,
    printSolution([CurrentState,Parent], Closed).

search(Open, Closed, Goal):-
    getState(Open, CurrentNode, TmpOpen),
    getAllValidChildren(CurrentNode,TmpOpen,Closed,Children), % Step3
    addChildren(Children, TmpOpen, NewOpen), % Step 4
    append(Closed, [CurrentNode], NewClosed), % Step 5.1
    search(NewOpen, NewClosed, Goal). % Step 5.2

% Implementation of step 3 to get the next states
getAllValidChildren(Node, Open, Closed, Children):-
    findall(Next, getNextState(Node, Open, Closed, Next), Children).



getNextState([State,_], Open, Closed, [Next,State]):-
    move(State, Next),
    not(member([Next,_], Open)),
    not(member([Next,_], Closed)),
    isOkay(Next).
% Implementation of getState and addChildren determine the search alg.
% BFS
getState([CurrentNode|Rest], CurrentNode, Rest).

addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).

    % Implementation of printSolution to print the actual solution path
printSolution([State, null],_):-
    %write(State), nl.
    get_width(Width),get_length(Length),
    print_2d_list(State, Width, Length), nl.

printSolution([State, Parent], Closed):-
    member([Parent, GrandParent], Closed),
    printSolution([Parent, GrandParent], Closed),
    get_width(Width),get_length(Length),
    print_2d_list(State, Width, Length), nl.


move(State, Next):-
    horizontal(State, Next); vertical(State, Next).

horizontal(State, Next):-
    left(State, Next); right(State, Next).

left(State, Next):-
    %get_length(Length),
    get_width(Width),
    nth0(EmptyTileIndex, State, .),
    %write(EmptyTileIndex),
    %write('Empty Index '),write(EmptyTileIndex),write(' '),
    %TempTile is EmptyTileIndex,
    %not(0 is TempTile mod Width),
    %write('Empty Index '),write(EmptyTileIndex),write(' '),
    NewIndex is EmptyTileIndex - 1,
    NewIndex > -1,
    not(0 is mod(NewIndex+1, Width)),
    %write('New Index '),write(NewIndex),write(' '),
    nth0(NewIndex, State, Element),
    not(Element=='b'),
    replace_at_index(State, EmptyTileIndex, 'd', Temp),
    replace_at_index(Temp, NewIndex, 'd', Next).


right(State, Next):-
    %get_length(Length),
    get_width(Width),
    nth0(EmptyTileIndex, State, .),
    %TempTile is EmptyTileIndex,
    %not(Width-1 is TempTile mod Width),
    NewIndex is EmptyTileIndex + 1,
    not(0 is mod(NewIndex, Width)),
    nth0(NewIndex, State, Element),
    not(Element=='b'),
    %not(NewIndex<Width*(Length-1)),
    replace_at_index(State, EmptyTileIndex, 'd', Temp),
    replace_at_index(Temp, NewIndex, 'd', Next).

vertical(State, Next):-
    up(State, Next); down(State, Next).


up(State, Next):-
    %get_length(Length),
    get_width(Width),
    nth0(EmptyTileIndex, State, .),
    %write('Empty Index '),write(EmptyTileIndex),write(' '),
    EmptyTileIndex > Width-1,
    NewIndex is EmptyTileIndex - Width,
    not(NewIndex<Width),
    nth0(NewIndex, State, Element),
    not(Element=='b'),
    %not(NewIndex<Width*1),
    replace_at_index(State, EmptyTileIndex, 'd', Temp),
    replace_at_index(Temp, NewIndex, 'd', Next).



down(State, Next):-
    get_length(Length),
    get_width(Width),
    nth0(EmptyTileIndex, State, .),
    EmptyTileIndex < Width*(Length-1),
    NewIndex is EmptyTileIndex + Width,
    nth0(NewIndex, State, Element),
    not(Element=='b'),
    %not(NewIndex<Width*(Length-1)),
    replace_at_index(State, EmptyTileIndex, 'd', Temp),
    replace_at_index(Temp, NewIndex, 'd', Next).


isOkay(_):- true.

replace_at_index([], _, _, []).
replace_at_index([_|T], 0, NewValue, [NewValue|T]).
replace_at_index([H|T], Index, NewValue, [H|Result]) :-
  Index > 0,
  NextIndex is Index - 1,
  replace_at_index(T, NextIndex, NewValue, Result).

print_2d_list(List, Width, Height) :-
    length(List, ListLength),
    TotalCells is Width * Height,
    (ListLength =< TotalCells ->
        % If the list fits in the 2D grid
        print_2d_list_helper(List, Width, Height)
    ;
        % If the list is too big for the 2D grid, print an error message
        format("Error: List is too big to fit in a (~d x ~d) grid.~n", [Width, Height])
    ).

% Helper predicate to print the 2D list
print_2d_list_helper([], _, _).
print_2d_list_helper(List, Width, Height) :-
    % Print the first row
    take(Width, List, Row),
    print_row(Row),
    nl,
    % Recurse on the remaining rows
    drop(Width, List, Rest),
    print_2d_list_helper(Rest, Width, Height - 1).

% Helper predicate to print a row of the 2D list
print_row([]).
print_row([X|Xs]) :-
    format("~w ", [X]),
    print_row(Xs).

% Helper predicate to take the first N elements of a list
take(0, _, []).
take(N, [X|Xs], [X|Ys]) :-
    N > 0,
    N1 is N - 1,
    take(N1, Xs, Ys).

% Helper predicate to drop the first N elements of a list
drop(0, Xs, Xs).
drop(N, [_|Xs], Ys) :-
    N > 0,
    N1 is N - 1,
    drop(N1, Xs, Ys).


