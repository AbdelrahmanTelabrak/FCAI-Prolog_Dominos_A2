:- assertz(width(0)).
:- assertz(length(0)).
:-assertz(initial(1)).
:-assertz(goal(0)).

get_goal(Goal) :-
    goal(Goal).

set_goal(Value) :-
    retract(goal(_)),
    assert(goal(Value)).

get_initial([[Initial|_]|_], Initial).

set_status():-
    retract(initial(_)),
    assert(initial(0)).


setwidth(Value) :-
    retract(width(_)),
    assert(width(Value)),
    retract(initial(_)),
    assert(initial(1)).

get_width(Result) :-
    width(Result).

setlength(Value) :-
    retract(length(_)),
    assert(length(Value)).

get_length(Result) :-
    length(Result).

get_status(Bool):-
    initial(Bool).

get_MAXD(Z, State):-
    get_width(Width),
    get_length(Length),
    nth0(BombIndex1, State, 'b'),
    %write("line 1"),nl,
    replace_at_index(State, BombIndex1, '#', Temp),
    nth0(BombIndex2, Temp, 'b'),!,
    M is Width * Length,
    X is M - 2,

    Diff is BombIndex2 - BombIndex1,
    %write("Diff is "),write(Diff),nl,
    ((Diff =:= Width+1 ; Diff =:= Width-1) ->
    X1 is X-1,
    Z is X1//2
    %write(X1),write(" then "), write(Z),nl
    ;
    Z is X//2
    %write(X),write(" else "), write(Z),nl

    ).



search(Open, Closed):-
    get_status(Bool),
    %write(Bool),nl,
    (Bool =:= 1 -> get_initial(Open, Initial),
     set_status(),
     get_MAXD(Goal,Initial),
     set_goal(Goal),
     getBestState(Open, [CurrentState,Parent,G,H,F], _), % Step 1

     %write(Goal),write(' '),
    %CurrentState = Goal, % Step 2
     count_d(CurrentState,D),
     X is D//2,
     X=Goal,
     %write(X),write(' '), write(Goal),write(' '),
     write("Search is complete!"), nl,
     printSolution([CurrentState,Parent,G,H,F], Closed), !;
    %else
    get_goal(Goal),
    getBestState(Open, [CurrentState,Parent,G,H,F], _), % Step 1

     %write(Goal),write(' '),
    %CurrentState = Goal, % Step 2
     count_d(CurrentState,D),
     X is D//2,
     X=Goal,
     %write(X),write(' '), write(Goal),write(' '),
     write("Search is complete!"), nl,
     write("Goal is "),write(Goal),nl,
     printSolution([CurrentState,Parent,G,H,F], Closed), !).
     %out of IF

search(Open, Closed):-
getBestState(Open, CurrentNode, TmpOpen),
get_goal(Goal),
getAllValidChildren(CurrentNode,TmpOpen,Closed,Goal,Children), % Step3
addChildren(Children, TmpOpen, NewOpen), % Step 4
append(Closed, [CurrentNode], NewClosed), % Step 5.1
search(NewOpen, NewClosed). % Step 5.2


count_d(List, Count) :-
    include(=(d), List, DList),
    length(DList, Count).


% Implementation of step 3 to get the next states
getAllValidChildren(Node, Open, Closed, Goal, Children):-
findall(Next, getNextState(Node,Open,Closed,Goal,Next),
Children).
getNextState([State,_,G,_,_],Open,Closed,Goal,[Next,State,NewG,NewH,NewF]):-
move(State, Next, MoveCost),
isOkay(Next),
calculateH(Next, Goal, NewH),
NewG is G + MoveCost,
NewF is NewG + NewH,
not(member([Next,_,_,_,_], Open)),
not(member([Next,_,_,_,_], Closed)).
% Implementation of addChildren and getBestState
addChildren(Children, Open, NewOpen):-
append(Open, Children, NewOpen).
getBestState(Open, BestChild, Rest):-
findMin(Open, BestChild),
delete(Open, BestChild, Rest).


findMin([X], X):- !.
findMin([Head|T], Min):-
findMin(T, TmpMin),
Head = [_,_,_,HeadH,HeadF],
TmpMin = [_,_,_,TmpH,TmpF],
(TmpF < HeadF -> Min = TmpMin ; Min = Head).


printSolution([State, null, G, H, F],_):-
get_width(Width),get_length(Length),
write([State, G, H, F]), nl,nl,
print_2d_list(State, Width, Length),nl.

printSolution([State, Parent, G, H, F], Closed):-
    get_width(Width),get_length(Length),
    member([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    printSolution([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    write([State, G, H, F]), nl,nl,
    print_2d_list(State, Width, Length),nl.

move(State, Next,1):-
    left(State, Next); right(State, Next);
    up(State, Next); down(State, Next).

left(State, Next):-
    get_width(Width),
    nth0(EmptyTileIndex, State, .),
    NewIndex is EmptyTileIndex - 1,
    NewIndex > -1,
    %not(mod(NewIndex+1, Width)==0),
    not(0 is mod(NewIndex+1, Width)),
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
    %not(mod(NewIndex, Width)==0),
    not(0 is mod(NewIndex, Width)),
    %write(not(mod(NewIndex, Width)==0)),
    nth0(NewIndex, State, Element),
    not(Element=='b'),
    %not(NewIndex<Width*(Length-1)),
    replace_at_index(State, EmptyTileIndex, 'd', Temp),
    replace_at_index(Temp, NewIndex, 'd', Next).


up(State, Next):-
    %get_length(Length),
    get_width(Width),
    nth0(EmptyTileIndex, State, .),
    %write('Empty Index '),write(EmptyTileIndex),write(' '),
    EmptyTileIndex > Width-1,
    NewIndex is EmptyTileIndex - Width,
    %not(NewIndex<Width),
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
    %not(NewIndex<Width(Length-1)),
    replace_at_index(State, EmptyTileIndex, 'd', Temp),
    replace_at_index(Temp, NewIndex, 'd', Next).

isOkay(_):- true.

replace_at_index([], _, _, []).
replace_at_index([_|T], 0, NewValue, [NewValue|T]).
replace_at_index([H|T], Index, NewValue, [H|Result]) :-
  Index > 0,
  NextIndex is Index - 1,
  replace_at_index(T, NextIndex, NewValue, Result).


%calculateH([], _, 0):- !.
calculateH(State, Goal, Hvalue):-
    count_d(State, D),
    H is D//2,
    Hvalue is Goal-H.




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
