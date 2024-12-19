:- dynamic edge/2.

% Path finding using breadth-first search
bfs(Start, End) :-
    bfs([[Start]], End, []).

bfs([[End|_]|_], End, _) :-
    write('true'), nl, !.

bfs([CurrentPath|Paths], End, Visited) :-
    CurrentPath = [CurrentNode|_],
    findall([NextNode|CurrentPath],
        (
            edge(CurrentNode, NextNode),
            \+ member(NextNode, Visited)
        ),
        NewPaths),
    append(Paths, NewPaths, UpdatedPaths),
    bfs(UpdatedPaths, End, [CurrentNode|Visited]).

bfs([], _, _) :-
    write('false'), nl, !.

% Main connectivity predicate
connected(Start, End) :-
    Start = End -> write('true'), nl; bfs(Start, End).

% Check connectivity for multiple pairs
check_connectivity([]).
check_connectivity([[X, Y] | Rest]) :-
    connected(X, Y),
    check_connectivity(Rest).

% Accepts user input and processes it
main :-
    write('Enter edges in the form [u-v], followed by "done":'), nl,
    read_edges,
    write('Enter pairs to check in the form [X, Y], followed by "done":'), nl,
    read_pairs(Pairs),
    write('Connectivity results:'), nl,
    check_connectivity(Pairs),
    halt.

% Reading edges dynamically and making them bidirectional
read_edges :-
    read(Line),
    ( Line = done -> true
    ; Line = U-V, assert(edge(U, V)), assert(edge(V, U)), read_edges
    ).

% Reading pairs to check
read_pairs(Pairs) :-
    read(Line),
    ( Line = done -> Pairs = []
    ; Line = [X, Y], read_pairs(Rest), Pairs = [[X, Y] | Rest]
    ).

:- initialization(main).
