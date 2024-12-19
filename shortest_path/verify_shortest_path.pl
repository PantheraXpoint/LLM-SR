% verify_shortest_path.pl

:- use_module(library(lists)).
:- dynamic edge/3.

% Entry point for interactive usage
main :-
    clear_edges,
    read_graph_interactive(Start, End, CandidatePath),
    ( catch(all_shortest_paths(Start, End, Paths, MinCost), _, fail) ->
        ( member(CandidatePath, Paths) -> 
            format('The candidate path is a shortest path. Its weight is: ~w~n', [MinCost])
        ; 
            writeln('The candidate path is NOT a shortest path.')
        ),
        write_shortest_path_info(Paths, MinCost)
    ;
        writeln('No paths found.')
    ),
    halt.

% Find all shortest paths
all_shortest_paths(Start, End, Paths, MinCost) :-
    setof([P, C], path(Start, End, P, C), AllPaths),
    find_min_cost(AllPaths, MinCost),
    findall(P, member([P, MinCost], AllPaths), Paths).

find_min_cost(Paths, MinCost) :-
    maplist(second_element, Paths, Costs),
    min_list(Costs, MinCost).

second_element([_, C], C).

% Pathfinding logic
path(Start, End, [Start], 0) :-
    Start == End.

path(Start, End, Path, Cost) :-
    travel(Start, End, [Start], PathRev, 0, Cost),
    reverse(PathRev, Path).

travel(Node, End, Visited, [End|Visited], AccCost, Cost) :-
    edge(Node, End, EdgeCost),
    \+ member(End, Visited),
    Cost is AccCost + EdgeCost.

travel(Node, End, Visited, Path, AccCost, Cost) :-
    edge(Node, NextNode, EdgeCost),
    \+ member(NextNode, Visited),
    NewAccCost is AccCost + EdgeCost,
    travel(NextNode, End, [NextNode|Visited], Path, NewAccCost, Cost).

% Interactive graph input
read_graph_interactive(StartNode, EndNode, CandidatePath) :-
    writeln('Enter the number of edges:'),
    read(NumEdges),
    writeln('Enter edges in the form nodeA-nodeB-weight. Type "done." when finished:'),
    read_edges_interactive(NumEdges),
    writeln('Enter the start node:'),
    read(StartNode),
    writeln('Enter the end node:'),
    read(EndNode),
    writeln('Enter the candidate path as a list (e.g., [1,2,3]):'),
    read(CandidatePath).

read_edges_interactive(0) :- !.
read_edges_interactive(N) :-
    N > 0,
    read(EdgeInput),
    ( EdgeInput = done ->
        true
    ; EdgeInput = NodeA-NodeB-Weight,
      assertz(edge(NodeA, NodeB, Weight)),
      assertz(edge(NodeB, NodeA, Weight)),
      N1 is N - 1,
      read_edges_interactive(N1)
    ).

% Clear edges
clear_edges :-
    retractall(edge(_, _, _)).

% Write shortest path information
write_shortest_path_info(Paths, MinCost) :-
    format('The weight of the shortest paths is: ~w~n', [MinCost]),
    writeln('The shortest paths are:'),
    forall(member(Path, Paths), writeln(Path)).

% Run main at startup
:- main.