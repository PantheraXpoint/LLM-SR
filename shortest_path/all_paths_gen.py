import json
import subprocess
from pathlib import Path

def create_prolog_code():
    """Create the Prolog code for finding all shortest paths"""
    return """
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(system)).

% Set stack limit
:- set_prolog_flag(stack_limit, 2_147_483_648).

% Declare edge/3 as dynamic
:- dynamic edge/3.

% Main predicate to find paths for a specific graph
process_graph(GraphFile) :-
    clear_edges,
    read_graph(GraphFile, Start, End),
    (catch(all_shortest_paths(Start, End, Paths, Cost), _, fail) ->
        write_paths_json(Paths)
    ;   
        write('[]')
    ).

% Find all shortest paths (same logic as the verifier)
all_shortest_paths(Start, End, Paths, MinCost) :-
    setof([P, C], path(Start, End, P, C), AllPaths),
    find_min_cost(AllPaths, MinCost),
    findall(P, member([P, MinCost], AllPaths), Paths).

find_min_cost(Paths, MinCost) :-
    maplist(second_element, Paths, Costs),
    min_list(Costs, MinCost).

second_element([_, C], C).

% Original path finding logic
path(Start, End, [Start], 0) :-
    Start == End.

path(Start, End, Path, Cost) :-
    travel(Start, End, [Start], PathRev, 0, Cost),
    reverse(PathRev, Path).

travel(Node, End, Visited, [End|Visited], AccCost, Cost) :-
    edge(Node, End, EdgeCost),
    \\+ member(End, Visited),
    Cost is AccCost + EdgeCost.

travel(Node, End, Visited, Path, AccCost, Cost) :-
    edge(Node, NextNode, EdgeCost),
    \\+ member(NextNode, Visited),
    NewAccCost is AccCost + EdgeCost,
    travel(NextNode, End, [NextNode|Visited], Path, NewAccCost, Cost).

% Graph reading predicates
read_graph(FileName, StartNode, EndNode) :-
    open(FileName, read, Stream),
    read_line(Stream, Line1),
    split_line(Line1, [_NumNodes, NumEdges]),
    read_edges(Stream, NumEdges),
    read_line(Stream, QueryLine),
    split_line(QueryLine, [StartNode, EndNode]),
    close(Stream).

read_line(Stream, Line) :-
    read_line_to_codes(Stream, LineCodes),
    ( LineCodes \\= end_of_file
    ->  atom_codes(LineString, LineCodes),
        Line = LineString
    ;   Line = ""
    ).

split_line(Line, Numbers) :-
    split_string(Line, " ", "\\s\\t\\n", StrNumbers),
    exclude(string_blank, StrNumbers, NonEmptyStrNumbers),
    maplist(number_string, Numbers, NonEmptyStrNumbers).

string_blank("").

read_edges(_, 0) :- !.
read_edges(Stream, N) :-
    N > 0,
    read_line(Stream, Line),
    split_line(Line, [NodeA, NodeB, Weight]),
    assertz(edge(NodeA, NodeB, Weight)),
    assertz(edge(NodeB, NodeA, Weight)),
    N1 is N - 1,
    read_edges(Stream, N1).

% Clear edges between graphs
clear_edges :-
    retractall(edge(_, _, _)).

% Write paths in JSON format
write_paths_json(Paths) :-
    (length(Paths, 1) ->
        % If there's only one path, write it directly
        [SinglePath] = Paths,
        write(SinglePath)
    ;   
        % If there are multiple paths, write them as an array
        write('['),
        write_paths_list(Paths),
        write(']')
    ).

write_paths_list([]).
write_paths_list([Path]) :-
    write_single_path(Path).
write_paths_list([Path|Rest]) :-
    write_single_path(Path),
    write(','),
    write_paths_list(Rest).

write_single_path(Path) :-
    write(Path).

% Entry point
:- current_prolog_flag(argv, [GraphFile]),
   process_graph(GraphFile),
   halt.

:- halt.
"""

def process_single_graph(graph_file):
    """Process a single graph file and return its shortest paths"""
    # Create temporary Prolog file
    prolog_code = create_prolog_code()
    with open('find_paths.pl', 'w') as f:
        f.write(prolog_code)

    try:
        # Run Prolog program
        result = subprocess.run(
            ['swipl', '-q', '-f', 'find_paths.pl', '--', graph_file],
            capture_output=True,
            text=True
        )
        
        # Parse the JSON output
        if result.stdout.strip():
            return json.loads(result.stdout)
        return []
    except Exception as e:
        print(f"Error processing {graph_file}: {e}")
        return []
    finally:
        # Clean up
        Path('find_paths.pl').unlink(missing_ok=True)

def process_all_graphs():
    """Process all graphs from 0 to 379 (example) in both easy and hard folders."""
    results = {}
    
    easy_dir = Path('/home/quang/CS579-TermProject/shortest_path/graphs_gpt4o/easy')
    hard_dir = Path('/home/quang/CS579-TermProject/shortest_path/graphs_gpt4o/hard')

    NUM_TEST_CASES = 380  # total number of test cases you expect
    for graph_num in range(NUM_TEST_CASES):
        if graph_num < 180:
            graph_file = easy_dir / f"graph_{graph_num}.txt"
        else:
            graph_file = hard_dir / f"graph_{graph_num}.txt"

        print(f"Processing graph {graph_num}")
        if graph_file.exists():
            result = process_single_graph(str(graph_file))
        else:
            # File is missing, record empty result or [] 
            result = []
        
        results[str(graph_num)] = result

    # Save results
    output_path = '/home/quang/CS579-TermProject/shortest_path/prolog_paths_gen/indirect_shortest_path_gpt-4o.json'
    with open(output_path, 'w') as f:
        json.dump(results, f, separators=(',', ':'))
    
    print(f"\nResults saved to {output_path}")

if __name__ == "__main__":
    process_all_graphs()