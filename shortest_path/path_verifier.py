import json
import subprocess
from pathlib import Path
import logging
import os

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[logging.StreamHandler(), logging.FileHandler('verification.log')]
)

def get_graph_path(graph_num):
    """Return the file path based on graph number."""
    num = int(graph_num)
    if num < 180:  # Easy difficulty
        difficulty = "easy"
    else:  # Hard difficulty
        difficulty = "hard"
    return f"graphs_gt/{difficulty}/graph_{graph_num}.txt"

def create_prolog_code():
    """Create the Prolog code that verifies if a given path is a shortest path."""
    return r"""
:- use_module(library(lists)).
:- dynamic edge/3.

process_graph(GraphFile) :-
    clear_edges,
    read_graph(GraphFile, Start, End, CandidatePath),
    ( catch(all_shortest_paths(Start, End, Paths, _Cost), _, fail) ->
        ( member(CandidatePath, Paths) -> 
            writeln("true")
        ; 
            writeln("false")
        )
    ;
        writeln("false")
    ).

all_shortest_paths(Start, End, Paths, MinCost) :-
    setof([P, C], path(Start, End, P, C), AllPaths),
    find_min_cost(AllPaths, MinCost),
    findall(P, member([P, MinCost], AllPaths), Paths).

find_min_cost(Paths, MinCost) :-
    maplist(second_element, Paths, Costs),
    min_list(Costs, MinCost).

second_element([_, C], C).

path(Start, End, [Start], 0) :- Start == End.
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

read_graph(FileName, StartNode, EndNode, CandidatePath) :-
    open(FileName, read, Stream),
    read_line(Stream, Line1),
    split_line(Line1, [N, M]),
    read_edges(Stream, M),
    read_line(Stream, QueryLine),
    split_line(QueryLine, [StartNode, EndNode]),
    read_line(Stream, CandidateLine),
    split_line(CandidateLine, CandidatePath),
    close(Stream).

read_line(Stream, Line) :-
    read_line_to_codes(Stream, LineCodes),
    ( LineCodes \= end_of_file ->
        atom_codes(LineAtom, LineCodes),
        Line = LineAtom
    ; 
        Line = ""
    ).

split_line(Line, Values) :-
    split_string(Line, " ", "\s\t\n", StrValues),
    exclude(string_blank, StrValues, NonEmptyStrValues),
    maplist(to_number, NonEmptyStrValues, Values).

to_number(S, N) :-
    ( number_string(N, S) -> true ; N = S ).

string_blank("").

read_edges(_, 0) :- !.
read_edges(Stream, N) :-
    N > 0,
    read_line(Stream, Line),
    split_line(Line, [A, B, W]),
    assertz(edge(A, B, W)),
    assertz(edge(B, A, W)),
    N1 is N - 1,
    read_edges(Stream, N1).

clear_edges :-
    retractall(edge(_, _, _)).

:- current_prolog_flag(argv, [GraphFile]),
   process_graph(GraphFile),
   halt.

:- halt.
"""

def verify_shortest_path_with_prolog(temp_graph_file):
    # Write prolog code to a file
    with open('verify_shortest_path.pl', 'w') as f:
        f.write(create_prolog_code())

    try:
        result = subprocess.run(
            ['swipl', '-q', '-f', 'verify_shortest_path.pl', '--', temp_graph_file],
            capture_output=True,
            text=True
        )
        output = result.stdout.strip()
        if output in ("true", "false"):
            return output
        return "false"
    except subprocess.TimeoutExpired:
        return "false"
    except Exception as e:
        logging.error(f"Error processing {temp_graph_file}: {e}")
        return "false"
    finally:
        Path('verify_shortest_path.pl').unlink(missing_ok=True)

def create_temp_graph_file(original_graph_file, start, end, candidate_path):
    """
    Create a temporary graph file in the required format:
    N M
    M edges
    Start End
    candidate_path (space-separated)
    """
    with open(original_graph_file, 'r') as f:
        lines = [l.strip() for l in f if l.strip()]

    # We assume first line is "N M"
    first_line = lines[0].split()
    N = int(first_line[0])
    M = int(first_line[1])
    edges = lines[1:1+M]

    temp_path = Path("temp_graph.txt")
    with open(temp_path, 'w') as out:
        # Write N M
        out.write(f"{N} {M}\n")
        # Write edges
        for e in edges:
            out.write(e + "\n")
        # Write start end
        out.write(f"{start} {end}\n")
        # Write candidate path
        out.write(" ".join(map(str, candidate_path)) + "\n")

    return str(temp_path)

def verify_and_compare(ground_truth_file, easy_output_file, hard_output_file):
    print(f"Starting shortest path verification process...")
    print(f"Reading input file: {ground_truth_file}")

    # Create directories for output files if they don't exist
    for output_file in [easy_output_file, hard_output_file]:
        output_dir = os.path.dirname(output_file)
        if output_dir and not os.path.exists(output_dir):
            os.makedirs(output_dir)

    # Load ground truth
    try:
        with open(ground_truth_file, 'r') as f:
            ground_truth = json.load(f)
    except Exception as e:
        print(f"Error reading input file: {e}")
        return

    verification_results_easy = {}
    verification_results_hard = {}
    total = len(ground_truth)

    for idx, (graph_num, paths) in enumerate(ground_truth.items(), 1):
        print(f"Processing {idx}/{total}: graph_{graph_num}")
        graph_path = get_graph_path(graph_num)

        if not Path(graph_path).exists():
            print(f"Missing file: {graph_path}")
            # If the graph file doesn't exist, mark all candidate paths as "false"
            if isinstance(paths[0], list):
                # multiple paths
                result = ["false"] * len(paths)
            else:
                # single path
                result = ["false"]
        else:
            # Check if paths is empty
            if not paths:
                print(f"No paths available for graph_{graph_num}. Skipping...")
                result = ["false"]  # No valid path to verify
            else:
                # paths can be a single path or a list of paths
                # Normalize to a list of candidate paths:
                if len(paths) > 0 and isinstance(paths[0], list):
                    candidate_paths = paths  # already a list of lists
                else:
                    # single path
                    candidate_paths = [paths]

                result = []
                for cand_path in candidate_paths:
                    start = cand_path[0]
                    end = cand_path[-1]
                    temp_graph_file = create_temp_graph_file(graph_path, start, end, cand_path)
                    outcome = verify_shortest_path_with_prolog(temp_graph_file)
                    Path(temp_graph_file).unlink(missing_ok=True)
                    result.append(outcome)

        # Store results in the appropriate file
        if int(graph_num) < 180:
            verification_results_easy[graph_num] = result
        else:
            verification_results_hard[graph_num] = result

    # Write results to output files
    try:
        with open(easy_output_file, 'w') as f:
            json.dump(verification_results_easy, f, indent=4)
        print(f"Easy results written to {easy_output_file}")

        with open(hard_output_file, 'w') as f:
            json.dump(verification_results_hard, f, indent=4)
        print(f"Hard results written to {hard_output_file}")
    except Exception as e:
        print(f"Error writing final results: {e}")


if __name__ == "__main__":
    verify_and_compare(
        '/home/quang/CS579-TermProject/shortest_path/prolog_paths_gen/few_shot_shortest_path_gpt-4o.json',
        'results/few_shot_shortest_path_gpt-4o_easy.txt',
        'results/few_shot_shortest_path_gpt-4o_hard.txt'
    )
