import json
import subprocess
from pathlib import Path
import logging
from collections import defaultdict

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[logging.StreamHandler(), logging.FileHandler('verification.log')]
)

def get_graph_path(graph_num):
    num = int(graph_num)
    if num <= 351:
        difficulty = "easy"
    elif num <= 1551:
        difficulty = "medium"
    else:
        difficulty = "hard"
    return f"/home/quang/CS579-TermProject/graph-prolog/connectivity/graphs_gt/{difficulty}/graph_{graph_num}.txt"

def parse_graph_file(filepath):
    with open(filepath) as f:
        lines = f.readlines()
    
    first_line = lines[0].strip().split()
    num_nodes = int(first_line[0])
    pairs = []
    for i in range(1, len(first_line), 2):
        pairs.append((int(first_line[i]), int(first_line[i+1])))
    
    edges = []
    for line in lines[1:]:
        if line.strip():
            u, v = map(int, line.strip().split())
            edges.append((u, v))
            edges.append((v, u))  # Since the graph is undirected
            
    return num_nodes, pairs, edges

def verify_connectivity_with_prolog(pairs, edges):
    prolog_code_lines = [
        ":- dynamic edge/2.",
        "",
        "% Define edges"
    ]

    for u, v in edges:
        prolog_code_lines.append(f"edge({u}, {v}).")
    
    prolog_code_lines.extend([
        "",
        "% Path finding using breadth-first search to improve performance",
        "bfs(Start, End) :-",
        "    bfs([[Start]], End, []).",
        "",
        "bfs([[End|_]|_], End, _) :-",
        "    write('true,'), !.",
        "",
        "bfs([CurrentPath|Paths], End, Visited) :-",
        "    CurrentPath = [CurrentNode|_],",
        "    findall([NextNode|CurrentPath],",
        "        (",
        "            edge(CurrentNode, NextNode),",
        "            \\+ member(NextNode, Visited)",
        "        ),",
        "        NewPaths),",
        "    append(Paths, NewPaths, UpdatedPaths),",
        "    bfs(UpdatedPaths, End, [CurrentNode|Visited]).",
        "",
        "bfs([], _, _) :-",
        "    write('false,'), !.",
        "",
        "% Main connectivity predicate",
        "connected(Start, End) :-",
        "    Start = End -> write('true,'); bfs(Start, End).",
        "",
        "% Check connectivity for given pairs",
        "check_connectivity([]).",
        "check_connectivity([[X, Y] | Rest]) :-",
        "    connected(X, Y),",
        "    check_connectivity(Rest).",
        "",
        ":- initialization(main).",
        "",
        "main :-",
        "    check_connectivity(["
    ])

    for start, end in pairs:
        prolog_code_lines.append(f"[{start}, {end}],")
    
    prolog_code_lines[-1] = prolog_code_lines[-1].rstrip(',') + "])."

    prolog_code = '\n'.join(prolog_code_lines)

    with open('temp_verify.pl', 'w') as f:
        f.write(prolog_code)

    try:
        result = subprocess.run(
            ['swipl', '-q', '-t', 'halt', '-s', 'temp_verify.pl'],
            capture_output=True,
            text=True,
            timeout=10  # Increased timeout to 10 seconds to allow Prolog more time to process
        )
        results = result.stdout.strip().split(',')[:-1]
        logging.info(f"Successfully processed graph connectivity")
        return results
    except subprocess.TimeoutExpired:
        logging.error(f"Timeout occurred while processing connectivity")
        return []
    except Exception as e:
        logging.error(f"Error occurred: {e}")
        return []
    finally:
        Path('temp_verify.pl').unlink(missing_ok=True)

def verify_and_compare(ground_truth_file, output_file):
    with open(ground_truth_file, 'r') as f:
        ground_truth = json.load(f)
    
    results = {}
    total = len(ground_truth)
    
    for idx, (graph_num, expected) in enumerate(ground_truth.items(), 1):
        logging.info(f"Processing {idx}/{total}: graph_{graph_num}")
        graph_path = get_graph_path(graph_num)
        
        if not Path(graph_path).exists():
            logging.warning(f"Missing file: {graph_path}")
            results[graph_num] = ["false"] * len(expected)
            continue
        
        num_nodes, pairs, edges = parse_graph_file(graph_path)
        prolog_results = verify_connectivity_with_prolog(pairs, edges)
        if not prolog_results:
            results[graph_num] = ["false"] * len(expected)
            continue
            
        comparison = []
        for pr, exp in zip(prolog_results, expected):
            matches = (pr.lower() == "true") == (exp.lower() == "yes")
            comparison.append(str(matches).lower())
        results[graph_num] = comparison
        
        # Write results incrementally
        with open(output_file, 'w') as f:
            json.dump(results, f)

if __name__ == "__main__":
    input_file = 'gt_ans/ground_truths.json'
    output_file = 'results/gt_results.txt'
    verify_and_compare(input_file, output_file)
