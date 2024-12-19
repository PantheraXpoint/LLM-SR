import json
import os
import re

def parse_graph_data(question):
    edges = re.findall(r'\((\d+),(\d+)\)', question)
    nodes = set()
    for edge in edges:
        nodes.update(edge)
    num_nodes = max(map(int, nodes)) + 1
    
    source_target = re.search(r'Is there a path between node (\d+) and node (\d+)', question)
    source = source_target.group(1)
    target = source_target.group(2)
    
    return num_nodes, source, target, edges

def create_graph_file(question, difficulty, index):
    base_dir = "examples"
    os.makedirs(f"{base_dir}/{difficulty}", exist_ok=True)
    
    num_nodes, source, target, edges = parse_graph_data(question)
    
    filename = f"{base_dir}/{difficulty}/graph_{index}.txt"
    with open(filename, 'w') as f:
        f.write(f"{num_nodes} {source} {target}\n")
        for edge in edges:
            f.write(f"{edge[0]} {edge[1]}\n")

def process_json(file_path):
    os.makedirs("examples", exist_ok=True)
    for diff in ["easy", "medium", "hard"]:
        os.makedirs(f"examples/{diff}", exist_ok=True)
    
    with open(file_path, 'r') as f:
        data = json.load(f)
        for index in data:
            create_graph_file(
                data[index]["question"],
                data[index]["difficulty"],
                index
            )

# Example usage:
process_json("inputs/ex.json")