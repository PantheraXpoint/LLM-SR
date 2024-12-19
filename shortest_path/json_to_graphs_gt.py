import json
import os
import re

def extract_edges_from_question(question):
    """Extract edges from the question text."""
    edges = []
    # Find all edge descriptions using regex
    edge_pattern = r"an edge between node (\d+) and node (\d+) with weight (\d+)"
    matches = re.findall(edge_pattern, question)
    
    for match in matches:
        node1, node2, weight = map(int, match)
        edges.append((node1, node2, weight))
    
    return edges

def extract_query_nodes(question):
    """Extract start and end nodes from the question."""
    query_pattern = r"Give the shortest path from node (\d+) to node (\d+)"
    match = re.search(query_pattern, question)
    if match:
        start_node, end_node = map(int, match.groups())
        return start_node, end_node
    return None, None

def create_graph_file(edges, start_node, end_node, num_nodes):
    """Create graph file content in the required format."""
    # First line: number of nodes and number of edges
    content = f"{num_nodes} {len(edges)}\n"
    
    # Add edges
    for edge in edges:
        content += f"{edge[0]} {edge[1]} {edge[2]}\n"
    
    # Add query nodes
    content += f"{start_node} {end_node}"
    
    return content

def process_json_to_graphs(json_file, output_dir):
    """Process JSON file and create graph files."""
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    # Read JSON file
    with open(json_file, 'r') as f:
        data = json.load(f)
    
    # Process each problem
    for problem_id, problem_data in data.items():
        question = problem_data['question']
        difficulty = problem_data['difficulty']
        
        # Extract edges and query nodes
        edges = extract_edges_from_question(question)
        start_node, end_node = extract_query_nodes(question)
        
        # Find the maximum node number to determine number of nodes
        max_node = max(max(edge[0] for edge in edges), 
                      max(edge[1] for edge in edges))
        num_nodes = max_node + 1
        
        # Create graph file content
        content = create_graph_file(edges, start_node, end_node, num_nodes)
        
        # Create difficulty subdirectory
        diff_dir = os.path.join(output_dir, difficulty)
        os.makedirs(diff_dir, exist_ok=True)
        
        # Write to file
        file_path = os.path.join(diff_dir, f'graph_{problem_id}.txt')
        with open(file_path, 'w') as f:
            f.write(content)

# Example usage
if __name__ == "__main__":
    json_file_path = "inputs/ex.json"  # Replace with your JSON file path
    output_directory = "examples"     # Replace with your desired output directory
    
    process_json_to_graphs(json_file_path, output_directory)