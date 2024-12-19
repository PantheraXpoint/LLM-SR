import json
import os
import ast

def parse_graph_content(content):
    """Parse the Python dictionary string from the content"""
    try:
        # Clean up the code block formatting (remove the triple backticks and leading/trailing whitespace)
        cleaned_content = content.strip().strip("```python").strip("```").strip()
        
        # Remove the double braces ({{ ... }})
        cleaned_content = cleaned_content.strip("{}").strip()

        # Replace single quotes with double quotes to make it JSON-compatible
        cleaned_content = cleaned_content.replace("'", '"')

        # Ensure it has the correct dictionary format
        cleaned_content = "{" + cleaned_content + "}"

        # Safely evaluate the string into a dictionary
        graph_dict = ast.literal_eval(cleaned_content)

        return graph_dict
    except Exception as e:
        print(f"Error parsing content: {e}")
        return None

def create_graph_file(graph_dict, filename):
    """Create a graph file in the specified format"""
    if not graph_dict:
        return
    
    try:
        with open(filename, 'w') as f:
            # Write number of nodes and edges
            num_nodes = graph_dict.get('num_nodes', 0)
            edges = graph_dict.get('edges', [])
            
            f.write(f"{num_nodes} {len(edges)}\n")
            
            # Write edges in the format: node1 node2 weight
            for edge in edges:
                f.write(f"{edge[0]} {edge[1]} {edge[2]}\n")
            
            # Write source and target
            source = graph_dict.get('source', 0)
            target = graph_dict.get('target', 0)
            f.write(f"{source} {target}\n")
            
            print(f"Created {filename}")  # Debugging success message
            
    except Exception as e:
        print(f"Error writing file {filename}: {e}")

def process_json_files(json_files):
    """Process JSON files and create graph files"""
    # Create directories if they don't exist
    os.makedirs('graphs_gpt4o/easy', exist_ok=True)
    os.makedirs('graphs_gpt4o/hard', exist_ok=True)
    
    for json_file in json_files:
        try:
            with open(json_file, 'r') as f:
                data = json.load(f)
                
            for entry in data:
                custom_id = entry['custom_id']
                print(f"Processing custom_id: {custom_id}")  # Debugging
                
                # Determine folder based on custom_id
                if custom_id.isdigit() and int(custom_id) < 180:
                    folder = 'graphs_gpt4o/easy'
                else:
                    folder = 'graphs_gpt4o/hard'
                print(f"Assigned folder: {folder}")  # Debugging
                
                filename = os.path.join(folder, f'graph_{custom_id}.txt')
                content = entry['response']['body']['choices'][0]['message']['content']
                
                # Parse and create graph file
                graph_dict = parse_graph_content(content)
                if graph_dict:
                    create_graph_file(graph_dict, filename)
                    print(f"Created {filename}")
                else:
                    print(f"Failed to process custom_id {custom_id}")
                    
        except Exception as e:
            print(f"Error processing {json_file}: {e}")

def main():
    # Specify your JSON files here
    json_files = ['/home/quang/CS579-TermProject/shortest_path/inputs/indirect_shortest_path_gpt-4o.json']  # Replace with your actual filenames
    process_json_files(json_files)

if __name__ == "__main__":
    main()
