import json
import os
import ast

def parse_graph_content(content):
    """Parse the Python dictionary string from the content"""
    try:
        # Clean up the content (strip the code block formatting and extra whitespace)
        cleaned_content = content.strip().strip("```python").strip("```").strip()
        
        # Remove the double braces ({{ ... }}) if present
        cleaned_content = cleaned_content.strip("{}").strip()

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
            # Write number of nodes and source-target
            num_nodes = graph_dict.get('num_nodes', 0)
            source = graph_dict.get('source', 0)
            target = graph_dict.get('target', 0)
            
            f.write(f"{num_nodes} {source} {target}\n")
            
            # Write edges (without weights)
            edges = graph_dict.get('edges', [])
            for edge in edges:
                f.write(f"{edge[0]} {edge[1]}\n")
            
            print(f"Created {filename}")  # Debugging success message
            
    except Exception as e:
        print(f"Error writing file {filename}: {e}")

def determine_folder(custom_id):
    """Determine the folder based on custom_id"""
    custom_id = int(custom_id)
    if 0 <= custom_id <= 351:
        return 'graphs_gpt4o/easy'
    elif 352 <= custom_id <= 1551:
        return 'graphs_gpt4o/medium'
    elif 1552 <= custom_id <= 2231:
        return 'graphs_gpt4o/hard'
    else:
        return None  # For invalid indices

def process_json_files(json_files):
    """Process JSON files and create graph files"""
    # Create directories if they don't exist
    os.makedirs('graphs_gpt4o/easy', exist_ok=True)
    os.makedirs('graphs_gpt4o/medium', exist_ok=True)
    os.makedirs('graphs_gpt4o/hard', exist_ok=True)
    
    for json_file in json_files:
        try:
            with open(json_file, 'r') as f:
                data = json.load(f)
                
            for entry in data:
                custom_id = entry['custom_id']
                print(f"Processing custom_id: {custom_id}")  # Debugging
                
                # Determine folder based on custom_id
                folder = determine_folder(custom_id)
                if folder:
                    print(f"Assigned folder: {folder}")  # Debugging
                    
                    filename = os.path.join(folder, f'graph_{custom_id}.txt')
                    content = entry['response']['body']['choices'][0]['message']['content']
                    
                    # Parse and create graph file
                    graph_dict = parse_graph_content(content)
                    if graph_dict:
                        create_graph_file(graph_dict, filename)
                    else:
                        print(f"Failed to process custom_id {custom_id}")
                else:
                    print(f"Custom ID {custom_id} is out of range and will be skipped.")
                    
        except Exception as e:
            print(f"Error processing {json_file}: {e}")

def main():
    # Specify your JSON files here
    json_files = ['inputs/indirect_connectivity_gpt-4o.json']  # Replace with your actual filenames
    process_json_files(json_files)

if __name__ == "__main__":
    main()
