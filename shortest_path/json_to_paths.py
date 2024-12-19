import json
import ast

def parse_shortest_paths(content):
    """Extract the shortest paths from the 'content' field."""
    start = content.find("{{")
    end = content.find("}}")
    if start == -1 or end == -1:
        # No shortest paths format found, return empty list
        return []
    
    paths_str = content[start + 2:end].strip()  # Extract the substring between {{ and }}
    if not paths_str:
        # If there's nothing between {{ and }}, return empty list
        return []
    
    # Attempt to parse the extracted string as a Python list
    # Replace single quotes with double quotes for valid JSON
    try:
        paths_json = paths_str.replace("'", '"')
        paths = ast.literal_eval(paths_json)
        
        # Ensure that paths is always a list of lists
        # If it's a single path (list of ints), wrap it in another list
        if paths and isinstance(paths[0], int):
            paths = [paths]
        
        return paths
    except SyntaxError as e:
        print(f"SyntaxError while parsing shortest paths: {e}\nContent: {content}")
        return []
    except Exception as e:
        print(f"Error parsing shortest paths: {e}\nContent: {content}")
        return []

def process_json_file(input_file, output_file):
    """Process the input JSON file and store shortest paths in the output JSON file."""
    results = {}

    try:
        with open(input_file, 'r') as f:
            data = json.load(f)

        for entry in data:
            custom_id = entry['custom_id']
            content = entry['response']['body']['choices'][0]['message']['content']
            
            # Parse the shortest paths
            print(f"Processing custom_id: {custom_id}")  # Debugging
            shortest_paths = parse_shortest_paths(content)
            
            # Always include custom_id in the results, even if empty
            results[custom_id] = shortest_paths

        # Write results to the output JSON file
        with open(output_file, 'w') as f:
            json.dump(results, f, indent=4)
        print(f"Results successfully written to {output_file}")
    
    except Exception as e:
        print(f"Error processing file: {e}")

def main():
    input_file = '/home/quang/CS579-TermProject/shortest_path/inputs/direct_shortest_path_gpt-4o.json'  # Replace with the actual input file path
    output_file = '/home/quang/CS579-TermProject/shortest_path/prolog_paths_gen/direct_shortest_path_gpt-4o.json'  # Replace with the desired output file path
    process_json_file(input_file, output_file)

if __name__ == "__main__":
    main()
