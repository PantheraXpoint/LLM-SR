import json
import re

def extract_path_from_answer(answer):
    """Extract the path from an answer string"""
    # Look for the pattern "path from node X to node Y is A,B,C"
    path_match = re.search(r'path .* is ([0-9,]+)', answer)
    if path_match:
        # Extract the path part and convert to list of integers
        path_str = path_match.group(1)
        return [int(x) for x in path_str.split(',')]
    return []

def process_answers(input_file, output_file):
    """Process the input JSON and create new JSON with just the paths"""
    # Read the input JSON file
    try:
        with open(input_file, 'r') as f:
            data = json.load(f)
    except Exception as e:
        print(f"Error reading input file: {e}")
        return

    # Extract paths from answers
    paths = {}
    for key, value in data.items():
        try:
            if 'answer' in value:
                path = extract_path_from_answer(value['answer'])
                if path:
                    paths[key] = path
            else:
                paths[key] = []
        except Exception as e:
            print(f"Error processing entry {key}: {e}")
            paths[key] = []

    # Write the extracted paths to output file with compact formatting
    try:
        with open(output_file, 'w') as f:
            json.dump(paths, f, separators=(',', ':'))
        print(f"Successfully wrote paths to {output_file}")
    except Exception as e:
        print(f"Error writing output file: {e}")

def main():
    input_file = 'inputs/ex.json'  # Your input JSON file
    output_file = 'gt_paths/examples.json'  # The output file for path.py
    
    print("Starting path extraction...")
    process_answers(input_file, output_file)
    print("Path extraction complete!")

if __name__ == "__main__":
    main()