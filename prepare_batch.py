import os
import json
from tqdm import tqdm

# Function to load the JSON file containing questions
def load_questions(json_file):
    with open(json_file, 'r') as f:
        data = json.load(f)
    return data

def load_few_shot_examples(few_shot_file):
    # few_shot_file is expected to contain a JSON array of strings (the examples)
    with open(few_shot_file, 'r') as f:
        examples = json.load(f)
    return examples

# Function to prepare batch files for requesting all possible shortest paths
def prepare_batch_files(task_name, data, batch_dir, batch_size=3000, model_name=None):
    requests = []
    batch_count = 0
    
    print(model_name)

    for i, (key, entry) in enumerate(tqdm(data.items(), desc="Preparing batch files"), start=1):
        question_text = entry["question"].strip()
        
        # Load few-shot examples if the task is few_shot_connectivity
        few_shot_examples = []
        if task_name == 'few_shot_connectivity':
            few_shot_file = 'dataset/few_shot_connectivity.json'
            few_shot_examples = load_few_shot_examples(few_shot_file)  # array of example strings
        elif task_name == 'few_shot_shortest_path':
            few_shot_file = 'dataset/few_shot_shortest_path.json'
            few_shot_examples = load_few_shot_examples(few_shot_file)
        
        # Remove any "A:" that might be present at the end of the question
        if question_text.endswith("A:"):
            question_text = question_text[:-2].strip()

        # Modify the question to ask for all possible shortest paths in the required format
        if task_name=='shortest_path':
            prompt = (
                f"{question_text.replace('Give the shortest path', 'Give all possible shortest paths')}\n\n"
                "Please provide shortest paths within double curly brackets, where each element should be a shortest path: {{[path 1], [path 2],... [path n]}}\n"
                "A:"
            )
        elif task_name=='extracted_shortest_path':
        # Modify the prompt to ask for a dictionary with graph details and clarify not to find the shortest path
            # Construct the prompt by adding instructions and then appending "A:"
            prompt = (
                f"{question_text}\n\n"
                "Given the above question and information, please do not find the shortest path. "
                "Instead, generate a dictionary with the following information:\n"
                "- Number of nodes\n"
                "- Source node\n"
                "- Target node\n"
                "- List of edges, where each edge is represented as a tuple of (node1, node2, weight).\n\n"
                "Please put your answer in double curly brackets format:\n"
                "{{'num_nodes': <int>, 'source': <int>, 'target': <int>, 'edges': [(node1, node2, weight), ...]}}\n\n"
                "A:"
            )
        elif task_name=='few_shot_shortest_path':
            examples_str = "\n\n".join(few_shot_examples)
            # Now append the new question in the same format:
            # The question should end with:
            # "Please put your answer in double curly brackets format: {{Yes}} or {{No}}.\nA:"
            prompt = (
                f"{examples_str}\n\n" 
                f"{question_text.replace('Give the shortest path', 'Give all possible shortest paths')}\n\n"
                "Please provide shortest paths within double curly brackets, where each element should be a shortest path: {{[path 1], [path 2],... [path n]}}\n"
                "A:"
            )
        elif task_name=='connectivity':
            prompt = (
                f"{question_text}. Please put your answer in double curly brackets format: {{Yes}} or {{No}}.\n"
                "A:"
            )
        elif task_name=='extracted_connectivity':
            prompt = (
                f"{question_text}\n\n"
                "Given the above question and information, please do not find the connectivity. " 
                "Instead, generate a dictionary with the following information:\n"
                "- Number of nodes\n"
                "- Source node\n"
                "- Target node\n"
                "- List of edges, where each edge is represented as a tuple of (node1, node2).\n\n"
                "Please put all of information inside a double curly brackets format:\n"
                "{{'num_nodes': <int>, 'source': <int>, 'target': <int>, 'edges': [(node1, node2), ...]}}\n"
                "A:"
            )
        elif task_name=='few_shot_connectivity':
            # Construct a prompt that first includes the few-shot examples, then the new question
            # The few-shot examples are already in the final desired format (with Q and A)
            # We'll just join them with a couple of new lines before presenting the new question
            examples_str = "\n\n".join(few_shot_examples)
            # Now append the new question in the same format:
            # The question should end with:
            # "Please put your answer in double curly brackets format: {{Yes}} or {{No}}.\nA:"
            prompt = (
                f"{examples_str}\n\n" 
                f"{question_text} Please put your answer in double curly brackets format: {{Yes}} or {{No}}.\n"
                "A:"
            )

        # print("model_name: ", model_name)
        # Create the request structure
        request = {
            "custom_id": key,
            "method": "POST",
            "url": "/v1/chat/completions",
            "body": {
                "model": model_name,
                "messages": [
                    {
                        "role": "user",
                        "content": prompt
                    }
                ],
                "max_tokens": 4096,
                "temperature": 0
            }
        }
        requests.append(request)

        # Write the batch file once we reach the batch size limit
        if len(requests) == batch_size:
            batch_file_path = os.path.join(batch_dir, f'batch_input_{batch_count}.jsonl')
            with open(batch_file_path, 'w') as file:
                for request in requests:
                    file.write(json.dumps(request) + '\n')
            requests = []
            batch_count += 1

    # Write any remaining requests to a new file
    if requests:
        batch_file_path = os.path.join(batch_dir, f'batch_input_{batch_count}.jsonl')
        with open(batch_file_path, 'w') as file:
            for request in requests:
                file.write(json.dumps(request) + '\n')

if __name__ == '__main__':
    
    # task_name = 'shortest_path'
    json_filename = 'connectivity'
    # task_name = 'few_shot_shortest_path'
    # task_name = 'shortest_path'
    # task_name = 'extracted_shortest_path'
    
    task_name = 'few_shot_connectivity'
    # task_name = 'connectivity'
    # task_name = 'extracted_connectivity'
    model_name = 'gpt-4o'
    
    
    # Specify the JSON file path with questions
    json_file = f'dataset/{json_filename}.json'
    # Specify the directory for output batch JSONL files
    batch_dir = f'batch_files/{task_name}_{model_name}'
    os.makedirs(batch_dir, exist_ok=True)
    
    # Load data and prepare batches
    data = load_questions(json_file)
    print("model: ", model_name)
    prepare_batch_files(task_name, data, batch_dir, 3000, model_name)
