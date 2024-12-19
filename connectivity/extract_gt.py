import json

def extract_answer(input_path, output_path):
    with open(input_path, 'r') as f:
        data = json.load(f)
    
    answers = {}
    for idx in data:
        answer = data[idx]["answer"].lower()
        answers[idx] = ["yes"] if "yes" in answer else ["no"]
    
    with open(output_path, 'w') as f:
        json.dump(answers, f, indent=4)

# Example usage:
extract_answer("connectivity/inputs/ex.json", "connectivity/gt_ans/examples.json")