import json
import os

def get_difficulty(index):
    """Determine the difficulty level based on the index."""
    num = int(index)  # Convert index to integer
    if num <= 351:
        return "easy"
    elif num <= 1551:
        return "medium"
    else:
        return "hard"

def analyze_arrays(llm_output_file, ground_truth_file):
    # Load the JSON data from TXT files
    with open(llm_output_file, 'r') as f:
        llm_output = json.loads(f.read().strip())

    with open(ground_truth_file, 'r') as f:
        ground_truth = json.loads(f.read().strip())

    # Initialize counters and containers for each difficulty level
    difficulty_counters = {
        "easy": {"less_than": 0, "equal": 0, "greater_than": 0, "greater_indexes": [], 
                 "all_false": 0, "at_least_one_true": 0, "at_least_one_false": 0, "total": 0},
        "medium": {"less_than": 0, "equal": 0, "greater_than": 0, "greater_indexes": [], 
                   "all_false": 0, "at_least_one_true": 0, "at_least_one_false": 0, "total": 0},
        "hard": {"less_than": 0, "equal": 0, "greater_than": 0, "greater_indexes": [], 
                 "all_false": 0, "at_least_one_true": 0, "at_least_one_false": 0, "total": 0},
    }

    # Iterate through llm_output indexes
    for index, llm_values in llm_output.items():
        difficulty = get_difficulty(index)
        counters = difficulty_counters[difficulty]
        counters["total"] += 1

        # Categorize indexes based on values
        if all(value == "false" for value in llm_values):
            counters["all_false"] += 1
        if any(value == "true" for value in llm_values):
            counters["at_least_one_true"] += 1
        if any(value == "false" for value in llm_values):
            counters["at_least_one_false"] += 1

        # Check if all values in llm_output are "true"
        if all(value == "true" for value in llm_values):
            # Get the corresponding ground_truth values
            ground_truth_values = ground_truth.get(index, [])

            # Compare lengths
            llm_length = len(llm_values)
            ground_truth_length = len(ground_truth_values)

            if llm_length < ground_truth_length:
                counters["less_than"] += 1
            elif llm_length == ground_truth_length:
                counters["equal"] += 1
            else:
                counters["greater_than"] += 1
                counters["greater_indexes"].append(index)

    # Save results to files based on difficulty
    base_dir = os.path.dirname(os.path.dirname(llm_output_file))  # Parent directory of 'results'
    scores_dir = os.path.join(base_dir, 'scores')
    os.makedirs(scores_dir, exist_ok=True)  # Create 'scores' directory if it doesn't exist

    # Extract base name of llm_output_file without extension
    base_name = os.path.splitext(os.path.basename(llm_output_file))[0]

    for difficulty, counters in difficulty_counters.items():
        results = (
            f"Difficulty level: {difficulty.capitalize()}\n"
            f"Total indexes: {counters['total']}\n"
            f"Less than: {counters['less_than']}\n"
            f"Equal: {counters['equal']}\n"
            f"Greater than: {counters['greater_than']}\n"
            f"Indexes with greater than length: {counters['greater_indexes']}\n"
            f"Count of indexes with all false values: {counters['all_false']}\n"
            f"Count of indexes with at least one true value: {counters['at_least_one_true']}\n"
            f"Count of indexes with at least one false value: {counters['at_least_one_false']}\n"
        )

        # Generate output file name based on difficulty and llm_output_file
        output_file_name = f"{base_name}_{difficulty}.txt"
        output_file_path = os.path.join(scores_dir, output_file_name)

        with open(output_file_path, 'w') as f:
            f.write(results)

        print(f"Results for {difficulty} saved to: {output_file_path}")

# File paths
llm_output_file = '/home/quang/CS579-TermProject/connectivity/results/gpt4o_results.txt'  # Replace with the actual path to your LLM output file
ground_truth_file = '/home/quang/CS579-TermProject/connectivity/results/prolog_results.txt'  # Replace with the actual path to your ground truth file

# Call the function
analyze_arrays(llm_output_file, ground_truth_file)
