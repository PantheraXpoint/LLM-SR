import json
import os

def analyze_arrays(llm_output_file, ground_truth_file):
    # Load the JSON data from TXT files
    with open(llm_output_file, 'r') as f:
        llm_output = json.loads(f.read().strip())

    with open(ground_truth_file, 'r') as f:
        ground_truth = json.loads(f.read().strip())

    # Initialize counters and containers
    less_than_count = 0
    equal_count = 0
    greater_than_count = 0
    greater_than_indexes = []
    total_indexes = len(llm_output)
    all_false_count = 0
    at_least_one_true_count = 0
    at_least_one_false_count = 0

    # Iterate through llm_output indexes
    for index, llm_values in llm_output.items():
        # Categorize indexes based on values
        if all(value == "false" for value in llm_values):
            all_false_count += 1
        if any(value == "true" for value in llm_values):
            at_least_one_true_count += 1
        if any(value == "false" for value in llm_values):
            at_least_one_false_count += 1

        # Check if all values in llm_output are "true"
        if all(value == "true" for value in llm_values):
            # Get the corresponding ground_truth values
            ground_truth_values = ground_truth.get(index, [])

            # Compare lengths
            llm_length = len(llm_values)
            ground_truth_length = len(ground_truth_values)

            if llm_length < ground_truth_length:
                less_than_count += 1
            elif llm_length == ground_truth_length:
                equal_count += 1
            else:
                greater_than_count += 1
                greater_than_indexes.append(index)

    # Prepare results
    results = (
        f"Total indexes in llm_output: {total_indexes}\n"
        f"Less than: {less_than_count}\n"
        f"Equal: {equal_count}\n"
        f"Greater than: {greater_than_count}\n"
        f"Indexes with greater than length: {greater_than_indexes}\n"
        f"Count of indexes with all false values: {all_false_count}\n"
        f"Count of indexes with at least one true value: {at_least_one_true_count}\n"
        f"Count of indexes with at least one false value: {at_least_one_false_count}\n"
    )

    # Print results to console
    print(results)

    # Save results to a file in the 'scores' directory
    base_dir = os.path.dirname(os.path.dirname(llm_output_file))  # Move up one level to the parent directory
    scores_dir = os.path.join(base_dir, 'scores')
    os.makedirs(scores_dir, exist_ok=True)  # Create the 'scores' directory if it doesn't exist

    output_file_name = os.path.basename(llm_output_file)
    output_file_path = os.path.join(scores_dir, output_file_name)

    with open(output_file_path, 'w') as f:
        f.write(results)

    print(f"Results saved to: {output_file_path}")

# File paths
llm_output_file = '/home/quang/CS579-TermProject/shortest_path/results/indirect_shortest_path_gpt-4o_hard.txt'  # Replace with the actual path to your LLM output file
ground_truth_file = '/home/quang/CS579-TermProject/shortest_path/results/prolog_shortest_paths_hard.txt'  # Replace with the actual path to your ground truth file

# Call the function
analyze_arrays(llm_output_file, ground_truth_file)