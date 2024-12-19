import json
import re

# Paths to your files
result_file = 'results/llm_gpt-4o/few_shot_connectivity/batch_input_0_results.json'
ground_truth_file = 'dataset/connectivity.json'


def extract_answer_from_result(content):
    """Extract the answer inside {{}} from the result.json content."""
    match = re.search(r'\{\{(.+?)\}\}', content)
    if match:
        # Normalize the extracted answer
        return match.group(1).strip().lower()
    return None

def normalize_ground_truth_answer(answer):
    """Normalize the ground truth answer to match the format of the extracted result."""
    # Extract "yes" or "no" from "The answer is yes/no."
    if "yes" in answer.lower():
        return "yes"
    elif "no" in answer.lower():
        return "no"
    return None

def load_result_data(file_path):
    """Load data from a .json or .jsonl file."""
    data = []
    if file_path.endswith('.jsonl'):
        # Process JSON Lines file
        with open(file_path, 'r') as file:
            for line in file:
                data.append(json.loads(line))
    elif file_path.endswith('.json'):
        # Process standard JSON file
        with open(file_path, 'r') as file:
            data = json.load(file)
    else:
        raise ValueError("Unsupported file format. Please use a .json or .jsonl file.")
    return data

def evaluate_accuracy(result_file, ground_truth_file):
    # Load the result and ground truth files
    result_data = load_result_data(result_file)
    with open(ground_truth_file, 'r') as ground_f:
        ground_truth_data = json.load(ground_f)

    correct = 0
    total = 0

    # Statistics for difficulty levels
    difficulty_stats = {
        "easy": {"correct": 0, "total": 0},
        "medium": {"correct": 0, "total": 0},
        "hard": {"correct": 0, "total": 0},
    }

    # Detailed statistics
    stats = {
        "total_yes": 0,
        "total_no": 0,
        "correct_yes": 0,
        "correct_no": 0,
        "incorrect_yes": 0,
        "incorrect_no": 0,
    }

    # Iterate over each result and compare with ground truth
    for result_entry in result_data:
        custom_id = result_entry.get("custom_id")
        if custom_id in ground_truth_data:
            # Extract the result and ground truth answers
            result_content = result_entry.get("response", {}).get("body", {}).get("choices", [{}])[0].get("message", {}).get("content", "")
            result_answer = extract_answer_from_result(result_content)
            ground_truth_entry = ground_truth_data[custom_id]
            ground_truth_answer = normalize_ground_truth_answer(ground_truth_entry["answer"])
            difficulty = ground_truth_entry.get("difficulty", "unknown").lower()

            if result_answer and ground_truth_answer:
                total += 1
                # Update overall stats
                if result_answer == "yes":
                    stats["total_yes"] += 1
                    if result_answer == ground_truth_answer:
                        stats["correct_yes"] += 1
                        correct += 1
                    else:
                        stats["incorrect_yes"] += 1
                elif result_answer == "no":
                    stats["total_no"] += 1
                    if result_answer == ground_truth_answer:
                        stats["correct_no"] += 1
                        correct += 1
                    else:
                        stats["incorrect_no"] += 1

                # Update difficulty-specific stats
                if difficulty in difficulty_stats:
                    difficulty_stats[difficulty]["total"] += 1
                    if result_answer == ground_truth_answer:
                        difficulty_stats[difficulty]["correct"] += 1

    # Calculate overall accuracy
    accuracy = (correct / total) * 100 if total > 0 else 0

    # Calculate accuracy for each difficulty level
    for diff, stats in difficulty_stats.items():
        stats["accuracy"] = (stats["correct"] / stats["total"]) * 100 if stats["total"] > 0 else 0

    return accuracy, correct, total, stats, difficulty_stats

# Run the evaluation
accuracy, correct, total, stats, difficulty_stats = evaluate_accuracy(result_file, ground_truth_file)

# Print the results
print(f"Overall Accuracy: {accuracy:.2f}% ({correct}/{total})")
# print(f"Total 'Yes' Predictions: {stats['total_yes']} (Correct: {stats['correct_yes']}, Incorrect: {stats['incorrect_yes']})")
# print(f"Total 'No' Predictions: {stats['total_no']} (Correct: {stats['correct_no']}, Incorrect: {stats['incorrect_no']})")
print("\nAccuracy by Difficulty Level:")
for diff, stats in difficulty_stats.items():
    print(f"  {diff.capitalize()}: {stats['accuracy']:.2f}% (Correct: {stats['correct']}, Total: {stats['total']})")
