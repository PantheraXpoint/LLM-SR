import os
import json
import time
import requests
from tqdm import tqdm

# Load OpenAI API Key from file
def load_api_key(file_path):
    with open(file_path, 'r') as file:
        return file.read().strip()

# Function to upload the batch file to OpenAI
def upload_batch_file(batch_file_path):
    response = requests.post(
        "https://api.openai.com/v1/files",
        headers={
            "Authorization": f"Bearer {API_KEY}"
        },
        files={
            "file": (batch_file_path, open(batch_file_path, 'rb')),
            "purpose": (None, "batch")
        }
    )
    response.raise_for_status()
    return response.json()

# Function to create a batch job
def create_batch_job(file_id):
    response = requests.post(
        "https://api.openai.com/v1/batches",
        headers={
            "Authorization": f"Bearer {API_KEY}",
            "Content-Type": "application/json"
        },
        json={
            "input_file_id": file_id,
            "endpoint": "/v1/chat/completions",
            "completion_window": "24h",
            "metadata": {
                "description": "OCR batch processing"
            }
        }
    )
    response.raise_for_status()
    return response.json()

# Function to check batch status
def check_batch_status(batch_id):
    response = requests.get(
        f"https://api.openai.com/v1/batches/{batch_id}",
        headers={
            "Authorization": f"Bearer {API_KEY}"
        }
    )
    response.raise_for_status()
    return response.json()

# Function to download the results
def download_results(file_id, output_json_path):
    response = requests.get(
        f"https://api.openai.com/v1/files/{file_id}/content",
        headers={
            "Authorization": f"Bearer {API_KEY}"
        }
    )
    response.raise_for_status()
    
    # Get the raw content and split by new lines
    raw_content = response.text.splitlines()
    
    # Parse each line as a separate JSON object
    json_content = [json.loads(line) for line in raw_content]

    # Save as a UTF-8 encoded file
    with open(output_json_path, 'w', encoding='utf-8') as file:
        json.dump(json_content, file, ensure_ascii=False, indent=4)
    
    print(f"Results saved to {output_json_path}")

# Function to upload and process a single batch file
def process_single_batch_file(batch_file, output_json_dir):
    result_file_path = os.path.join(output_json_dir, f"{os.path.basename(batch_file).replace('.jsonl', '')}_results.json")
    
    # Check if result file already exists
    if os.path.exists(result_file_path):
        print(f"Result file already exists for {batch_file}, skipping upload.")
        return

    # Upload the batch file and create a batch job
    batch_input_file = upload_batch_file(batch_file)
    batch = create_batch_job(batch_input_file['id'])
    batch_id = batch['id']
    print(f"Batch job created with ID: {batch_id} for file: {batch_file}")

    # Poll for batch status
    while True:
        batch_status = check_batch_status(batch_id)
        status = batch_status['status']
        print(f"Batch status for {batch_id}: {status}")
        print('/---------------------------------------/\n')
        if status in ['completed', 'failed', 'expired', 'cancelled']:
            break
        time.sleep(10)  # Wait for 60 seconds before checking again

    # Download the results if the batch is completed
    if status == 'completed':
        output_file_id = batch_status['output_file_id']
        download_results(output_file_id, result_file_path)

        # Map results back to input files
        with open(result_file_path, 'r', encoding='utf-8') as file:
            results = json.load(file)
            for result in results:
                custom_id = result['custom_id']
                # request_index, image_path = custom_id.split(':', 1)
                ocr_result = result['response']['body']['choices'][0]['message']['content']
                # print(f"Image: {image_path}, OCR Result: {ocr_result}")

if __name__ == '__main__':
    # OpenAI API Key
    # task_name = 'connectivity'
    # task_name = 'few_shot_connectivity'
    # task_name = 'extracted_connectivity'
    
    # task_name = 'shortest_path'
    # task_name = 'few_shot_shortest_path'
    task_name = 'extracted_shortest_path'
    method_name = 'llm'
    model_name = 'gpt-4o'
    
    API_KEY = load_api_key('api_key.txt')
    batch_dir = f'batch_files/{task_name}_{model_name}'  # Directory containing batch JSONL files
    output_json_dir = f'results/{method_name}_{model_name}/{task_name}'  # Directory to save OCR results

    os.makedirs(output_json_dir, exist_ok=True)

    # Get list of batch files
    batch_files = [os.path.join(batch_dir, f) for f in os.listdir(batch_dir) if f.endswith('.jsonl')]

    for batch_file in tqdm(batch_files, desc="Processing batch files"):
        process_single_batch_file(batch_file, output_json_dir)
