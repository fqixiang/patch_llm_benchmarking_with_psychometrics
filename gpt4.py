import re
from tqdm import tqdm
import polars as pl
import time
from prompts import system_prompt_variants
from utils import create_payload, query_openai, folder_path, latex_file_name, test_item_info_file_name, string_in_file
import os

# test items in latex
with open(folder_path+latex_file_name, 'r') as file:
    # Read the content of the file
    latex_content = file.read()

# load test item type information
items_df = pl.read_csv(folder_path+test_item_info_file_name).select(pl.col("Item", "MC"))
items_mc = dict(items_df.iter_rows())

# Regular expression to parse the test items
section_pattern = re.compile(r'\\section\*{([^}]+)}')
image_pattern = re.compile(r'\\includegraphics.*?{([^}]+)}')
content_pattern = re.compile(r'\\section\*{([^}]+)}\s*(.*?)(?=\\section\*{|\\newpage|\Z)', re.DOTALL)

parsed_data = []
matches = content_pattern.findall(latex_content)

for match in matches:
    item_id = match[0]
    item_content = match[1].strip()

    image_paths = image_pattern.findall(match[1])
    parsed_data.append({
        "item_id": item_id,
        "item_content": item_content,
        "image_paths": image_paths
    })

def main():
    for variant, instruction in system_prompt_variants.items():
        results_file_name = f"gpt4_results_{variant}.txt"
        for item_pos in tqdm(range(len(parsed_data))):
            if len(parsed_data) != 88:
                exit("Incorrect number of test items.")

            item_id = parsed_data[item_pos]["item_id"]
            if os.path.exists(results_file_name) and string_in_file(results_file_name, item_id):
                continue
            image_paths = parsed_data[item_pos]["image_paths"]
            prompt_raw = parsed_data[item_pos]["item_content"]
            multiple_choice = items_mc[item_id]

            output_lines = []
            for line in prompt_raw.split('\n'):
                if not line.strip().startswith('\\includegraphics'):
                    output_lines.append(line)
            prompt_clean = '\n'.join(output_lines)

            payload = create_payload(image_paths,
                                     instruction,
                                     prompt_clean,
                                     multiple_choice)

            response = query_openai(payload)

            if 'choices' in response:
                response_text = response['choices'][0]['message']['content']
            else:
                response_text = "No choices available in the response."

            with open(f"results_{variant}.txt", 'a', encoding="utf-8") as file:
                # Write the string to the file
                file.write("\section{" + item_id + "}" + '\n')
                file.write(response_text + '\n-------------------------------------------\n')

            time.sleep(30)

if __name__ == "__main__":
    main()