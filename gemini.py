from tqdm import tqdm
import re
import google.generativeai as genai
import PIL.Image
import polars as pl
import time
from prompts import system_prompt_variants
from utils import folder_path, latex_file_name, test_item_info_file_name, string_in_file, google_api_key
import os

# load test items in latex
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
    genai.configure(api_key=google_api_key)
    for variant, instruction in system_prompt_variants.items():
        results_file_name = f"gemini_results_{variant}.txt"
        for item_pos in tqdm(range(len(parsed_data))):
            if len(parsed_data) != 88:
                exit("Incorrect number of test items.")

            item_id = parsed_data[item_pos]["item_id"]
            if os.path.exists(results_file_name) and string_in_file(results_file_name, item_id):
                continue

            image_paths = parsed_data[item_pos]["image_paths"]
            test_item = parsed_data[item_pos]["item_content"]
            multiple_choice = items_mc[item_id]
            temperature = 0.3 if multiple_choice == 1 else 0.6

            test_item_lines = []
            for line in test_item.split('\n'):
                if not line.strip().startswith('\\includegraphics'):
                    test_item_lines.append(line)
            test_item_clean = '\n'.join(test_item_lines)

            prompt = [instruction, test_item_clean]
            if len(image_paths) > 0:
                model = genai.GenerativeModel('gemini-pro-vision')
                for image_path in image_paths:
                    image_path = folder_path + "images/" + image_path + ".jpg"
                    image = PIL.Image.open(image_path)
                    prompt.append(image)
            else:
                model = genai.GenerativeModel('gemini-pro')

            response = model.generate_content(prompt,
                                              generation_config=genai.types.GenerationConfig(temperature=temperature))

            try:
                response_text = response.text
            except:
                response_text = "Something went wrong."

            with open(results_file_name, 'a', encoding="utf-8") as file:
                # Write the string to the file
                file.write("\section{" + item_id + "}" + '\n')
                file.write(response_text + '\n-------------------------------------------\n')

            time.sleep(10)

if __name__ == "__main__":
    main()

