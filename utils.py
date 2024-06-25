import base64
import mimetypes
import requests

folder_path = "insert folder path"
latex_file_name = "insert latex file name"
test_item_info_file_name = "insert name of the info file for test items"

open_ai_api_key = "insert open ai api key"
google_api_key = "insert google api key"

def encode_image(image_path: str):
    """Encodes an image to base64 and determines the correct MIME type."""
    mime_type, _ = mimetypes.guess_type(image_path)
    if mime_type is None:
        raise ValueError(f"Cannot determine MIME type for {image_path}")

    with open(image_path, "rb") as image_file:
        encoded_string = base64.b64encode(image_file.read()).decode('utf-8')
        return f"data:{mime_type};base64,{encoded_string}"


def create_payload(image_paths: list[str],
                   instruction: str,
                   test_item: str,
                   multiple_choice: int,
                   model="gpt-4-vision-preview",
                   max_tokens=3000,
                   detail="auto",
                   seed=1):
    """Creates the payload for the API request."""
    messages = [
        {
            "role": "system",
            "content": instruction
        },
        {
            "role": "user",
            "content": [
                {
                    "type": "text",
                    "text": test_item,
                },
            ],
        },
    ]

    temperature = 0.3 if multiple_choice == 1 else 0.6

    if len(image_paths) > 0:
        for image_path in image_paths:
            image_path = folder_path + "images/" + image_path + ".jpg"
            base64_image = encode_image(image_path)
            messages[1]["content"].append({
                "type": "image_url",
                "image_url": {
                    "url": base64_image,
                    "detail": detail,
                }
            })

    return {
        "model": model,
        "messages": messages,
        "max_tokens": max_tokens,
        "seed": seed,
        "temperature": temperature
    }

def query_openai(payload):
    """Sends a request to the OpenAI API and prints the response."""
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {open_ai_api_key}"
    }
    response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)
    return response.json()


def string_in_file(file_path, search_string):
    try:
        with open(file_path, 'r') as file:
            for line in file:
                if search_string in line:
                    next_line = next(file, None)
                    if next_line and "No choices available in the response." not in next_line and "Something went wrong." not in next_line and "I cannot assist with this request." not in next_line:
                        return True
        return False
    except FileNotFoundError:
        print("File not found.")
        return False
