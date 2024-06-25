# PATCH! Psychometrics-AssisTed benCHmarking of Large Language Models: A Case Study of Mathematics Proficiency
 
This is the code repo for our paper [Psychometrics-AssisTed benCHmarking of Large Language Models: A Case Study of Mathematics Proficiency](https://arxiv.org/abs/2404.01799).

Four evaluation datasets based on [TIMSS 2008 and 2011](https://nces.ed.gov/timss/released-questions.asp) can be downloaded separately [here](https://zenodo.org/records/12531906). 

### Instructions
1. Download and unzip the evaluation datasets in the `data` folder.
2. You will also need to create a separate csv file denoting whether a test item is a multiple choice (MC), in the following format:

```angular2html
| Item    | MC |
|---------|----|
| M032064 | 0  |
| M032094 | 1  |
| M032166 | 1  |
```
3. Define the following variables in `utils.py`:

```angular2html
folder_path = "insert folder path"
latex_file_name = "insert the name of the latex file of the evaluation dataset"
test_item_info_file_name = "insert name of the info file for test items (whether they are multiple choices)"
open_ai_api_key = "insert open ai api key"
google_api_key = "insert google api key"
```

4. To sample responses from gemini and gpt4, run:
```angular2html
python gemini.py
python gpt4.py
```

5. Grade the responses from gemini and gpt4 manually

6. To analyse the graded responses, use the `analysis.R` script under `analysis` folder.