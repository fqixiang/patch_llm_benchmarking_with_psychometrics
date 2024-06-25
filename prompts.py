system_prompt_base = """
You are given a math question written in LaTeX format.

Instructions: 
1. Type of question: Is it multiple choice, free text response, or drawing?
2. Think step by step, and describe your thought process and reasoning.
3. Answer: 
- For multiple choice: [selected answer key].
- For free-text response: [provide your short answer].
- For drawing: [describe clearly the steps to complete the drawing].
- If uncertain, make an educated guess.
"""

# variant 1: vary the order of MC, free-text and drawing response questions.
system_prompt_variant_1 = """
You are given a math question written in LaTeX format.

Instructions: 
1. Type of question: Is it drawing, free text response, or multiple choice?
2. Think step by step, and describe your thought process and reasoning.
3. Answer: 
- For drawing: [describe clearly the steps to complete the drawing].
- For free-text response: [provide your short answer].
- For multiple choice: [selected answer key].
- If uncertain, make an educated guess.
"""

# variant 2: add #### heading indicator
system_prompt_variant_2 = """
You are given a math question written in LaTeX format.

#### Instructions: 
1. Type of question: Is it multiple choice, free text response, or drawing?
2. Think step by step, and describe your thought process and reasoning.
3. Answer: 
- For multiple choice: [selected answer key].
- For free-text response: [provide your short answer].
- For drawing: [describe clearly the steps to complete the drawing].
- If uncertain, make an educated guess.
"""

# variant 3: remove new lines
system_prompt_variant_3 = """
You are given a math question written in LaTeX format.
Instructions: 
1. Type of question: Is it multiple choice, free text response, or drawing?
2. Think step by step, and describe your thought process and reasoning.
3. Answer: 
- For multiple choice: [selected answer key].
- For free-text response: [provide your short answer].
- For drawing: [describe clearly the steps to complete the drawing].
- If uncertain, make an educated guess.
"""

# variant 4: upper case heading
system_prompt_variant_4 = """
You are given a math question written in LaTeX format.

INSTRUCTIONS: 
1. Type of question: Is it multiple choice, free text response, or drawing?
2. Think step by step, and describe your thought process and reasoning.
3. Answer: 
- For multiple choice: [selected answer key].
- For free-text response: [provide your short answer].
- For drawing: [describe clearly the steps to complete the drawing].
- If uncertain, make an educated guess.
"""

# variant 5: lower case heading
system_prompt_variant_5 = """
You are given a math question written in LaTeX format.

instructions: 
1. Type of question: Is it multiple choice, free text response, or drawing?
2. Think step by step, and describe your thought process and reasoning.
3. Answer: 
- For multiple choice: [selected answer key].
- For free-text response: [provide your short answer].
- For drawing: [describe clearly the steps to complete the drawing].
- If uncertain, make an educated guess.
"""

# variant 6: remove : from heading
system_prompt_variant_6 = """
You are given a math question written in LaTeX format.

Instructions
1. Type of question: Is it multiple choice, free text response, or drawing?
2. Think step by step, and describe your thought process and reasoning.
3. Answer: 
- For multiple choice: [selected answer key].
- For free-text response: [provide your short answer].
- For drawing: [describe clearly the steps to complete the drawing].
- If uncertain, make an educated guess.
"""

# variant 7: add new lines
system_prompt_variant_7 = """
You are given a math question written in LaTeX format.

Instructions: 

1. Type of question: Is it multiple choice, free text response, or drawing?

2. Think step by step, and describe your thought process and reasoning.

3. Answer: 
- For multiple choice: [selected answer key].

- For free-text response: [provide your short answer].

- For drawing: [describe clearly the steps to complete the drawing].

- If uncertain, make an educated guess.
"""

# variant 8: replace 1. 2. 3. with first, second, third.
system_prompt_variant_8 = """
You are given a math question written in LaTeX format.

Instructions: 
First, type of question: Is it multiple choice, free text response, or drawing?
Second, think step by step, and describe your thought process and reasoning.
Third, answer: 
- For multiple choice: [selected answer key].
- For free-text response: [provide your short answer].
- For drawing: [describe clearly the steps to complete the drawing].
- If uncertain, make an educated guess.
"""

# variant 9: add ; between steps
system_prompt_variant_9 = """
You are given a math question written in LaTeX format.

Instructions: 
1. Type of question: Is it multiple choice, free text response, or drawing?;
2. Think step by step, and describe your thought process and reasoning;
3. Answer: 
- For multiple choice: [selected answer key].
- For free-text response: [provide your short answer].
- For drawing: [describe clearly the steps to complete the drawing].
- If uncertain, make an educated guess.
"""

# variant 10: lower case all sentences
system_prompt_variant_10 = """
you are given a math question written in LaTeX format.

instructions: 
1. type of question: is it multiple choice, free text response, or drawing?;
2. think step by step, and describe your thought process and reasoning;
3. answer: 
- for multiple choice: [selected answer key].
- for free-text response: [provide your short answer].
- for drawing: [describe clearly the steps to complete the drawing].
- if uncertain, make an educated guess.
"""

# major variant 1
system_prompt_base_remove_instructions = """
You are given a math question written in LaTeX format.
Think step by step, describe your thought process and reasoning, and give your answer.
"""

# major variant 2
system_prompt_base_remove_cot = """
You are given a math question written in LaTeX format.

Instructions: 
1. Type of question: Is it multiple choice, free text response, or drawing?
2. Answer: 
- For multiple choice: [selected answer key].
- For free-text response: [provide your short answer].
- For drawing: [describe clearly the steps to complete the drawing].
- If uncertain, make an educated guess.
"""

# major variant 3
system_prompt_base_remove_instructions_and_cot = """
You are given a math question written in LaTeX format.
"""

system_prompt_variants = {"system_prompt_variant_1": system_prompt_variant_1,
                          "system_prompt_variant_2": system_prompt_variant_2,
                          "system_prompt_variant_3": system_prompt_variant_3,
                          "system_prompt_variant_4": system_prompt_variant_4,
                          "system_prompt_variant_5": system_prompt_variant_5,
                          "system_prompt_variant_6": system_prompt_variant_6,
                          "system_prompt_variant_7": system_prompt_variant_7,
                          "system_prompt_variant_8": system_prompt_variant_8,
                          "system_prompt_variant_9": system_prompt_variant_9,
                          "system_prompt_variant_10": system_prompt_variant_10,
                          "system_prompt_variant_11": system_prompt_base}

# "system_prompt_variant_12": system_prompt_base_remove_instructions,
# "system_prompt_variant_13": system_prompt_base_remove_cot,
# "system_prompt_variant_14": system_prompt_base_remove_instructions_and_cot