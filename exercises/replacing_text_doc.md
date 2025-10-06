Since we will usually have the same texts in R, Python and Quarto, we need an approach that will allow to keep those texts in sync.

Solution is putting placeholders in the .ipynb or .qmd files and then replacing them with the actual text stored in another .ipynb file.

# Requirements
Assumes python 3.10+

# Steps
## Step 1
Create an ipynb file with cells containing texts you want to insert later (let's call it `texts.ipynb`):

Cells must start with `label: <key_name>` and from the second line contain the text to insert.

```markdown
label: text1
This is a text to insert
```Since we will usually have the same texts in R, Python and Quarto, we need an approach that will allow to keep those texts in sync.

Solution is putting placeholders in the .ipynb or .qmd files and then replacing them with the actual text stored in another .ipynb file.

# Requirements
Assumes python 3.10+

# Steps
## Step 1
Create an ipynb file with cells containing texts you want to insert later (let's call it `texts.ipynb`):

Cells must start with `label: <key_name>` and from the second line contain the text to insert.

```markdown
label: text1
This is a text to insert
```
Another cell:
```markdown
label: text
This is another text to insert
```
## Step 2
In the markdown cells of the `.ipynb` file (or anywhere in `.qmd`) put  `INSERT_text1` where you want to insert the text "Some text to insert". 
- You **must** add the `INSERT_` prefix.
- You can have regular markdown cells as well, no need to always insert.
- You can have multiple placeholders in one cell.
- You can combine regular markdown with placeholders.
- Code cells are not affected

## Step 3
Run
   ```
   python replace_texts.py <folder> --texts <name_of_texts_file>.ipynb
   ```
where `<folder>` is the path to the folder containing the `.ipynb` or `.qmd` files. If --texts is not specified, will default to `texts.ipynb`. The script will generate new files with the same names and put them in an “inserted” subfolder. 
- It will search for all `.ipynb` and `.qmd` files in the folder (but not its subfolders).
- It will replace all occurrences of `INSERT_text1`, `INSERT_text2`, etc. with the corresponding text from the ipynb file.
- The code will also check that all the placeholders have corresponding entries in the ipynb file. Also, if not all the keys in the ipynb file are used, a warning will be printed.
## Step 4
See the newly created files in the “inserted” subfolder. Initial files will not be changed.

# Full process
Input folder structure:
```
<folder_name>/
    ├── <chapter_name>.qmd
    ├── sol_<chapter_name>_py.ipynb
    ├── sol_<chapter_name>_r.qmd
    ├── sol_<chapter_name>_texts.ipynb (if there is only one exercise you can also just name the file `texts.ipynb`)
    ├── Makefile (containing the line `include ../quarto.mk`) (optional)
    └── _quarto (copied from the `exercises` folder, duplication is a temporary solution and will be removed in the future)
        ├── latex-math.qmd
        └── i2ml_theme.scss
```
running `python replace_texts.py <folder_name> --texts sol_<chapter_name>_texts.ipynb` from the `exercises` folder will result in the following output:
```
<folder_name>/
    ├── (all the original files, unchanged)
    └── inserted/ (new subfolder)
        ├── <chapter_name>.qmd
        ├── sol_<chapter_name>_py.ipynb
        └── sol_<chapter_name>_r.qmd
```
Afterwards, in the `inserted` folder one can run:
- `quarto render <chapter_name>.qmd --profile=solution` - generate the html file containing the solutions
- `quarto render <chapter_name>.qmd` - generate the html file containing only the exercises
- add `--to pdf` to the above commands to generate the pdf files
- add `--out <file_name.pdf>` to specify the output file name

To keep the naming consistent with the [guideline](https://github.com/slds-lmu/lecture_service/wiki/Exercises#structure-1) name the solution html files as `<chapter_name>_all.html` and the exercise html files as `<chapter_name>_ex.html`.


# Example
Please see the [`Advanced risk minimization`](https://github.com/slds-lmu/lecture_sl/tree/main/exercises/advriskmin-quarto)  folder for an actual example. Below is a toy example: 

Folder name: `replace_texts_example`
`texts.ipynb` file
```markdown
label: text1
This is a text to insert
```
Another cell:
```markdown
label: text2
This is another text to insert
```
---

Jupyter Notebook cell with solutions:
```markdown
Regular text here. INSERT_text1. Regular text here. INSERT_text2.
```
---
Running 
`python replace_texts.py replace_texts_example`

---
Resulting Jupyter Notebook cell in the `inserted` subfolder:
```markdown
Regular text here. This is a text to insert. Regular text here. This is another text to insert.
```

# Notes
1. If you want to ignore the `.qmd` files and only process `.ipynb` files, you can use the `--ignore_quarto` flag:
2. In case of chapters with multiple exercises, for the text files the code expects the structure `sol_<exercise_name>_texts.ipynb` (e.g. `sol_information_theory_1_texts.ipynb`). For exercise `k` you will need to run `python replace_texts.py <folder_name> --texts sol_<chapter_name>_k_texts.ipynb`.

# Overall workflow
## 1. Setup
1. Create a new branch `ex_<chapter_name>_quarto`
2. Create a new folder `<chapter_name>-quarto` in the `exercises` folder

## 2. R
1. For speeding up the process Github Copilot agent mode with Claude Sonnet 4 is quite helpful.
2. Copy R solutions (fastest way to find the solution is to just ctrl F the entire project for a segment of text (manually finding the file is slow))
3. Extend/improve them, add texts in between.
You can use the following prompt to get a draft:
*Prompt example:* "Please go over the code and split it into cells and add in between texts. Please also add labels to code cells via `#| label: some name` and for markdown cells just do `label: some name`"
4. Label code cells by adding `#| label: <key_name>` in the first line. And for markdown cells just do `label: <key_name>`.

## 3. Python
1. Copy the notebook and translate R to Python. Maybe use the following prompt to quickly get a draft:
*Prompt example:* "Please go over the R codes and translate them to Python. Try to not change the markdown cells, but if you need to change them, specifically state that. Keep the Python code understandable."

## 4. Text Sync
1.  Copy the notebook again, remove the code cells, remove all the text from markdown cells except for the first line (`label: <key_name>`)
2. `cd ..`; `python replace_texts.py <folder_name> --texts texts.ipynb --ignore_quarto`
3. Check the output in the `inserted` subfolder

## 5. Feedback
4. Add the solutions to Colab and share edit access links and wait for feedback
5. Incorporate feedback and create the Quarto file

## 6. Rendering
6.  `cd ..`; `python replace_texts.py <folder_name> --texts texts.ipynb`
8.  `cd <folder_name>/inserted`; `quarto render <chapter_name>.qmd --to html`

## 7. GitHub + Colab
9.  Create PR
10. Once merged, add jupyter notebooks to [Google Colab](https://colab.research.google.com/). File -> Open notebook -> GitHub -> search for "slds-lmu", then select the notebook you want to add, or alternatively paste the full link, for example https://github.com/slds-lmu/lecture_sl/blob/ex_gp_quarto/exercises/gaussian-processes-quarto/sol_gp_2_R.ipynb (this example is not from the `main` branch, this steps assumes that PR is merged, so the url will actually be from the `main` branch). The created notebook's URL will have the following structure "https://colab.research.google.com/github/slds-lmu/lecture_sl/blob/main/exercises/<exercise_chunk>/<file_name>.ipynb".
11. Check that the links work
12. Some tex macros may fail, may need to add e.g. `$$\newcommand{\bm}{\boldsymbol}$$`


Another cell:
```markdown
label: text2
This is another text to insert
```
## Step 2
In the markdown cells of the `.ipynb` file (or anywhere in `.qmd`) put  `INSERT_text1` where you want to insert the text "Some text to insert". 
- You **must** add the `INSERT_` prefix.
- You can have regular markdown cells as well, no need to always insert.
- You can have multiple placeholders in one cell.
- You can combine regular markdown with placeholders.
- Code cells are not affected

## Step 3
Run
   ```
   python replace_texts.py <folder> --texts <name_of_texts_file>.ipynb
   ```
where `<folder>` is the path to the folder containing the `.ipynb` or `.qmd` files. If --texts is not specified, will default to `texts.ipynb`. The script will generate new files with the same names and put them in an “inserted” subfolder. 
- It will search for all `.ipynb` and `.qmd` files in the folder (but not its subfolders).
- It will replace all occurrences of `INSERT_text1`, `INSERT_text2`, etc. with the corresponding text from the ipynb file.
- The code will also check that all the placeholders have corresponding entries in the ipynb file. Also, if not all the keys in the ipynb file are used, a warning will be printed.
## Step 4
See the newly created files in the “inserted” subfolder. Initial files will not be changed.

# Full process
Input folder structure:
```
<folder_name>/
    ├── <chapter_name>.qmd
    ├── sol_<chapter_name>_py.ipynb
    ├── sol_<chapter_name>_r.qmd
    ├── sol_<chapter_name>_texts.ipynb (if there is only one exercise you can also just name the file `texts.ipynb`)
    ├── Makefile (containing the line `include ../quarto.mk`) (optional)
    └── _quarto (copied from the `exercises` folder, duplication is a temporary solution and will be removed in the future)
        ├── latex-math.qmd
        └── i2ml_theme.scss
```
running `python replace_texts.py <folder_name> --texts sol_<chapter_name>_texts.ipynb` from the `exercises` folder will result in the following output:
```
<folder_name>/
    ├── (all the original files, unchanged)
    └── inserted/ (new subfolder)
        ├── <chapter_name>.qmd
        ├── sol_<chapter_name>_py.ipynb
        └── sol_<chapter_name>_r.qmd
```
Afterwards, in the `inserted` folder one can run:
- `quarto render <chapter_name>.qmd --profile=solution` - generate the html file containing the solutions
- `quarto render <chapter_name>.qmd` - generate the html file containing only the exercises
- add `--to pdf` to the above commands to generate the pdf files
- add `--out <file_name.pdf>` to specify the output file name

To keep the naming consistent with the [guideline](https://github.com/slds-lmu/lecture_service/wiki/Exercises#structure-1) name the solution html files as `<chapter_name>_all.html` and the exercise html files as `<chapter_name>_ex.html`.


# Example
Please see the [`Advanced risk minimization`](https://github.com/slds-lmu/lecture_sl/tree/main/exercises/advriskmin-quarto)  folder for an actual example. Below is a toy example: 

Folder name: `replace_texts_example`
`texts.ipynb` file
```markdown
label: text1
This is a text to insert
```
Another cell:
```markdown
label: text2
This is another text to insert
```
---

Jupyter Notebook cell with solutions:
```markdown
Regular text here. INSERT_text1. Regular text here. INSERT_text2.
```
---
Running 
`python replace_texts.py replace_texts_example`

---
Resulting Jupyter Notebook cell in the `inserted` subfolder:
```markdown
Regular text here. This is a text to insert. Regular text here. This is another text to insert.
```

# Notes
1. If you want to ignore the `.qmd` files and only process `.ipynb` files, you can use the `--ignore_quarto` flag:
2. In case of chapters with multiple exercises, for the text files the code expects the structure `sol_<exercise_name>_texts.ipynb` (e.g. `sol_information_theory_1_texts.ipynb`). For exercise `k` you will need to run `python replace_texts.py <folder_name> --texts sol_<chapter_name>_k_texts.ipynb`.

# Overall workflow
## 1. Setup
1. Create a new branch `ex_<chapter_name>_quarto`
2. Create a new folder `<chapter_name>-quarto` in the `exercises` folder

## 2. R
1. For speeding up the process Github Copilot agent mode with Claude Sonnet 4 is quite helpful.
2. Copy R solutions (fastest way to find the solution is to just ctrl F the entire project for a segment of text (manually finding the file is slow))
3. Extend/improve them, add texts in between.
You can use the following prompt to get a draft:
*Prompt example:* "Please go over the code and split it into cells and add in between texts. Please also add labels to code cells via `#| label: some name` and for markdown cells just do `label: some name`"
4. Label code cells by adding `#| label: <key_name>` in the first line. And for markdown cells just do `label: <key_name>`.

## 3. Python
1. Copy the notebook and translate R to Python. Maybe use the following prompt to quickly get a draft:
*Prompt example:* "Please go over the R codes and translate them to Python. Try to not change the markdown cells, but if you need to change them, specifically state that. Keep the Python code understandable."

## 4. Text Sync
1.  Copy the notebook again, remove the code cells, remove all the text from markdown cells except for the first line (`label: <key_name>`)
2. `cd ..`; `python replace_texts.py <folder_name> --texts texts.ipynb --ignore_quarto`
3. Check the output in the `inserted` subfolder

## 5. Feedback
4. Add the solutions to Colab and share edit access links and wait for feedback
5. Incorporate feedback and create the Quarto file

## 6. Rendering
6.  `cd ..`; `python replace_texts.py <folder_name> --texts texts.ipynb`
8.  `cd <folder_name>/inserted`; `quarto render <chapter_name>.qmd --to html`

## 7. GitHub + Colab
9.  Create PR
10. Once merged, add jupyter notebooks to Google Colab
11. Check that the links work
12. Some tex macros may fail, may need to add e.g. `$$\newcommand{\bm}{\boldsymbol}$$`