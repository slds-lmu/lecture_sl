Since we will usually have the same texts in R, Python and Quarto, we need an approach that will allow to keep those texts in sync.

Solution is putting placeholders in the .ipynb or .qmd files and then replacing them with the actual text from a JSON file.

# Steps
## Step 1
Create a JSON file with the texts you want to insert later (`texts.json`):
```json
{
    "text1": "Some text to insert",
    "text2": "Another text to insert",
}
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
   python replace_texts.py <folder>
   ```
where `<folder>` is the path to the folder containing the `.ipynb` or `.qmd` files. The script will generate new files with the same names and put them in an “inserted” subfolder. You can also add "--texts some_name.json" if you want to use a different JSON file than `texts.json`.
- It will search for all `.ipynb` and `.qmd` files in the folder (but not its subfolders).
- It will replace all occurrences of `INSERT_text1`, `INSERT_text2`, etc. with the corresponding text from the JSON file.
- The code will also check that all the placeholders have corresponding entries in the JSON file. Also, if not all the keys in the JSON file are used, a warning will be printed.
## Step 4
See the newly created files in the “inserted” subfolder. Initial files will not be changed.

# Full process
Input folder structure:
```
<folder_name>/
    ├── <chapter_name>.qmd
    ├── sol_<chapter_name>_py.ipynb
    ├── sol_<chapter_name>_r.qmd
    ├── texts.json
    ├── Makefile (containing the line `include ../quarto.mk`) (optional)
    └── _quarto (copied from the `exercises` folder, duplication is a temporary solution and will be removed in the future)
        ├── latex-math.qmd
        └── i2ml_theme.scss
```
running `python replace_texts.py <folder_name>` from the `exercises` folder will result in the following output:
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
Please see the `advriskmin_3` folder for an actual example. Below is a toy example:

Folder name: `replace_texts_example`
`texts.json` file
```json
{
    "text1": "This is a text to insert",
    "text2": "This is another text to insert"
}
```

Jupyter Notebook cell:
```markdown
Regular text here. INSERT_text1. Regular text here. INSERT_text2.
```

Running 
`python replace_texts.py replace_texts_example`

Resulting Jupyter Notebook cell in the `inserted` subfolder:
```markdown
Regular text here. This is a text to insert. Regular text here. This is another text to insert.
```

# Notes
1. When creating the JSON file, make sure to handle line breaks with `\n`. Also, don't forget to make "\"-s coming from LaTeX escaped changing them to `\\`, e.g. `\\textbf{bold text}`.