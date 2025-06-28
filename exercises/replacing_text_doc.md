Since we will usually have the same texts in R, Python and Quarto, we need an approach that will allow to keep those texts in sync.

Solution is putting placeholders in the .ipynb or .qmd files and then replacing them with the actual text stored in another .ipynb file.

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
    ├── texts.ipynb
    ├── Makefile (containing the line `include ../quarto.mk`) (optional)
    └── _quarto (copied from the `exercises` folder, duplication is a temporary solution and will be removed in the future)
        ├── latex-math.qmd
        └── i2ml_theme.scss
```
running `python replace_texts.py <folder_name> --texts texts.ipynb` from the `exercises` folder will result in the following output:
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
Please see the [`information-theory-quarto`](https://github.com/slds-lmu/lecture_sl/tree/ex_info_theory_quarto/exercises/information-theory-quarto) (internal comment: the link will break once we merge the branch) folder for an actual example. Below is a toy example: 

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