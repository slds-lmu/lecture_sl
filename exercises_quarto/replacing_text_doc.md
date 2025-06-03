Since we will usually have the same texts in R, Python and Quarto, we need an approach that will allow to keep those texts in sync.

Solution is putting placeholders in the .ipynb or .qmd files and then replacing them with the actual text from a JSON file.

# Step 1
Create a JSON file with the texts you want to insert later (`texts.json`):
```json
{
    "text1": "Some text to insert",
    "text2": "Another text to insert",
}
```
# Step 2
In the markdown cells of the `.ipynb` file (or anywhere in `.qmd`) put  `INSERT_text1` where you want to insert the text "Some text to insert". 
- You **must** add the `INSERT_` prefix.
- You can have regular markdown cells as well, no need to always insert.
- You can have multiple placeholders in one cell.
- You can combine regular markdown with placeholders.
- Code cells are not affected

# Step 3
Run
   ```
   python replace_texts.py <folder>
   ```
where `<folder>` is the path to the folder containing the `.ipynb` or `.qmd` files. The script will. You can also add "--texts some_name.json" if you want to use a different JSON file than `texts.json`.
- It will search for all `.ipynb` and `.qmd` files in the folder (but not its subfolders).
- It will replace all occurrences of `INSERT_text1`, `INSERT_text2`, etc. with the corresponding text from the JSON file.
- The code will also check that all the placeholders have corresponding entries in the JSON file. Also, if not all the keys in the JSON file are used, a warning will be printed.
# Step 4
See the newly created files in the “inserted” subfolder. Initial files will not be changed.

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