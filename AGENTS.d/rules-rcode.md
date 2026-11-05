# Rules R service scripts which produce plots and tables for slides

## General Rules
- Write clean, readable, good code, which can later be worked on by other people
- Connect to the notation of the lecture, don't come up with "new terms" – if not needed.

## Filenames and Paths

- Always assume the current working directory is the directory of the script (so `rsrc`)
- R files are always named according to the results they produce. Example: `rsrc/bernoulli-loss.R` generates `figure/bernoulli-loss.png`
- For animations, it is allowed to put "-1", "-2" at the end.
* No whitespaces, no special chars in filenames
- If you really need to generate multiple output files in one R file that CAN be ok (because you avoid code duplication), but the prefixes of the R file and results must coincide

## Code Style
* Always use `=` for assignment, never `<-`.
* 2-space indentation, 120-character line limit.
* `snake_case` for functions and variables, `CamelCase` for R6 classes.
* When calling a function from imported package `foo` do not write `foo::bar()` but `bar()`
* Double quotes for strings, explicit `TRUE`/`FALSE` (never `T`/`F`), explicit `1L` for integers.
* Use implicit return values for functions.
* Prefer `result = if (...) ... else ...` over `if (...) { result = ... } else { result = ... }`
  when the only difference between branches is the assigned value.

## Reproducibility

- Always seed the code in the beginning with `set.seed()`
- Load all libraries in the header of the file

## Documentation and Comments

- each script starts with a short doc header like this:
```
# Used in: <filepath(s)>
#
# <short general docs what happens now>
```
- Try to write self-documenting code, then you can use comments sparingly.
- comment "blocks of code" w.r.t. what happens there, the more complex, the more comments, if trivial, less comments

## Tables

- Store tables at the end of your script as a mini .tex file, in subfolder "<chunk>/tables"
- Use `kableExtra` to create the latex table
- If you produce numbers (in tables) think about significant/relevant digits.
  Usually, this might be "2-3 digits after the dot", not 10

## Plots

- For plots directly supported by vistool, use [vistool](https://github.com/slds-lmu/vistool)
  If you can produce the main parts of a plot with vistool, then simply have to add a few extra ggplot layers, do that.
- Use `ggplot2` as a default, nearly always
- For some plots, it might be ok to use "plots from packages" or "base R plots", but only do that if necessary
- Plots are saved at the end of a file, to `../figure/` (see rule for filenames above)
- Try to make plot and axis labels somewhat consistent and large enough.
  Look in other parts of the chunk, or even the chapter.
  Very often, we have things like "MSE" or so on the axis, use one form for this.
- Use the symbols for Greek letters, don't write `"theta"`.
- Prefer `expression(lambda[1])` / `bquote(...)` for axis labels when the
  plotting function supports expressions (ggplot2 does). Only when the function
  does NOT accept expressions (e.g. base R `persp()`) fall back to literal
  Unicode characters (`"λ₁"`, `"λ₂"`) directly in the source -- do NOT use
  `\uXXXX` escape sequences, they are unreadable in the code.


## ML Examples, Simulation Studies, Mini Benchmarks

- For ML examples, use `mlr3`.
- Especially for some form of "multiple model benchmark", "param tuning" or "param sweeps", etc.
- For some things, directly calling into the package of the learner can be simpler, but if code becomes longer and mlr3 shortens it, use `mlr3`

## Further service packages
- Use `data.table` for tabular manipulation




