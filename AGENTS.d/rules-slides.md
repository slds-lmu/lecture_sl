
## Global Rules

- Slide chunks are modular and should represent about 15-20 minutes of material and be moderately self-contained.
- Don't put code on the slides if not specifically requested; code is left strictly for exercises/ practice sessions.

## File Structure

**Slides** are located at

```
lecture_XYZ/slides/<chapter>/<slide-name>.tex
```

where `<slide-name>` starts with `slide-` or `slide01-` and is also often referred to as a "chunk".

**Figures** are created either manually and live in

```
slides/<chapter>/figure_man/
```

or created with an R script in

```
slides/<chapter>/rsrc/
```

with the outputs located in

```
slides/<chapter>/figure/
slides/<chapter>/tables/
```

## Preamble & Title Slide

Each slideset starts with a preamble and a title slide, looking approximately like this:

```tex
\documentclass[11pt,compress,t,notes=noshow, xcolor=table]{beamer}

\input{../../style/preamble}
\input{../../latex-math/basic-math.tex}
\input{../../latex-math/basic-ml.tex}

\title{Introduction to Machine Learning}

\begin{document}

\titlemeta{Random Forest}{Bagging Ensembles}{
    figure_man/rf_majvot_averaging.png
}{
    \item The basic idea of bagging
    \item The second learning goals
}
```

- `latex-math` includes used **as needed**. Do not include more than required.
- The `\titlemeta` macro for the title slide inserts the title slide with 4 positional arguments:
    1.  The lecture chunk title, e.g. \"CART\"
    2.  The title for this slide deck
    3.  Left: A figure
    4.  Right: An itemized list of learning goals 

## Frames

- Use descriptive and concise titles, the title is part of the content, don't repeat if not really necessary,
  don't mindlessly copy-paste titles
- Nearly all slides, if possible, should be "flat itemize" lists; so sometimes you want like 2 separated "blocks" of
  item-lists, but items can be separated with `\vfill` and `\spacer`, you can even have "block titles" by using `\item[Foo Bar]`, this creates
  an entry without the bullet point, [see also the Overleaf docs](https://www.overleaf.com/learn/latex/Lists).
- Nearly all slides should use `framei`, maybe if needed `framev`, those are custom environments, see docs below
- For verbatim slides we unfortunately still need `\begin{frame}[fragile]`
- Don't use framebreak, create new frames
- Animations: avoid `only`, `pause`, etc. - most cases can be solved as follows:
  - `\foreach \i [count=\idx from 1] in {10, 20, 30}`: gives you counters `i` $\in \{10, 20, 30\}$ and `idx` $\in \{1, 2, 3\}$
  - Use like, e.g., `myfigure_\i_points.pdf`
  - For iteration-dependent text, use conditionals like `\ifnum \idx=1 ... \fi`

### Language and Text Formatting

- Use American english
- Use the Oxford comma
- **Prefer precision over flourish**: concise, concrete, unambiguous language beats fancy prose.
- Write SHORT, PRECISE sentences in a terse style, remove filler words as much as possible; 
  if the sentence is ROUGHLY grammatically correct, slight "fragments" are fine;  
  especially do this if this saves a nearly empty line
- You can abbreviate words (mainly technical terms) if an intelligent person can guess the abbreviation.
- Do not capitalize normal nouns or method names. "Bayesian" is capitalized, "random forest" is not. 
- Capitalize at the beginning of a bullet point
- Use punctuation "sparingly"; not a "." after every text fragment in a bullet list or so
- Use cspell to check against typos, and add needed words to .cspell/project-words.txt if reasonable


### Text Formatting
- Avoid font size changes unless really necessary.
- One sentence / thought per line (roughly), consider manual LBs after comma or semicolon; no "hanging" words
- Use "emphasized" markup in text sparingly, only for few words in slide and when really important;
  use `\textbf` then not `\emph`
- Quote text using backticks and apostrophes ``` ``text'' ``` syntax to produce "text", rather than `"text"`. Use of `\enquote{}` from `csquotes` is not required in an English locale.
- In VERY rare cases, to align text, you can use `phantom` (e.g., `\item hello world \item \phantom{hello} world` will align "world" vertically


### Spacing and Layout

- No overcomplicated whitespace control
- `\vfill` and `\spacer` should be used for vertical spacing, the former more often

## Custom Macros for Layout, Images, Citations

Below are some macros for easier creation of column layouts and images. Always use them. 
They make the tex code way shorter, more readable and maintainable. 
A usual slide is "framei", with some flat items, maybe a pic, maybe the pic is displayed in a separate column. 
These are precisely the cases the macros cover. 

Demo files to see the macros in action in [the demo lecture](https://github.com/slds-lmu/lecture_debug):

- the [Demo Lecture PDF](https://slds-lmu.github.io/lecture_debug/lecture_debug/slides/demo/slides-demo-summary.pdf) 
- with the [the accompanying source LaTeX](https://github.com/slds-lmu/lecture_debug/blob/main/slides/demo/slides-demo-summary.tex)


### `\splitV` column layout helpers

- The `\splitV` helpers should be used to replace any `\begin{columns}` and `\begin{minipage}` environment for splitting slide content into aligned columns, see [their definition here](https://github.com/slds-lmu/lecture_service/blob/main/service/style/splitV.sty) and a short description below.
- Create two columns that are either **B**ottom, **C**entered, or **T**op aligned
- The main variants for `{B, C, T}` exist:
  - `\splitVTT`: Both columns top-aligned
  - `\splitVCC`: Centered columns
  - `\splitVBB`: Bottom-aligned columns
- Further permutations may be added in the future if frequently needed
- `\splitVCC[0.3]{left}{right}` Makes left column take up 30%, right column 70% of the width.
- `\splitV{left content}{right content}`: Alias for `\splitVCC`

### Special cases for column layouts

- `\splitVCompact`: Like `\splitVTT`, but allows the specification of both column widths to add up to less than 100% of the available width, in which case only the requested space will be used. `\splitVTT` however will always use the full available width.
- `\twobytwo{A}{B}{C}{D}`: Creates a quadrant layout with equally-sized quadrants and vertical centering of content
- `\gridLayout[0.3]{A}{B}{C}{D}` like `\twobytwo` but the optional argument specifies the left column width

- `\splitVThree{A}{B}{C}` is like `\splitVCC` top-aligned but with 3 columns of equal width
- `\splitVThreeCustom[0.2]{0.3}{0.4}{A}{B}{C}` is like `\splitVThree` but all column widths are specified explicitly.

Within these, `\vfill` or `\spacer` should be used if needed to create vertical alignment.

### Image helpers (`\image{L, R, C}` and `\imageFixed`)

Image helpers wrap `\includegraphics` and use `\textwidth` as default width with scaling relative to that

- `\image{file.png}`: Full-width image --- does not do any positional alignment like centering for maximum flexibility & compatibility with other environments
- `\image[0.5]{file.png}`: Image is scaled to 50% of `\textwidth`
- `\image[0.5][CITEKEY / URL]{file.png}`: Inserts a `Source: \sourceref{CITEKEY / URL}` underneath (see `\sourceref` docs below)
- What is used is autodetected ("http://" part from the URL)
- If the src is provided, you need to set the scale arg as well, e.g., `\image[1][CITEKEY]{file.png}`.
- Using a URL for a source is most natural, you can use a CITEKEY from the bibtex file as "source" but do NOT add bibtex entries which are ONLY image-sources, the references.bib is strictly for a list of "further reading" material
- If `CITEKEY` is misspecified and not found in `references.bib`, there likely will _not_ be an informative error message!
- Variants for other horizontal alignments are: `\imageC`, `\imageL` and `\imageR`, with the same args for centered, left and right
- You nearly always want `\imageC`

#### `\imageFixed` for absolute positioning

For placing images at absolute coordinates (e.g., in animations to avoid "jumping"):

- `\imageFixed{x}{y}[width][CITEKEY/URL]{file.png}`: Places image at absolute position (x, y) from top-left corner
- Coordinates can be lengths like `2cm` or relative like `0.1\paperwidth`
- Width is optional (default 1.0 relative to `\textwidth`)
- Attribution works the same as other image macros
- Example: `\imageFixed{0cm}{4cm}[0.5]{figure/example.pdf}` places a half-width image at the top-left
- See section "Positioning in animations with fixed coordinates" below for more details

### `\itemize` helpers with spacing and font size control

A suite of helpers with control for font size and item separation.  
They are grouped by item separation, with font size being an optional argument:

- `itemizeS` for smaller item separation (spacing)
- `itemizeM` identical spacing to default `itemize`, but adds the font size control
  - The original `itemize` environment is untouched and equivalent to `itemizeM[normalsize]`
- `itemizeL` for larger item separation
- `itemizeF` automatically stretches across the available vertical width. Please note that there might be undesirable interactions with other environments, including but not limited to `\splitV` and derivatives.

- All of these support font size arguments, e.g. `\begin{itemizeL}[large]`
- Font sizes are standard LaTeX sizes like `small`, `footnotesize`, `large`, defaulting to `normalsize`


### Custom `frame` environments

#### `framev` environment

- `framev` is like `frame` but supports key-value options for font size and alignment
- This also affects nested environments (`itemize`, `splitV`)
- **Prefer the explicit key-value syntax** `[fs=small]` over the older `[small]` for clarity

```tex
\begin{framev}[fs=small]{Example with Small Font Size}
  This entire frame uses the small font size.

  \begin{itemize}
    \item All items in this list will also use the small font size
  \end{itemize}
\end{framev}
```

- Supported options:
  - `fs`: Font size (`small`, `tiny`, `footnotesize`, `large`, `normal`)
  - `align`: Vertical alignment (`top`/`t`, `center`/`c`, `bottom`/`b`)
- Example with alignment: `\begin{framev}[fs=small,align=top]{Title}`

#### `framei` environment

- `framei` environment wraps the custom itemize environments to allow frames consisting only of `itemize` with font size, spacing, and alignment control:
  - `fs` controls font size
  - `sep` controls separation (`S` for small, `M` for medium, `L` for large, `F` for fill)
  - `align` controls vertical alignment (`top`/`t`, `center`/`c`, `bottom`/`b`)

```latex
\begin{framei}[fs=small,sep=S]{framei Environment Example}
  \item foo
  \item bar
\end{framei}
```

- Example with alignment: `\begin{framei}[fs=small,sep=S,align=top]{Title}`
- All options are optional and can be combined as needed

### Citations (`\sourceref` and `\furtherreading`)

There are two macros for different contexts, both create different-looking clickable beamer-buttons:

- `\furtherreading{CITEKEY}`: Declare "this is a useful reference, look at it"
  - `CITEKEY` must be in `references.bib` and must have a `url` field for the button to link to
  - Use this for things that should go in the chapter-wise literature list (so no Wikipedia articles etc.)
- `\sourceref{CITEKEY}` and `\sourceref{<some url>}`: Declare "this is where we got the image / table / whatever from"
  - Use a `CITEKEY` if the source is in references.bib anyway (like an important book or paper)
  - Use a URL if the source is e.g. a Wikipedia page, some blog, something that should not go on the chapter-wise literature list

Keep in mind that:

- `CITEKEY` **must** be all-caps and a cite-key defined in an accompanying `slides/<chapter>/references.bib`
- Each `lecture_XYZ/slides/<chapter>` subfolder has it's own `references.bib`
- Note that `CITEKEY` is not directly verified, so if you mispecify it you will get an uninformative error message!
- In `references.bib`, only add most relevant information
- Do not add e.g. `note = {Accessed: 2024-11-17}`

- A **literature list** can be [compiled from this file](https://github.com/slds-lmu/lecture_service/wiki/Slides-Compilation#literature-list), the `Makefile` target `literature` handles this.

<details>
<summary>Technical note</summary>
The preamble is set up such that package `biblatex` is only loaded if a `references.bib` is detected to avoid spurious errors.   
Note that the `biber` command must be available in `$PATH` / by `latexmk` for this to work.  
This should be the case of a normal LaTeX installation, but debugging is hard.
</details>

## Figures

- When you include a graphic generated from code, ensure that
    - you add a comment specifying which script creates it
    - the file name is ideally descriptive / standardized such that this comment is not necessary (preferred!, See code style guide)
- Include them with the `\image` macros
- Figure paths **must** start *relative to the slide* to make sure they compile locally:
  - **DO**: `\image{figure_man/ridge-vs-sgd-path.png}`
  - **DO NOT**: `\image{slides/regularization/figure_man/ridge-vs-sgd-path.png}`
  - Watch out: Overleaf automatically tab-completes figure paths based on the project root :(

### Positioning in animations with fixed coordinates

- Example: Three consecutive slides show variations of the same image
- Goal: In each slide, the images are placed at the exact same position so slide transitions avoid "jumping" images
- **Recommended solution**: Use the `\imageFixed` macro (preferred for simplicity)
- **Alternative solution**: Using the [`textpos`](https://www.liverpool.ac.uk/~maryrees/posterproduction/textpos.pdf) package directly.
- For text *below* animations, use `\imageFixed` for the figure, and a `textblock*` environment for placing the text.

#### Using `\imageFixed` (recommended)

```tex
\begin{framei}{matérn kernel}
\item Foo
\item bar
\imageFixed{0cm}{4cm}[1]{figure/cov_funs/cov_matern.pdf}
\end{framei}

\begin{framei}{exponential kernel}
\item Foo
\item bar
\imageFixed{0cm}{4cm}[1]{figure/cov_funs/cov_exponential.pdf}
\end{framei}
```

- `\imageFixed{x}{y}[width]{file}` places the image at coordinates (x, y) from top-left corner
- Width is relative to `\textwidth` (default 1.0)
- Coordinates can use lengths (`2cm`) or relative units (`0.1\paperwidth`)
- Attribution is supported: `\imageFixed{0cm}{4cm}[1][CITEKEY]{file.pdf}`

#### Using `textpos` directly (alternative)

Example derived from `lecture_sl/slides/gaussian-processes/slides-gp-covariance.tex`:

```tex
\begin{framei}{matérn kernel}
\item Foo
\item bar
\begin{textblock*}{\linewidth}(0cm,4cm)
\image[1]{figure/cov_funs/cov_exponential.pdf}
\end{textblock*}
\end{framei}

\begin{framei}{exponential kernel}
\item Foo
\item bar
\begin{textblock*}{\linewidth}(0cm,4cm)
\image[1]{figure/cov_funs/cov_exponential.pdf}
\end{textblock*}
\end{framei}
```

- Image is placed at coordinates (0cm, 4cm) starting from top left corner
- `\image` is used to insert image (full width `[1]`) (centering with \imageC does not work!)
- Coordinates will need to be trial-and-errored and need to be consistent across frames obviously
  - Possibly useful: Coordinates `(0.1\linewidth,4cm)` would horizontally center image leaving 10% of the textwidth at either side

### Placing figures in for loops

- Images can be placed in consecutive frames using for loop over file names
- Example from `lecture_sl/slides/gaussian-processes/slides-gp-basic.tex`:

```tex
\foreach \i [count=\idx from 1] in {2, 5, 10} {
\begin{framei}{example: random discrete functions}
\item Example ctd: $\fv$ on $\i$ points
\item Sample representatives by sampling from a $\i$-dim Gaussian
\ifnum \i=2
$$\fv = [f(\xi[1]), f(\xi[2])]^T \sim \Nzk$$
\else 
$$\fv = [f(\xi[1]), \dots, f(\xi[\i])]^T \sim \Nzk$$
\fi
\item Where points are not (top) or strongly (bottom) correlated
\ifnum \i=2
\item RHS shows 2D density 
\else 
\item RHS shows correlation matrix / structure
\fi
```

- We iterate over `\i in {2, 5, 10}` and can use `\i` as variable in the loop
- Additional flexibility: We can use conditional logic like `\ifnum \i=2` to insert content only at certain steps in the loop
- Can be combined with previous `textblock` approach for precise figure positioning

(Credit to Lisa W.!)

## Tables

- When you include a table generated from code, ensure that
    - you add a comment specifying which script creates it
    - the file name is ideally descriptive / standardized such that this comment is not necessary (preferred!, See code style guide)
- Include them with the `\input` 
- Same comments for file paths as in figures above

### Google Figures

Google Figures are stored in the [G-Drive](https://drive.google.com/drive/folders/1JVlK94X7-h1DNaUo-gxOvIyVZph42iHj)

## Math
- Have latex formulas in src as readable as possible
- Use [latex-math](https://github.com/slds-lmu/lecture_service/wiki/latex-math) for common symbols
- If an expression is used often but not in latex-math yet, bring it up iun chat and offer to add
- Use `$$ ... $$` to denote display math, not `\[`
- The `equation` environment is equivalent to `\[ ... \]`, which we do not use in favor of the simple `$$ ... $$`.
- Do not use `eqnarray` and remove it where you see it. It has been deprecated for years and `align` or `$$ ... $$` is usually preferred.
- Do not use English orthography (`.`, `,`) in math formulas
- Avoid "manual whitespace control" in formulas as much as possible, keep them simply;
  don't use "," often, maybe some wellplaces q(q)uad sometimes
  
- Do not use `{ }` around single-character elements unless required; 
  to display $e^x$, use `e^x` rather than `e^{x}`
- Vector / Matrix transposition is denoted using `^T`
- For delimiters such as parentheses and vertical bars, the "simple" versions are preferred,
  so no `\left` and `\right` stuff, use this only when ABSOLUTELY necessary


## Verbatim content / Code

In **rare cases** where code has to be on a slide, use the following `frame` options:

```
\begin{frame}[fragile,c]{Verbatim content}

\begin{verbatim}
some code here
\end{verbatim}

\end{frame}
```

- You **can not** use `framei` or `framev` with `fragile` unfortunately.
- You are free to use any other custom environment or macro (`itemizeXY`, `\imageC`, ...) on a slide with verbatim content
