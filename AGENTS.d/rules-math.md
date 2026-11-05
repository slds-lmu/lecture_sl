# Rules for math in latex slides

## General
- Use the macros in the `latex-math` folder for common symbols
- If an expression is used often but not in latex-math yet, bring it up in chat and offer to add
- Have latex formulas in src as readable as possible

## Environments

- Use `$$ ... $$` to denote display math, not `\[`
- The `equation` environment is equivalent to `\[ ... \]`, which we do not use in favor of the simple `$$ ... $$`.
- Do not use `eqnarray` and remove it where you see it. It has been deprecated for years and `align` or `$$ ... $$` is usually preferred.
- Only use `align` environments if you truly need alignment, use `$$ ... $$` instead

## Formula cleanliness
- Avoid manual whitespace control in formulas (no `\,`, `\;`, `\:`, `\!`, `\quad`/`\qquad` everywhere) — keep formulas simple; a well-placed `\quad` is fine when really needed. Literal commas in tuples/lists (e.g. `(x_1, x_2)`) are of course fine.
- Do not use English orthography (`.`, `,`) as sentence punctuation inside math formulas
- Do not use `{ }` around single-character elements unless required;
  to display $e^x$, use `e^x` rather than `e^{x}`

## Conventions

- Vectors and vector-valued symbols are typeset bold via `\bm{...}` (not `\mathbf{...}`); #
  when a symbol denotes a vector, always render it bold — don't fall back to plain italic `x_0`
- Matrix transposition: use `^T`, never `^\top` or `^{\intercal}`
- For delimiters such as parentheses and vertical bars, prefer the simple versions; avoid `\left`/`\right` unless absolutely necessary
- Norms: write `||x||` in source (not `\|x\|`)
