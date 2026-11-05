# AGENTS.md — SLDS lecture repos

Shared rules for all SLDS lecture repositories (slides, exercises, R scripts).
Detailed rules live in `AGENTS.d/`:

- LaTeX slides: `AGENTS.d/rules-slides.md`
- R scripts in `slides/<chapter>/rsrc/`: `AGENTS.d/rules-rcode.md`
- Math notation: `AGENTS.d/rules-math.md`

Read the relevant file before working on that kind of content.

## Shared files — do not edit here

- `style/` is synced from `slds-lmu/lecture_service` (`make update-service`).
  Local edits are overwritten on the next sync. Propose style changes in lecture_service instead.
- `latex-math/` is synced from `slds-lmu/latex-math` (`make update-latex-math`).
  Use its macros for notation. If a frequently used expression is missing, say so and propose
  adding it upstream; do not define ad-hoc local macros.

## Working style

- Change only what the task asks for. Keep diffs small and reviewable.
- Do not regenerate or commit PDFs (`slides-pdf/`, `exercises-pdf/`) unless asked.
- After editing slides, compile the affected file (`make slides` in the chapter folder)
  and check the log for errors and overfull boxes.
- After editing an R script, run it and check that the expected figure in `figure/` is produced.
- Never stage files with `git add .`; add the files you changed explicitly.
- If something is unclear (content, notation, which exercise group), ask instead of guessing.
