@AGENTS.md
@AGENTS.d/rules-slides.md
@AGENTS.d/rules-rcode.md
@AGENTS.d/rules-math.md

# lecture_sl — Supervised Learning

Repo-specific notes. The shared rules above come from lecture_service; this section is owned by this repo.

## Structure

- `slides/<chapter>/slides-<chapter>-<topic>.tex` — one slide set ("chunk") per file
- `slides/<chapter>/rsrc/*.R` — scripts that generate `figure/`; `figure_man/` holds manual figures
- `slides/<chapter>/chapter-order.tex`, `slides-order.txt` — chapter order (may be outdated, check against existing files)
- `exercises/<chapter>/` — Rnw exercises (`ex_*`, `sol_*`, `ic_*` for in-class sheets), build with `make` (see `exercises/chapter.mk`)
- `exercises/<chapter>-quarto/` — Quarto exercises (`make` via `exercises/quarto.mk`).
  The target format for Quarto sheets is defined in `lecture_i2ml/EXERCISE_SPEC.md`; SL sheets partly predate it.
  Ask before converting or restructuring a sheet.
- `slides-pdf/`, `exercises-pdf/` — compiled PDFs, committed to git
- `attic/` — parked material, do not touch unless asked

## Build commands

- Slides, in `slides/<chapter>/`: `make slides` (compile), `make release` (compile + literature + copy to `slides-pdf/`),
  `make texclean`, `make check-files-missing`, `make audit`. Run `make help` for all targets.
- R scripts, in `slides/<chapter>/rsrc/`: `make all` (runs all scripts via the `lese` R package), `make deps`.
- Repo root: `make slides` (all chapters), `make update-service`, `make update-latex-math`, `make help`.

