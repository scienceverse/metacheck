# repro_paths

A tiny deposit for testing how `reproducibility_check` rewrites file paths in
the authors' scripts before it runs them (`.repro_rewrite_preview()`;
`test-repro-paths.R`; scienceverse/metacheck#648).

Each line that reads a file ends in a marker:

- `# KEEP <id>`: the line must stay exactly as written. Most are paths that
  already work, or pieces the code joins to a folder itself (`file.path()`,
  `paste0()`, `here()` with several parts, a `Path` with `/`); rewriting such
  a piece put the folder in twice.
- `# RESOLVES <id> <file>`: the path must lead to `<file>` (or folder) from the
  folder the script runs in: a bare name, a path on the authors' computer,
  Windows backslashes, a folder on the authors' computer held in a variable.
- `# NAMES <id> <name>`: the line names the file as the deposit spells it, and
  is otherwise as written: a piece with a name in another case.

`top.R` and `top.py` are run by their authors from the top of the project;
`analysis/from_analysis.R`, `analysis/from_analysis.py` and
`analysis/report.Rmd` from `analysis/`. `data/raw/not_deposited.csv` is
missing on purpose.
