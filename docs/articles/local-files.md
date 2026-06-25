# Checking Local Files with repo_check and code_check

``` r

library(metacheck)
```

## Overview

Metacheck has two modules that work together to check research code:

- **`repo_check`** lists all files in a repository and checks for basic
  organisational quality: is there a README? Are there unextracted zip
  files? How many code, data, and other files are there?

- **`code_check`** reads the actual code files and checks them for four
  best-practice issues: comments, absolute file paths, grouped library
  loading, and missing data files.

Normally both modules fetch files automatically from online repositories
(OSF, GitHub, ResearchBox, Zenodo) by following links found in a paper.
This vignette shows how to point them at a **local folder** instead —
useful when:

- you want to check your own code before sharing it
- you are reviewing a paper whose files were sent as a zip attachment
- the authors used a repository service that metacheck does not yet
  support automatically (GitLab, Figshare, Dataverse, and others)

------------------------------------------------------------------------

## Before you start

Load the package and note the path format for your operating system:

``` r

library(metacheck)
```

| Operating system | Example path                               |
|------------------|--------------------------------------------|
| Windows          | `"C:/Users/researcher/Documents/my_study"` |
| macOS / Linux    | `"/Users/researcher/Documents/my_study"`   |

Both forward slashes `/` and backslashes `\\` work on Windows inside R.

When you provide a local path, the folder is treated as a repository in
exactly the same way as an OSF project or GitHub repo. Each folder gets
its own row in the summary tables and its own traffic light.

------------------------------------------------------------------------

## Part 1: `repo_check` on local files

### What `repo_check` checks

For each folder you provide, `repo_check` will:

- **List all files** and classify them by type (code, data, archive,
  readme, …)
- **Check for a README** — a README file is good practice and helps
  others understand the contents of a folder
- **Flag zip archives** — if the folder contains `.zip` or other archive
  files, `repo_check` will flag them because their contents cannot be
  inspected
- **Report counts** — how many code files, data files, and so on

The traffic light is `"green"` when a README is present and there are no
unextracted archives, and `"yellow"` otherwise.

### Checking a single folder

``` r

repo <- module_run(test_paper(), "repo_check",
                   local_path = "C:/projects/reaction_time_study_2024")

# Plain-text summary
repo$summary_text

# Summary counts (one row per folder)
repo$summary_table

# "green" or "yellow"
repo$traffic_light

# Full file list — one row per file, with file_type and file_size columns
repo$table
```

[`test_paper()`](https://scienceverse.github.io/metacheck/reference/test_paper.md)
creates a minimal paper object with no repository links. The module
pipeline always requires a paper argument, but when you are only
checking a local folder there is no manuscript —
[`test_paper()`](https://scienceverse.github.io/metacheck/reference/test_paper.md)
gives you a valid object that causes the online repository search to
find nothing, so only the local files are checked.

> **Note on cloud-sync folders (OneDrive, iCloud, Dropbox):** If the
> folder is stored online and files have not been downloaded locally,
> the check will be slow because each code file triggers a download when
> it is read. Before running, right-click the folder in your file
> browser and choose **“Always keep on this device”** (OneDrive) or
> **“Download Now”** (iCloud) to make all files available locally first.

A typical `$summary_text` when the folder is missing a README might look
like:

    We found 8 files in 1 repository.
    We found 0 README files and 1 repository without READMEs.

And `$summary_table` will have columns: `repo_n`, `files_n`,
`files_code`, `files_data`, `files_readme`, `files_zip`.

### Checking multiple folders

Pass a vector of paths to check several folders at once. Each is treated
as a separate repository, so you get one summary row per folder:

``` r

paths <- c(
  "C:/projects/study_1_power_analysis",
  "C:/projects/study_2_replication",
  "C:/projects/study_3_meta_analysis"
)

repo <- module_run(test_paper(), "repo_check", local_path = paths)

# Three rows — one per folder
repo$summary_table

# All files from all three folders, with repo_url identifying the source
repo$table
```

This is a quick way to compare whether several projects all have READMEs
and no stray zip files before you submit or share them.

------------------------------------------------------------------------

## Part 2: `code_check` on local files

### What `code_check` checks

`code_check` builds on `repo_check`: it first runs the file inventory,
then reads every code file (R, Rmd, Qmd, SAS, SPSS, Stata) and checks
for:

- **Comments** — is the code explained? A file with no comment lines at
  all is flagged.
- **Absolute paths** — paths like `C:/myname/files/data.csv` only work
  on one specific computer and will break for anyone else trying to
  reproduce the analysis.
- **Library loading** — are all
  [`library()`](https://rdrr.io/r/base/library.html) /
  [`require()`](https://rdrr.io/r/base/library.html) calls grouped
  together near the top of the script, or are they scattered throughout?
- **Missing data files** — if the script loads `read.csv("data.csv")`
  but `data.csv` is not in the folder, that is flagged.

### Checking your own code

You have finished a project and want to review the quality of your
scripts before sharing:

``` r

result <- module_run(test_paper(), "code_check",
                     local_path = "C:/projects/reaction_time_study_2024")

# Short summary of all findings
result$summary_text

# One row per code file with detailed metrics
result$table

# "green" (no issues), "yellow" (some issues), or "na" (no code files found)
result$traffic_light

# Full formatted report
result$report
```

> **Note on cloud-sync folders:** If your files are on OneDrive, iCloud,
> or another cloud-sync service and are not fully downloaded, you will
> see the message *“If folders are stored online, the check might be
> slow as all files need to be downloaded.”* In that case, right-click
> the folder and choose “Always keep on this device” (OneDrive) or
> “Download Now” (iCloud) before running the check.

### Checking multiple projects

``` r

paths <- c(
  "C:/projects/study_1_power_analysis",
  "C:/projects/study_2_replication",
  "C:/projects/study_3_meta_analysis"
)

result <- module_run(test_paper(), "code_check", local_path = paths)

result$summary_text

# Filter to one folder using dplyr
library(dplyr)

result$table |>
  filter(grepl("study_2", repo_url)) |>
  select(file_name, percentage_comment, code_abs_path, loaded_files_missing)
```

The `repo_url` column contains the folder path, so you can always
identify which folder a code file came from. Each folder has its own
independent `file_limit` (default 20 files).

### Checking more than 20 code files

By default `code_check` analyses at most 20 code files per folder. This
prevents accidentally processing hundreds of files in, say, an R package
repository. If your folder has more code files than that and you want
them all checked, raise the limit or remove it entirely:

``` r

# analyse up to 100 code files per folder
result <- module_run(test_paper(), "code_check",
                     local_path = "C:/projects/large_project",
                     file_limit = 100)

# no limit — check every code file found
result <- module_run(test_paper(), "code_check",
                     local_path = "C:/projects/large_project",
                     file_limit = Inf)
```

The summary text will tell you if the limit was applied:
`"Only the first 20 files per repository were analysed."` If you see
this message and want complete results, re-run with a higher
`file_limit`.

### Peer review — files sent as a zip archive

When reviewing a paper, authors sometimes supply code and data as a zip
file attached to the submission. Once you unzip it, you have a local
folder:

``` r

# After unzipping the submission to a folder:
result <- module_run(test_paper(), "code_check",
                     local_path = "C:/peer_review/garcia_2024_submission")

result$report   # copy this into your review
```

### Code hosted on an unsupported repository

Metacheck currently retrieves files automatically from **OSF, GitHub,
ResearchBox, and Zenodo**. Many other services exist that are not yet
supported. If the paper links to one of these, download the files
manually and point `code_check` at the downloaded folder.

The workflow is the same for all of them:

1.  Go to the link in the paper
2.  Download the files (usually a “Download all” button, or clone with
    git)
3.  Unzip if needed
4.  Run `code_check`

``` r

# Author shared code on GitLab — you downloaded or cloned it locally
result <- module_run(test_paper(), "code_check",
                     local_path = "C:/downloads/lee_2023_gitlab_code")

result$summary_text
result$traffic_light
```

------------------------------------------------------------------------

## Part 3: Combining a paper with local files

Sometimes a paper links to an online repository that metacheck supports,
but there are also supplementary files that were shared separately — for
example, the main data is on OSF but additional analysis scripts were
sent as a zip by the editor. You can check both at once:

``` r

paper <- read("garcia_2024.pdf")

# repo_check fetches the OSF files AND lists the local folder
result <- module_run(paper, "code_check",
                 local_path = "C:/peer_review/garcia_2024_extra_scripts")

# Online files have a URL in repo_url; local files have the folder path
unique(result$table$repo_url)
```

`repo_check` reports on each repository separately, so you can see at a
glance whether the README and zip issues are in the online repo or the
local folder:

``` r

repo <- module_run(paper, "repo_check",
                   local_path = "C:/peer_review/garcia_2024_extra_scripts")

# One row per repository — the OSF project and the local folder
repo$summary_table
repo$traffic_light
```

------------------------------------------------------------------------

## Part 4: Running `repo_check` and `code_check` as a two-step pipeline

By default, `code_check` runs `repo_check` internally and you do not see
its output. If you want to inspect the file inventory before the code
analysis, or save the `repo_check` results separately, run them as two
steps. When you pass a `repo_check` result as the first argument to
`code_check`, the file list is reused — the folder is not re-read:

``` r

path <- "C:/projects/reaction_time_study_2024"

# Step 1: inventory and organisational checks
repo <- module_run(test_paper(), "repo_check", local_path = path)

repo$summary_text    # how many files, README present?
repo$traffic_light   # green / yellow
repo$table           # full file list with types and sizes

# Step 2: code quality analysis (reuses the file list from step 1)
code <- module_run(repo, "code_check")

code$summary_text    # code-specific summary
code$table           # one row per code file
code$traffic_light
```

This is especially useful when the folder is on a cloud-sync drive: any
slow file downloads happen once (during `code_check`), not twice.

------------------------------------------------------------------------

## Understanding the output

### `repo_check` output

| Element | What it contains |
|----|----|
| `$traffic_light` | `"green"` (README present, no zips), `"yellow"` (issues), or `"na"` (no repos) |
| `$summary_text` | Plain-text summary: file counts, README status, zip files |
| `$summary_table` | One row per folder: `repo_n`, `files_n`, `files_code`, `files_data`, `files_readme`, `files_zip` |
| `$table` | One row per file: `repo_url`, `file_name`, `file_url`, `file_location`, `file_size`, `file_type` |
| `$report` | Formatted report explaining any README or zip issues |

### `code_check` output

| Element | What it contains |
|----|----|
| `$traffic_light` | `"green"` (no issues), `"yellow"` (issues found), or `"na"` (no code files) |
| `$summary_text` | Plain-text summary of all four checks across all files |
| `$summary_table` | Counts: `code_n`, `code_abs_path`, `code_missing_files`, `code_min_comments` |
| `$table` | One row per code file with detailed metrics (see below) |
| `$report` | Full formatted report suitable for pasting into a review |

Key columns in `code_check`’s `$table`:

| Column | What it means |
|----|----|
| `file_name` | Script filename |
| `repo_url` | Which folder (or online repo) the file came from |
| `language` | Detected language: R, SAS, SPSS, or Stata |
| `percentage_comment` | Share of non-blank lines that are comments (0 = no comments at all) |
| `loaded_files_missing` | Number of data files the script tries to load that are not in the folder |
| `loaded_files_missing_names` | The names of those missing files |
| `code_abs_path` | Number of absolute file paths detected |
| `library_max_between` | Largest gap in lines between consecutive [`library()`](https://rdrr.io/r/base/library.html) calls |
