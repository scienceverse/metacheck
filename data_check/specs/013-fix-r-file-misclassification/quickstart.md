# Quickstart: Fix R File Misclassification and CSV Read Errors

**Feature**: 013-fix-r-file-misclassification

## Changes Summary

Two files are modified — no new files, no new packages.

### `data_check/0_index.R`

1. **Add `AGGREGATE_EXT_OVERRIDE` constant** in the constants block (after `ARCHIVE_EXTS`):
   ```r
   AGGREGATE_EXT_OVERRIDE <- c(
     r = "code", rmd = "code", qmd = "code", py = "code", m = "code",
     do = "code", sps = "code", jl = "code", js = "code", sh = "code",
     bash = "code", pl = "code", rb = "code", cpp = "code", c = "code",
     h = "code", java = "code", scala = "code", sql = "code",
     jpg = "asset", jpeg = "asset", png = "asset", gif = "asset",
     bmp = "asset", tiff = "asset", tif = "asset", svg = "asset",
     mp4 = "asset", avi = "asset", mov = "asset", mp3 = "asset",
     wav = "asset", flac = "asset",
     csv = "data", sav = "data", dta = "data", sas7bdat = "data",
     xlsx = "data", xls = "data", rds = "data"
   )
   ```

2. **Apply extension override after sentinel expansion** (end of Step 7, `agg_expanded_df` construction):
   ```r
   # Apply extension-based type correction for files expanded from aggregates
   agg_ext <- tolower(tools::file_ext(agg_expanded_df$rel_path))
   override <- AGGREGATE_EXT_OVERRIDE[agg_ext]
   to_override <- !is.na(override)
   agg_expanded_df$type[to_override] <- override[to_override]
   ```

### `data_check/helper.R`

3. **Fix `sniff_delimiter()` — guard against empty file**:
   ```r
   sniff_delimiter <- function(path) {
     line <- character(0)                          # was: line <- ""
     con  <- file(path, "r")
     on.exit(close(con))
     for (i in seq_len(10)) {
       line <- readLines(con, n = 1, warn = FALSE)
       if (length(line) == 0) break               # NEW: EOF guard
       if (nchar(trimws(line)) > 0) break
     }
     if (length(line) == 0) return(",")           # NEW: empty file default
     candidates <- c(",", ";", "\t", "|")
     counts     <- vapply(candidates, function(d)
       nchar(line) - nchar(gsub(d, "", line, fixed = TRUE)), integer(1))
     if (max(counts) == 0) "," else candidates[which.max(counts)]
   }
   ```

4. **Suppress "incomplete final line" warning in `read_data_head()`**:
   ```r
   csv  = ,
   txt  = ,
   tsv  = ,
   dat  = {
     sep <- if (ext == "tsv") "\t" else sniff_delimiter(path)
     suppressWarnings(                              # NEW wrapper
       read.delim(path, sep = sep, nrows = n_rows, check.names = FALSE,
                  stringsAsFactors = FALSE)
     )
   },
   ```

## Verifying the fix

Run paper `09567976211040491` (already downloaded):
```r
source("./data_check/0_index.R")
run_index("09567976211040491", download = FALSE)
```

Expected after fix:
- File inventory shows `code / other` and `asset / na` entries (not just `data / other`)
- Total data files sent to column extraction is well below 378
- No "argument is of length zero" warning for `PickupsBehavProf.csv`
- No "incomplete final line" warning in output
- Run completes without `^C` interrupt
