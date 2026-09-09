url <- "https://raw.githubusercontent.com/dyne/file-extension-list/refs/heads/master/pub/extensions.json"

jtypes <- jsonlite::read_json(url, simplifyVector = TRUE)
types <- dplyr::tibble(
  ext = names(jtypes),
  type = I(jtypes)
) |> tidyr::unnest_longer(type)

types$type[types$type == "sheet"] <- "data"

extensions <- list()
extensions[["code"]] <- data.frame(
  ext = c("R", "Rmd", "qmd", "quarto",
          "Rd", "Rds",
          "ado","dss",
          "Rnw", "jl", "ipynb",
          "ts", "dart", "sol", "vue", "svelte", "gradle")
)
extensions[["config"]] <- data.frame(
  ext = c("gitignore", "git", "yml", "yaml", "config", "Rproj",
          "make", "mk", "cmake", "lock", "env", "toml", "ini", "cfg",
          "bak", "tmp", "old", "orig", "swp")
)
extensions[["data"]] <- data.frame(
  ext = c("rds", "RData", "rda", "sav", "zsav", "por",
          "dta", "sas7bdat", "xpt", "sd7", "jasp",
          "omv", "arff", "sav.gz", "csv.sav", "gen",
          "gdt", "gdtb", "dft", "wf1",
          "dat", "json", "tsv", "csv",
          "parquet", "feather", "orc", "ndjson", "geojson",
          # MATLAB binary array/workspace file. Not source code (MATLAB
          # scripts are .m); it holds saved variables, so it belongs with the
          # other scientific data containers below.
          "mat",
          # Genomic sequence formats, gzip-compressed (the common convention
          # for these, since sequence files compress well). A bare
          # tools::file_ext() lookup (used by .ext_registry's data_type
          # lock) only ever sees "gz" for a compound name like
          # "sample.fasta.gz", which is itself classified "archive" -- these
          # compound extensions are matched directly against the whole
          # filename instead (see file_category()'s own per-extension regex
          # scan), so a real sequence file is not misread as a generic
          # archive. Confirmed against real corpus files that were falling
          # through to "unknown" (data_availability validation).
          "fasta.gz", "fa.gz", "fq.gz", "fastq.gz")
)
extensions[["archive"]] <- data.frame(
  ext = c("z", "lzop", "sz")
)
extensions[["audio"]] <- data.frame(
  ext = c("amr", "aif", "mka")
)
extensions[["image"]] <- data.frame(
  ext = c("ico", "raw", "nef", "cr2", "cr3", "arw", "psb", "jp2")
)
extensions[["video"]] <- data.frame(
  ext = c("hevc")
)
extensions[["text"]] <- data.frame(
  ext = c("odg", "odf", "rtx", "abw")
)
extensions[["font"]] <- data.frame(
  ext = c("pfb", "pfm")
)
extensions[["stats"]] <- data.frame(
  ext = c( "sas", "por", "jasp", "sps", "spss", "DO", "spv", "spo")
)

for (nm in names(extensions)) {
  extensions[[nm]]$type <- nm
}


file_types <- do.call(dplyr::bind_rows, c(list(types), extensions))
file_types$ext <- tolower(file_types$ext)
file_types <- unique(file_types) |>
  #dplyr::summarise(type = paste(type, collapse = ";"), .by = ext) |>
  dplyr::arrange(ext)

usethis::use_data(file_types, overwrite = TRUE, compress = "xz")
usethis::use_r("file_types")



