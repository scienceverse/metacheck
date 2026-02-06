# Download all OSF Project Files

Creates a directory for the OSF ID and downloads all of the files using
a folder structure from the OSF project nodes and file storage
structure. Returns (invisibly) a data frame with file info.

## Usage

``` r
osf_file_download(
  osf_id,
  download_to = ".",
  max_file_size = 10,
  max_download_size = 100,
  max_folder_length = Inf,
  ignore_folder_structure = FALSE
)
```

## Arguments

- osf_id:

  an OSF ID or URL

- download_to:

  path to download to

- max_file_size:

  maximum file size to download (in MB) - set to NULL for no
  restrictions

- max_download_size:

  maximum total size to download

- max_folder_length:

  maximum folder name length (set to make sure paths are \<260 character
  on some Windows OS)

- ignore_folder_structure:

  if TRUE, download all files into a single folder

## Value

data frame of file info

## Details

Some differences may exist because the OSF allows longer file names with
characters that may not be allowed on a file system, so these are
cleaned up when downloading.

You can limit downloads to only files under a specific size (defaults to
10MB) and only a maximum download size (largest files will be omitted
until total size is under the limit). Omitted files will be listed as
messages in verbose mode, and included in the returned data frame with
the downloaded column value set to FALSE.

## Examples

``` r
# \donttest{
osf_file_download("6nt4v")
#> Starting retrieval for 6nt4v
#> - Created directory /Users/debruine/rproj/scienceverse/metacheck/docs/reference/6nt4v
# }
```
