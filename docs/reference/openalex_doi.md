# OpenAlex info from DOI

See details for a list of root-level fields that can be selected.

## Usage

``` r
openalex_doi(doi, select = NULL)
```

## Arguments

- doi:

  the DOI of the paper to get info for

- select:

  a vector of fields to return, NULL returns all

## Value

list with DOIs and info

## Details

See <https://docs.openalex.org/api-entities/works/work-object> for
explanations of the information you can retrieve about works.

Root-level fields for the select argument:

- id

- doi

- title

- display_name

- publication_year

- publication_date

- ids

- language

- primary_location

- type

- type_crossref

- indexed_in

- open_access

- authorships

- institution_assertions

- countries_distinct_count

- institutions_distinct_count

- corresponding_author_ids

- corresponding_institution_ids

- apc_list

- apc_paid

- fwci

- has_fulltext

- fulltext_origin

- cited_by_count

- citation_normalized_percentile

- cited_by_percentile_year

- biblio

- is_retracted

- is_paratext

- primary_topic

- topics

- keywords

- concepts

- mesh

- locations_count

- locations

- best_oa_location

- sustainable_development_goals

- grants

- datasets

- versions

- referenced_works_count

- referenced_works

- related_works

- abstract_inverted_index

- abstract_inverted_index_v3

- cited_by_api_url

- counts_by_year

- updated_date

- created_date

## Examples

``` r
doi <- "10.7717/peerj.4375"
# \donttest{
oa_info <- openalex_doi(doi)
oa_info <- openalex_doi(doi, "title")
# }
```
