# Metacheck Paper Schema

<https://www.scienceverse.org/schema/paper.json>

## Big picture questions

What text should be in the text table so it's easily searchable by search_text() and should there be ways to include/exclude some of that by default or with arguments?

The text categories are: body text, section headers, tables (captions), figures (captions), footnotes,  references, and "backmatter" (like page numbers and citation info in the header, the paper title, author lists and info). Also boxes?

## Section table

For reference:

| section_id |            header | parent_section_id | section_type | classification_score |
|------------|-------------------|-------------------|--------------|----------------------|
|          1 |             Title |    0              |      unknown |                0.00 |
|          2 |          Abstract |    1              |     abstract |                1.00 |
|          3 |                   |    1              |        intro |                  NA |
|          4 |            Method |    1              |       method |                1.00 |
|          5 |         Procedure |    4              |       method |                0.85 |
|          6 | Data Availability |    4              |      endnote |                1.00 |

## Xrefs table

For reference:

| xref_id | xref_type | contents       | text_id |
|---------|-----------|----------------|---------|
|       1 | tbl       | Table 1        |      19 |
|       1 | fig       | Figure 1       |      19 |
|       2 | fig       | Figure 2       |      22 |
|       1 | bib       | DeBruine, 2025 |       7 |
|       1 | bib       | DeBruine, 2025 |      11 |

The bib entries arent cross-referenced yet, but the figs and tables are.

## Text Table

Current format:

| text_id| section_id | paragraph_id |  text                     | page_number |
|--------|----------|------------|-----------------------------------|----------|
|      1 |        1 |         1  | "Daniel Lakens Lisa DeBruine Jakub W…   |  1 |
|      2 |        1 |         2  | "2026-02-22"                            |  1 |
|      3 |        2 |         3  | "This paper demonstrates some good a…   |  1 |
|      4 |        2 |         3  | "All data are simulated."               |  1 |
|      5 |        2 |         3  | "The paper shows examples of (1) ope…   |  1 |
|      6 |        3 |         4  | "Although intentional dishonesty mig…   |  1 |

Should the text table include:

- figure and table captions (currently in the figures and tables tables?)
- text from the reference section
- text of footnotes (currently in their own footnotes table)
- other text classified into data (e.g., title, authors)
- section headers (currently only in the new sections table)

The text table is what is searched by search_text(). So it might be confusing to include reference text in it. We can search those table with search_text like: `paper$sections$header |> search_text("text")` but it's awkward.

Or include a flag in search_text to by default only search the body text, but you can specify to include headers, fig-caps, tbl-caps, bib, footnotes, etc. This requires the text be labelled in either the section table (all text must have a section number then) or in the text table.

Including fig/tbl/foot gets a little tricky when trying to figure out the paragraph/section number, especially in formatted PDFs where, e.g., the text is 2-columns and the figure spans both columns and is just at the top or bottom of the page, not clearly at a specific point in the text flow. The could just get no paragraph_id?

Including headers is tricky because do they get their own paragraph numbers? Or NA? They used to be p = 0 for each div, but now paragraph_id is continuously numbered.

## Links

The links table currently contains links from the reference section, with a text_id that doesn't exist in the text table because references are omitted. Whether to include links (doi and/or non-doi) from the bib is probably tied to the decision whether to include them in the text table.

## References

Should the exact text go in the text table instead of `bib$`bib_text` and be replaced by `bib$text_id`?

## Figures/Tables

They can potentially have titles/headers and multi-sentence or multi-paragraph captions. 

If all text is in the text table, should each fig/tbl get its own section_id (always after the body sections, since placement is determined by xrefs?), with section_type = "table" or "figure"? And figure/table tables still exist to number and reference figs/tables for xrefs.

## Footnotes

footnote_id, text, section_id, text_id

| footnote_id | text               | section_id | text_id |
|-------------|--------------------|------------|---------|
|           1 |Text of footnote... |          7 |      24 |

(Section ID should be omitted, as it can be looked up from the text_id if necessary.)

Could be included in xrefs table instead as:

| xref_id | xref_type | contents            | text_id |
|---------|-----------|---------------------|---------|
|       1 | footnote  | Text of footnote... |      24 |

Although "contents" is technically the "(1)" marking the footnote. 

Footnotes are different from tbl, fig and bib xrefs because they can link to multiple sentences (or none), while there is a 1-to-1 relationship between footnotes and text (I think).
