# check invalid DOIS in the Psychsci set

bib <- concat_tables(psychsci, "bib")

clean_dois <- doi_clean(bib$doi)
missing_dois <- is.na(clean_dois)
unclean_dois <- (clean_dois != bib$doi & !missing_dois)
valid_dois <- doi_valid_format(clean_dois)
invalid_dois <- !valid_dois & !missing_dois

doi_count <- list(
  all = nrow(bib),
  valid = sum(valid_dois),
  invalid = sum(invalid_dois),
  unclean = sum(unclean_dois),
  missing = sum(missing_dois)
)

# look up cleaned and valid DOIs ----
doi <- clean_dois[valid_dois]

# this code checks the first 1000 references ----
# subset <- 1:1000
# unresolved_dois <- !doi_resolves(doi[subset])
# doi[subset][unresolved_dois] |> dput()

# unresolvable DOIs from first 1000 refs checked ----
unres <- c("10.1037/0033-2909.115", # imported as written
           "10.1146/annurevclinpsy-032210-104544", # imported as written,
                                                   # line break but no - between annurev and clinpsy
           "10.1007/s10862-009-9171", # correct in PDF as doi:10.1073/pnas.1530467100,
                                      # but 2-col across is doi:10.1007/s10862-009-9171-z
           "10.1002/bdm", # imported as written
           "10.1111/j.1745-6924.2008.00076", # correct in PDF as 10.1111/j.1745-6924.2008.00076.x
           "10.1016/0042-6989(85)90207", # merged two references :
                                         # Levi, D. M., Klein, S. A., & Aitsebaomo, A. P. (1985). Vernier
                                         # acuity, crowding and cortical magnification. Vision
                                         # Research, 25, 963–977. doi:10.1016/0042-6989(85)90207-X
                                         #
                                         # Mon-Williams, M., Tresilian, J. R., Strang, N. C., Kochhar, P.,
                                         # & Wann, J. P. (1998). Improving vision: Neural com-
                                         #   pensation for optical defocus. Proceedings of the Royal
                                         # Society B: Biological Sciences, 265, 71–77. doi:10.1098/
                                         #   rspb.1998.0266
           "10.1523/JNEU-ROSCI.1097-11.2011", # imported as written
           "10.1017/thg.2012.91HESA", # correct in PDF, merged part of the next reference into the DOI
           "10.1037//0033-295X", # imported as written
           "10.1002/1097-4679", # correct in PDF as 10.1002/1097-4679(199511)51:6<768::AID-JCLP2270510607>3.0.CO;2-1
           "10.1111/j.1744-6570.1991.tb00688", # merged two references? :
                                               # Barrick, M. R., & Mount, M. K. (1991). The Big Five personality
                                               # dimensions and job performance: A meta-analysis. Personnel
                                               # Psychology, 44, 1–26. doi:10.1111/j.1744-6570.1991.tb00688
                                               # .x
                                               #
                                               # Barrick, M. R., Mount, M. K., & Li, N. (2013). The theory
                                               # of purposeful work behavior: The role of personality,
                                               # higher-order goals, and job characteristics. Academy
                                               # of Management Review, 38, 132–153. doi:10.5465/amr
                                               # .2010.0479
           "10.1016/S0885-2014", # correct in PDF, 2014(96)90027-1 is after line break
           "10.1016/1048-9843", # correct in PDF as 10.1016/1048-9843(91)90018-W (line break)
           "10.1016/S0140-6736", # correct in PDF as 10.1016/S0140-6736(86)90837-8
           "10.1017/S003329171200150", # correct in PDF as 10.1017/S003329171200150X
           "10.1093/ije/dyy022/4931207", # imported as written
           "10.1016/0191-8869", # correct in PDF as 10.1016/0191-8869(85)90026-1
           "10.1097/PSY.0" # correct in PDF as 10.1097/PSY.0b013e3181a2925b
)

which_unres <- sapply(unres, \(p) which(p == clean_dois[valid_dois][1:1000]))

# set up check table
df <- data.frame(
  paper_id = bib$id[valid_dois][which_unres],
  doi = unres
)
df$ref <- bib$ref[valid_dois][which_unres]
df$title <- sapply(df$ref, \(x) x$title)

# look up and add dois in crossref
cr <- crossref_query(df$ref)
df$cr_doi <- cr$DOI
df$cr_title <- cr$title

# they all seem to be badly parsed, manually review
i = 1
df$doi[[i]]
df$cr_doi[[i]]
df$ref[[i]]
paste0("data-raw/psychsci/pdf/", df$paper_id[[i]], ".pdf") |>
  browseURL()

# invalid ----

invalid <- data.frame(
  paper_id = bib$id[invalid_dois],
  doi = clean_dois[invalid_dois]
)
invalid$ref <- bib$ref[invalid_dois]
invalid$title <- sapply(invalid$ref, \(x) x$title)

# look up and add dois in crossref
cr <- crossref_query(invalid$ref)
invalid$cr_doi <- cr$DOI
invalid$cr_title <- cr$title

invalid$ref[[2]]
paste0("data-raw/psychsci/pdf/", invalid$paper_id[[2]], ".pdf") |>
  browseURL()

# 1: 10.1007/s00426-016-0769-y in PDF, merged with next ref
# 2: typo in PDF, repeated DOI twice


# check title/author mismatches ----


