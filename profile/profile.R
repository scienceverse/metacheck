library(lineprof)

devtools::load_all(".")
#source("profile/stats-custom.R")

files <- list.files("profile/psyarxiv", "xml$", full.names = TRUE)

lineprof( papers <- read_grobid(files[1:50]) )

lineprof( refs <- concat_tables(papers, "references") )
lineprof( cite <- concat_tables(papers, "citations") )

lineprof( res <- search_text(papers, section = "results") )

lineprof( sc <- stats(res) )

lineprof( p <- module_run(res, "all-p-values") )

