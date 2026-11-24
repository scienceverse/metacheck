# Where did a paper's data come from? (#695, point 18)
#
# ethics_check asks for an ethics statement when the authors collected data
# from people or animals themselves (now or in an earlier study of their own),
# or analyse records about individual people that they obtained for this
# study. It does not ask for one when the paper has no data about people
# (a review, simulation, commentary, theory, tutorial, coding of published
# papers, or figures about groups), or analyses an open resource that any
# researcher can use (UK Biobank, NHANES, a dataset another team shared).
# .data_source() sorts each paper into one of these by weighing signs in
# its text, and in the results of other modules when they ran first.
#
# How the rule was made: 954 PsyArXiv and medRxiv preprints were labelled
# from extracts by language-model readers following these definitions, and
# the existing-data papers were labelled again as open resource or obtained
# records. The rule was written on 755 of them in rounds, each round's change
# stated as a principle first and then tested on papers it had not seen; the
# last 199 were labelled after the rule was fixed and used once. On those 199
# the rule decided right whether a statement is needed for 86.9% (the earlier
# live-data search: 83.4%); on the 150 Meta-Psychology papers with hand labels
# of new data from people, wrong red results went from 34 to 8.
#
# What the signs mean:
#
# - own collection: consent given, people recruited, paid or sent through a
#   recruitment platform, a sample description (mean age, % women), what the
#   participants did, interviews or focus groups, animals housed, and a
#   "Participants" or "Procedure" heading. Each counts only in a sentence that
#   is not about someone else's study (a citation, the studies a review
#   included, a named cohort or survey without "we" or "our") and not in the
#   future tense (a protocol, counted separately). "Data were collected", a
#   power analysis and participant columns in the authors' own deposit name no
#   people, and only add to these.
# - paper type, named in the title or abstract: review or meta-analysis,
#   tutorial or software, simulation study, commentary. Such a paper needs
#   more own-collection signs before it counts as collecting data: a review
#   at least three, one of them in the first person; the others at least two,
#   one of them consent, recruitment or payment. The same words in the body
#   only (a search strategy, "1,000 simulations", a theorem, coded articles)
#   decide only when the other evidence is thin.
# - existing data: secondary analysis, reanalysis, data obtained from, a
#   registry, health records, retrospective study, a named study, survey or
#   cohort ("the HUNT study"), the authors' earlier study ("data from our
#   previous study"), or a statement that nobody was recruited.
# - open resource: a well-known open resource named in the title, the
#   abstract or a part about the data or method, or three times anywhere
#   (.ds_open_resources); or "publicly available", "public-use", "freely
#   available" there.
# - obtained records: hospital or clinic records, "this study was approved",
#   a waiver of consent, data that cannot be shared.
# - no data about people: figures about groups or things ("country-level",
#   "aggregate data", surveillance data, language models tested) without
#   records of individuals, or the paper's statement that it has no data.
#
# The rule answers "uncertain" when the signs do not decide, and "no text"
# for a paper without text. (The evaluation left out 185 preprints whose
# report page held one line of at most 80 characters instead of the text.)

# months and seasons followed by a year are dates, not citations
.ds_months <- "January|February|March|April|May|June|July|August|September|October|November|December|Spring|Summer|Autumn|Fall|Winter"

# the signs that the authors collected data from people or animals; each but
# `generic` names people or animals
.ds_kinds <- list(
  consent = r"---(\b(gave|give|provided|obtained|signed)\s+(their\s+)?(written|verbal|oral|electronic|online|digital)?\s*(and\s+\w+\s+)?(informed\s+)?(consent|assent)|consent\s+(was|were)\s+obtained)---",
  recruit = r"---(\b(were|was|been)\s+recruited\b|\brecruited\s+(via|through|from|using|on)\b|\brecruitment\s+(took place|was|occurred)|\bwe\s+(recruited|enrolled)\b)---",
  pay = paste0(r"---(\bprolific\b|\bm-?turk\b|mechanical turk|\bsona\b|cloudresearch|course credits?|\b(were|was)\s+(paid|compensated|reimbursed)\b|\bmonetary\s+(compensation|reward)|gift\s+(cards?|vouchers?)|\bhonorarium\b|received\s+[$)---",
               "\u00a3\u20ac", r"---(]\s?\d)---"),
  sample = r"---(\bM\s?age\b|\bmean\s+age\b|\bage\s+range\b|\baged\s+(between\s+)?\d|\b\d+(\.\d+)?\s?%\s+(were\s+)?(female|women|male|men)\b|\(\s?\d+\s+(female|women|males?|men)\b|\b\d+\s+(women|men|females|males)\s+(and|,))---",
  procedure = r"---(\bparticipants\s+(completed|were\s+(asked|instructed|presented|shown|randomly|randomized|told|seated|tested))|\bwere\s+randomly\s+assigned)---",
  generic = r"---(\b(data|responses)\s+(were|was)\s+collected\b|\bwe\s+(collected|tested|measured)\b|\bdata\s+collection\s+(took\s+place|was\s+(conducted|carried|completed)|began|started|lasted)|\bwere\s+excluded\s+(from|because|due))---",
  interview = r"---(semi-?structured\s+interviews?|focus\s+groups?\s+(were|was)|\binterviews\s+(were|lasted)|\bwe\s+(surveyed|interviewed)\b)---",
  animal = r"---(\b(mice|rats|monkeys|macaques|zebrafish|animals|pigs|dogs)\s+were\s+(housed|kept|maintained|trained|tested|bred|purchased|obtained)|\banimal\s+care\b|\bhoused\s+(in|under|individually))---"
)
# the kinds that name people or animals, as the rule counts them
.ds_people_kinds <- c("consent", "recruit", "pay", "sample", "procedure", "interview", "animal")

.ds_future <- r"---(\bwill\s+be\s+(recruited|collected|enrolled|invited|asked|randomi[sz]ed|conducted|obtained)|\bwe\s+will\s+(recruit|collect|enrol|invite|conduct|obtain)|\bparticipants\s+will\b|\bplanned\s+sample\s+size)---"
.ds_first <- r"---(\b(we|our)\b)---"
# a sentence about other people's study (matched with case): an author and
# year, "et al.", a numbered reference, the studies a review included, a
# meta-analysis, or the original study of a secondary analysis
.ds_others <- paste0(
  r"---(\b(?!(?:)---", .ds_months, r"---()\b)[A-Z][a-z]+(\s+et\s+al\.?|\s+(and|&)\s+[A-Z][a-z]+)?(,\s*|\s*\()(19|20)\d{2}[a-z]?\s*[),;])---",
  r"---(|\bet\s+al\.?[,\s]|\[\d+(\s*[,)---", "\u2013", r"---(-]\s*\d+)*\])---",
  r"---(|\b(?i:included|eligible|selected|primary|original|individual|reviewed|pooled)\s+(\w+\s+){0,2}(?i:studies|trials|articles|papers|reports|publications|samples|cohorts)\b)---",
  r"---(|\b(?i:studies|trials|articles|papers|reports|publications|cohorts)\s+(\w+\s+){0,6}(?i:enrolled|included|comprising|comprised|involving|recruited|reported)\b)---",
  r"---(|(?i:\bmeta-?analy)|(?i:\b(in|of|by|from)\s+the\s+(original|parent|primary|main)\s+(study|trial|survey|cohort|authors)))---"
)
# figures about groups or things, not about individual people
.ds_aggregate <- r"---(\baggregated?\s+(data|statistics|counts|rates|estimates|level|figures)|\b(country|national|state|county|regional|population|website|tract|area|district)-level\b|\bno\s+individual-level|\bsurveillance\s+data|\bmortality\s+(database|statistics|data)\b|global\s+burden\s+of\s+disease|\bgoogle\s+trends\b|\blanguage\s+models?\s+(were|was)\s+(tested|evaluated|prompted|queried))---"
# the paper says itself that it has no data from people
.ds_no_data <- r"---(\bno\s+(new\s+|primary\s+|original\s+)?(data|datasets?)\s+(were|was)\s+(collected|generated|created|gathered|analy[sz]ed)|\bno\s+(human\s+)?(participants|subjects)\s+(were|was)\s+(recruited|involved|tested)|\b(did|does)\s+not\s+involve\s+(any\s+)?(human|participants|new\s+data)|\bthis\s+(paper|article|manuscript)\s+is\s+(a\s+)?(purely\s+)?(theor|conceptual)|\bdata\s+sharing\s+(is\s+)?not\s+applicable)---"
# the authors' own earlier data
.ds_earlier <- r"---(\bdata\s+(were|was)\s+(collected|drawn|taken)\s+(as\s+part\s+of|from)\s+(a|an|the|our)\s+(larger|ongoing|broader|previous|earlier|prior|parent)\b|\bpreviously\s+(published|reported|described|collected)\s+(data|dataset|sample|cohort)|\bas\s+part\s+of\s+(a|an|the)\s+(larger|ongoing|broader)\s+(study|project|trial)|\bbaseline\s+data\s+(from|of)\b|\bsecondary\s+analysis\s+of\s+(our|a|the)\s+(\w+\s+){0,3}(trial|study)|\bdata\s+from\s+(our|a)\s+(previous|earlier|prior)\s+(study|trial))---"
# existing data about people: wording that names the source of the data
.ds_existing <- r"---(secondary\s+(data\s+)?analys|\bre-?analy[sz]|publicly\s+available\s+(data|dataset)|\bopen\s+data\s?set|archival\s+data|existing\s+(data|dataset)|\bdata\s+(were|was)\s+(obtained|drawn|taken|retrieved|downloaded|extracted|sourced)\s+from|\bwe\s+(used|analy[sz]ed|leveraged)\s+data\s+from|\b(data|participants|patients|records|samples)\s+from\s+the\s+[^.;]{0,60}(registry|biobank|cohort|survey|database|study)\b|\b(using|from)\s+(electronic\s+)?(health|medical)\s+records|\bretrospective(ly)?\s+(cohort|study|analysis|review|chart|observational|identified|collected|reviewed)|\bchart\s+review)---"
# a named dataset (matched with case): a proper name ending in Study, Survey,
# Cohort, Biobank, Registry, Initiative or Consortium, or an abbreviation of
# four or more capitals followed by study, cohort, participants or data
.ds_named_stop <- r"---((?:In|As|For|A|An|The|This|That|Our|Each|Both|Simulation|Pilot|Present|Current|Qualitative|Quantitative|Longitudinal|Cross-Sectional|Cross-sectional|Observational|Prospective|Retrospective|Experimental|Main|First|Second|Third|Replication|Validation|Online|SoSci|Case|Cohort|Feasibility|Original|Previous|Future|Follow-up|Mixed-Methods|Preregistered|Registered|Empirical|Field|Lab|Laboratory|Supplementary|Study|Experiment|Survey|Sample|Discovery|Replication|Exploratory|Confirmatory|National|Web|Internet|Delphi)\b)---"
.ds_named <- paste0(
  r"---(\b(?!)---", .ds_named_stop, r"---()[A-Z][A-Za-z/&-]*(?:\s+(?:of|and|for|the|in|on)|\s+(?!)---", .ds_named_stop,
  r"---()[A-Z0-9][A-Za-z0-9/&-]*){0,7}\s+(?:Study|Survey|Cohort|Biobank|Registry|Initiative|Consortium)\b)---",
  r"---(|\b(?!(?:COVID|ADHD|PTSD|SARS|AIDS|STEM|LGBT\w*|ANOVA|GWAS|WEIRD|BOLD|NIRS|RNA\w*|MMSE|OSF|APA)\b)[A-Z]{4,}[A-Za-z0-9/-]*\s+(?:study|cohort|release|wave|participants|data|dataset)\b)---"
)
# a heading about the data or the method
.ds_head_source <- r"---(method|data|sample|participant|cohort|population|design|setting|subjects)---"
# an open resource
.ds_public <- r"---(publicly\s+(available|accessible)|public(ly)?[- ]use\b|open[- ]access\s+(data|dataset|database|resource)|(freely|openly)\s+(available|accessible)|available\s+(to|for)\s+(all\s+)?(bona\s+fide\s+|qualified\s+|approved\s+|registered\s+)?researchers|(downloaded|obtained|accessed|retrieved)\s+from\s+(https?://|the\s+[\w-]+\s+(website|repository|portal))|\bno\s+(additional\s+|further\s+|separate\s+)?(ethic\w*|irb|institutional)\s+(approval|review)\s+(was|is)\s+(required|needed|necessary)|\bexempt(ed)?\s+from\b|\bopen\s+(data|dataset|resource)\b)---"
# records obtained for this study
.ds_records <- r"---((cannot|can\s+not|not)\s+be\s+(shared|made\s+(public|available))|not\s+publicly\s+available|due\s+to\s+(privacy|legal|ethical|confidentiality|data\s+protection)\s+(restrictions|reasons|regulations|concerns)|(hospital|medical\s+cent(er|re)|clinic|health\s+system|health\s+service)\S*\s+(records|database|data)\b|\b(this|the\s+present|the\s+current)\s+study\s+was\s+approved|\bwaiver\s+of\s+(informed\s+)?consent|consent\s+was\s+waived)---"
# records of individual people
.ds_individual <- r"---((?<!no\s)\b(individual|patient|participant|person)-level\s+(data|records)|\b(patients|participants|individuals|adults|children|women|men|respondents)\s+(were|was)\s+(included|identified|eligible|excluded|followed|enrolled)|\bcohort\s+of\s+[\d,]+\s+\w+|\b(health|medical|hospital|clinical)\s+records\s+of\b|\blinked\s+(records|data))---"
# the paper says it recruited or contacted nobody
.ds_no_recruit <- r"---(\bno\s+(individuals?|patients?|participants?|subjects?|persons?|new\s+\w+)\s+(were|was)\s+(recruited|contacted|enrolled|approached))---"
# a named cohort, biobank or data release as the one that recruited
.ds_cohort <- r"---(\b(cohort|biobank|registry|consortium|panel|release\s+\d|longitudinal\s+study|(?-i:[A-Z]{3,})(\s+study)?)\b)---"
# the paper type, named in the title or abstract
.ds_type <- list(
  review = r"---(systematic\s+review|scoping\s+review|meta-?analy|umbrella\s+review|literature\s+review|narrative\s+review|review\s+of\s+the\s+literature|\bprisma\b)---",
  commentary = r"---(\bin\s+this\s+(commentary|essay|perspective|opinion|viewpoint|piece|comment)\b|\b(we|I)\s+argue\b)---",
  simulation = r"---(simulation\s+stud|monte\s+carlo|\bwe\s+simulate|\bsimulated\s+(data|datasets)|\bsimulations\s+(show|showed|reveal|demonstrate|indicate))---",
  tutorial = r"---(\btutorial\b|\bprimer\b|\bR\s+package\b|\bintroduc\w+\s+(a|an|the)\s+(new\s+)?(method|tool|package|approach|framework))---",
  protocol = r"---(\bprotocol\b|registered\s+report|stage\s+1)---"
)
.ds_comment_title <- r"---(^\s*(a\s+)?(comment|commentary|reply|response|rejoinder|editorial)\b|\b(comment\s+on|reply\s+to|response\s+to|rejoinder\s+to)\b)---"
# the same kinds of paper, suggested only by sentences in the body
.ds_body <- list(
  simulation = r"---(simulation\s+stud|monte\s+carlo|\bwe\s+simulated\b|\bsimulated\s+(data|datasets?|samples|participants)|\bdata\s+(were|was)\s+(simulated|generated)|number\s+of\s+(simulations|iterations|replications|repetitions)|\b\d[\d,]*\s+(simulations|simulated|iterations|replications|repetitions)\b|simulation\s+conditions|set\.seed|data[- ]generating)---",
  review = r"---(databases?\s+(were|was)\s+searched|search\s+strateg|\bwe\s+searched\b|search\s+terms|\bprisma\b|inclusion\s+criteria\s+for\s+(studies|articles))---",
  papers = r"---(\b(coded|screened)\b.{0,40}\b(articles|papers|publications|preregistrations|abstracts)\b|\barticles\s+published\s+in\b|\bsample\s+of\s+(articles|papers|publications)\b)---",
  theory = r"---(\btheorem\b|\blemma\b|\bcorollary\b)---"
)
.ds_head_participants <- r"---(^\s*([0-9.]+\s*)?(participants?|subjects|sample|procedures?|apparatus|stimuli|participants\s+and\s+procedures?|recruitment|study\s+population|animals)\s*$)---"
.ds_head_method <- r"---(method|participant|subject|\bsample\b|procedure|\bdesign\b|material|measure|recruit|study\s+population|data\s+collection|setting|(study|experiment)\s*\d)---"

#' Well-known open resources
#'
#' Names of research resources that any researcher can download or apply
#' for, and that come already de-identified. ethics_check does not ask for an
#' ethics statement for a paper that analyses one of these (see
#' `.data_source()`). The list was written from general knowledge, not from
#' the papers the rule was tested on; add names here.
#'
#' @keywords internal
#' @noRd
.ds_open_resources <- c(
  r"---(\bUK\s+Biobank\b)---", r"---(\bNHANES\b)---", r"---(National\s+Health\s+and\s+Nutrition\s+Examination\s+Survey)---",
  r"---(\bBRFSS\b)---", r"---(Behavioral\s+Risk\s+Factor\s+Surveillance)---", r"---(\bNHIS\b)---",
  r"---(National\s+Health\s+Interview\s+Survey)---", r"---(\bMEPS\b)---", r"---(Medical\s+Expenditure\s+Panel\s+Survey)---",
  r"---(\bNSDUH\b)---", r"---(National\s+Survey\s+on\s+Drug\s+Use)---", r"---(\bYRBSS?\b)---", r"---(Youth\s+Risk\s+Behavior)---",
  r"---(\bAdd\s+Health\b)---", r"---(National\s+Longitudinal\s+(Study|Survey)\s+of\s+(Adolescent|Youth))---", r"---(\bNLSY\d*\b)---",
  r"---(Health\s+and\s+Retirement\s+Study)---", r"---(English\s+Longitudinal\s+Study\s+of\s+Age?ing)---",
  r"---(Survey\s+of\s+Health,?\s+Age?ing\s+and\s+Retirement\s+in\s+Europe)---", r"---(\bCHARLS\b)---", r"---(\bMIDUS\b)---",
  r"---(Midlife\s+in\s+the\s+United\s+States)---", r"---(\bABCD\s+[Ss]tudy)---", r"---(Adolescent\s+Brain\s+Cognitive\s+Development)---",
  r"---(\bAll\s+of\s+Us\b)---", r"---(\bADNI\b)---", r"---(Alzheimer.s\s+Disease\s+Neuroimaging\s+Initiative)---", r"---(\bPPMI\b)---",
  r"---(Parkinson.s\s+Progression\s+Markers)---", r"---(Human\s+Connectome\s+Project)---", r"---(\bOpenNeuro\b)---",
  r"---(\bMIMIC-?(III|IV|CXR)?\b)---", r"---(\beICU\b)---", r"---(\bPhysioNet\b)---", r"---(\bTCGA\b)---", r"---(Cancer\s+Genome\s+Atlas)---",
  r"---(\bGTEx\b)---", r"---(1000\s+Genomes)---", r"---(\bgnomAD\b)---", r"---(Gene\s+Expression\s+Omnibus)---", r"---(\bdbGaP\b)---",
  r"---(\bSEER\s+(database|program|registry|data))---", r"---(Demographic\s+and\s+Health\s+Surveys?)---",
  r"---(Multiple\s+Indicator\s+Cluster\s+Surveys?)---", r"---(World\s+Values\s+Survey)---", r"---(European\s+Social\s+Survey)---",
  r"---(General\s+Social\s+Survey)---", r"---(Understanding\s+Society)---", r"---(\bUKHLS\b)---", r"---(British\s+Household\s+Panel)---",
  r"---(Millennium\s+Cohort\s+Study)---", r"---(British\s+Cohort\s+Study)---", r"---(National\s+Child\s+Development\s+Study)---",
  r"---(British\s+Social\s+Attitudes)---",r"---(Socio-?Economic\s+Panel)---", r"---(\bLISS\s+panel)---", r"---(\bHILDA\b)---",
  r"---(\bPISA\b)---", r"---(\bTIMSS\b)---", r"---(\bPIRLS\b)---", r"---(\bICILS\b)---", r"---(Early\s+Childhood\s+Longitudinal\s+Study)---",
  r"---(Fragile\s+Families)---", r"---(Future\s+of\s+Families)---", r"---(Global\s+Burden\s+of\s+Disease)---", r"---(\bFinnGen\b)---",
  r"---(\bKaggle\b)---", r"---(American\s+Community\s+Survey)---", r"---(Current\s+Population\s+Survey)---", r"---(\bIPUMS\b)---",
  r"---(Eurobarometer)---", r"---(Afrobarometer)---", r"---(American\s+National\s+Election\s+Stud)---",
  r"---(Cooperative\s+(Congressional\s+)?Election\s+Study)---", r"---(Project\s+Implicit)---", r"---(\bCHILDES\b)---",
  r"---(\bTalkBank\b)---", r"---(National\s+Survey\s+of\s+Children.s\s+Health)---", r"---(\bNESARC\b)---",
  r"---(Monitoring\s+the\s+Future)---", r"---(China\s+Family\s+Panel\s+Studies)---", r"---(China\s+Health\s+and\s+Nutrition\s+Survey)---",
  r"---(\bKNHANES\b)---", r"---(Longitudinal\s+Aging\s+Study\s+in\s+India)---", r"---(\bICPSR\b)---", r"---(UK\s+Data\s+Service)---",
  r"---(Human\s+Mortality\s+Database)---", r"---(\bGWAS\s+Catalog\b)---", r"---(\bOpenAlex\b)---",
  r"---(Violence\s+Against\s+Children\s+(and\s+Youth\s+)?Surveys?)---"
)

# signs in the results of other modules
.ds_col_id <- r"---(^(participant|subject|subj|ppt|pp|pid|prolific|worker|response|respondent|user)_?(id|nr|no|num|number|code)?$|^(id|sub|ppn|responseid|workerid|prolific_pid)$)---"
.ds_col_sim <- r"---(^(iter|iteration|rep|replication|seed|nsim|n_sim|sim|bias|rmse|mse|coverage|true_\w+|power|type_?1|alpha)(_|\d|$))---"
.ds_file_material <- r"---(\.(qsf|psyexp|psydat|osexp|iqx|jasp|ebs\d?|es\d?)$|jspsych|consent|debrief|questionnaire|survey|stimul|instruction|ethic|irb|information[ _-]?sheet|recruit)---"
.ds_file_sim <- r"---(simulat|\bsim[_-]|_sim\b|monte)---"
.ds_prereg_people <- r"---(participant|recruit|prolific|mturk|respondent|sample\s+of|students|patients|children|adults)---"
.ds_human_concepts <- c("age", "gender", "race", "reaction_time", "likert", "accuracy")

# grepl with perl = TRUE that keeps going when one text cannot be searched
.ds_has <- function(pattern, x, ignore.case = TRUE) {
  if (!length(x)) return(logical(0))
  x[is.na(x)] <- ""
  .search_kept(pattern, x, "sentence", ignore.case = ignore.case, perl = TRUE)
}

# the number of TRUE values per paper, in the order of `ids`
.ds_count <- function(flag, pid, ids) {
  if (!length(flag)) return(stats::setNames(integer(length(ids)), ids))
  n <- tapply(as.integer(flag), factor(pid, levels = ids), sum)
  n[is.na(n)] <- 0L
  stats::setNames(as.integer(n), ids)
}

# a module's table, with every row given a paper_id
.ds_module_table <- function(prev, module, item, ids) {
  x <- prev[[module]][[item]]
  if (!is.data.frame(x) || !nrow(x)) return(NULL)
  if (length(ids) == 1) {
    x$paper_id <- ids
  } else if (!"paper_id" %in% names(x)) {
    return(NULL)
  }
  x$paper_id <- as.character(x$paper_id)
  x
}

# counts from the results of other modules, per paper
.ds_module_signs <- function(prev, ids, paper = NULL) {
  zero <- stats::setNames(integer(length(ids)), ids)
  out <- list(power_n = zero, power_apriori = zero, p_n = zero, test_n = zero,
              dep_earlier_text = zero, dep_others_text = zero, dep_earlier = zero,
              people_cols = zero, id_cols = zero, sim_cols = zero,
              material_files = zero, sim_files = zero, prereg_people = zero)

  pw <- .ds_module_table(prev, "power", "table", ids)
  if (!is.null(pw)) {
    out$power_n <- .ds_count(rep(TRUE, nrow(pw)), pw$paper_id, ids)
    type <- tolower(as.character(pw[["power_type"]] %||% rep(NA, nrow(pw))))
    out$power_apriori <- .ds_count(type %in% c("apriori", "a priori", "a-priori"), pw$paper_id, ids)
  }
  # p-values: from stat_p_exact, or found here when it did not run
  if (!is.null(prev[["stat_p_exact"]])) {
    p <- .ds_module_table(prev, "stat_p_exact", "table", ids)
  } else {
    p <- if (!is.null(paper)) tryCatch(extract_p_values(paper), error = function(e) NULL)
    if (is.data.frame(p) && nrow(p) && length(ids) == 1) p$paper_id <- ids
  }
  if (is.data.frame(p) && nrow(p) && "paper_id" %in% names(p)) {
    out$p_n <- .ds_count(rep(TRUE, nrow(p)), as.character(p$paper_id), ids)
  }
  st <- .ds_module_table(prev, "stat_check", "table", ids)
  if (!is.null(st)) out$test_n <- .ds_count(rep(TRUE, nrow(st)), st$paper_id, ids)

  own <- .ds_module_table(prev, "repo_check", "ownership", ids)
  own_urls <- character(0)
  if (!is.null(own) && all(c("ownership", "repo_url") %in% names(own))) {
    text_src <- (own[["source"]] %||% rep(NA, nrow(own))) %in% "text"
    out$dep_earlier_text <- .ds_count(own$ownership %in% "own_earlier_data" & text_src, own$paper_id, ids)
    out$dep_others_text <- .ds_count(own$ownership %in% "others_data" & text_src, own$paper_id, ids)
    out$dep_earlier <- .ds_count(own$ownership %in% "own_earlier_data", own$paper_id, ids)
    own_urls <- unique(own$repo_url[own$ownership %in% "own_this"])
  }
  files <- .ds_module_table(prev, "repo_check", "table", ids)
  if (!is.null(files) && "repo_url" %in% names(files)) {
    files <- files[files$repo_url %in% own_urls, , drop = FALSE]
    fname <- as.character(files[["file_name"]] %||% rep(NA, nrow(files)))
    fpath <- as.character(files[["file_path"]] %||% rep(NA, nrow(files)))
    fpath[is.na(fpath) | !nzchar(fpath)] <- fname[is.na(fpath) | !nzchar(fpath)]
    out$material_files <- .ds_count(.ds_has(.ds_file_material, fname), files$paper_id, ids)
    out$sim_files <- .ds_count(.ds_has(.ds_file_sim, fpath), files$paper_id, ids)
  }
  cols <- .ds_module_table(prev, "data_check", "table", ids)
  if (!is.null(cols) && "repo_url" %in% names(cols)) {
    cols <- cols[cols$repo_url %in% own_urls, , drop = FALSE]
    concept <- as.character(cols[["concept"]] %||% rep(NA, nrow(cols)))
    concept[is.na(concept)] <- ""
    cname <- as.character(cols[["column_name"]] %||% rep(NA, nrow(cols)))
    out$people_cols <- .ds_count(concept %in% .ds_human_concepts | startsWith(concept, "qualtrics"),
                                 cols$paper_id, ids)
    out$id_cols <- .ds_count(.ds_has(.ds_col_id, cname), cols$paper_id, ids)
    out$sim_cols <- .ds_count(.ds_has(.ds_col_sim, cname), cols$paper_id, ids)
  }
  pre <- .ds_module_table(prev, "prereg_check", "table", ids)
  if (!is.null(pre)) {
    keep <- setdiff(names(pre), c("link", "id", "ia_url", "paper_id"))
    txt <- vapply(seq_len(nrow(pre)), \(r) {
      v <- unlist(lapply(pre[r, keep, drop = FALSE], \(x) as.character(unlist(x))))
      paste(v[!is.na(v) & nzchar(v)], collapse = " ")
    }, character(1))
    out$prereg_people <- .ds_count(.ds_has(.ds_prereg_people, txt), pre$paper_id, ids)
  }
  out
}

#' Where a paper's data came from
#'
#' Sorts each paper by where its data came from, to decide whether an ethics
#' statement is expected (#695, point 18). See the comments at the top of
#' `R/data-source.R` for the signs and how the rule was made and tested.
#'
#' @param paper a paper object, a paper list, or a table of sentences with
#'   `paper_id`, `text`, `header` and `section_type` columns
#' @param info a table with `paper_id` and `title`, for a table of sentences
#'   (read from the papers otherwise)
#' @param prev the results of other modules that ran before, as a named list
#'   of module outputs (`power`, `stat_p_exact`, `stat_check`, `repo_check`,
#'   `data_check`, `prereg_check`); the text alone is used for any that is
#'   missing, and p-values are then found in the text
#'
#' @returns a list with `summary` (one row per paper: `paper_id`,
#'   `data_source`, `needs_ethics`, `signs`) and `evidence` (the sentences
#'   behind it, with a `sign` column). `data_source` is one of "own" (the
#'   authors collected the data, now or earlier, or will: a protocol),
#'   "existing_records" (existing records obtained for this study),
#'   "existing_unclear" (existing data, neither open nor obtained wording),
#'   "open_resource", "none" (no data about people), "uncertain" or
#'   "no_text". `needs_ethics` is TRUE for the first three, FALSE for
#'   "open_resource" and "none", and NA otherwise.
#' @keywords internal
.data_source <- function(paper, info = NULL, prev = list()) {
  sent <- text_search(paper)
  if (is.data.frame(paper)) {
    ids <- unique(as.character(paper$paper_id))
  } else {
    ids <- paper_id(paper)
  }
  ids <- ids[!is.na(ids)]
  sent$paper_id <- as.character(sent$paper_id)
  sent <- sent[sent$paper_id %in% ids, , drop = FALSE]
  if (is.null(info) && !is.data.frame(paper)) {
    info <- tryCatch(paper_table(paper, "info"), error = function(e) NULL)
  }
  title <- stats::setNames(rep("", length(ids)), ids)
  if (is.data.frame(info) && "title" %in% names(info) && nrow(info)) {
    t <- as.character(info$title)
    t[is.na(t)] <- ""
    title[as.character(info$paper_id)[as.character(info$paper_id) %in% ids]] <- t[as.character(info$paper_id) %in% ids]
  }
  pid <- sent$paper_id
  txt <- as.character(sent$text)
  txt[is.na(txt)] <- ""
  header <- as.character(sent$header)
  header[is.na(header)] <- ""
  is_abstract <- sent$section_type %in% "abstract" | tolower(trimws(header)) == "abstract"
  cnt <- \(flag) .ds_count(flag, pid, ids)

  # each sentence: whose study, and when
  first <- .ds_has(.ds_first, txt)
  others <- .ds_has(.ds_others, txt, ignore.case = FALSE)
  named <- .ds_has(.ds_named, txt, ignore.case = FALSE)
  future <- .ds_has(.ds_future, txt)
  no_recruit <- .ds_has(.ds_no_recruit, txt)
  cohort <- .ds_has(.ds_cohort, txt)
  meth_head <- .ds_has(.ds_head_method, header)
  # a part about the data or the method; open-resource wording also counts
  # in the abstract
  src_head <- .ds_has(.ds_head_source, header)
  src_abs <- src_head | is_abstract

  own_rows <- logical(length(txt))
  f <- list()
  for (k in names(.ds_kinds)) {
    hit <- .ds_has(.ds_kinds[[k]], txt) & !no_recruit
    oth <- hit & (others | (k == "recruit" & cohort & !first) | (named & !first))
    fut <- hit & !oth & future
    own <- hit & !oth & !fut
    f[[paste0(k, "_own")]] <- cnt(own)
    f[[paste0(k, "_meth")]] <- cnt(own & meth_head)
    f[[paste0(k, "_first")]] <- cnt(own & first)
    f[[paste0(k, "_fut")]] <- cnt(fut)
    if (k %in% .ds_people_kinds) own_rows <- own_rows | own
  }
  open_hit <- .ds_has(paste(.ds_open_resources, collapse = "|"), txt, ignore.case = FALSE)
  public_hit <- .ds_has(.ds_public, txt)
  records_hit <- .ds_has(.ds_records, txt)
  existing_hit <- .ds_has(.ds_existing, txt) & !others
  earlier_hit <- .ds_has(.ds_earlier, txt)
  aggregate_hit <- .ds_has(.ds_aggregate, txt)
  no_data_hit <- .ds_has(.ds_no_data, txt)
  individual_hit <- .ds_has(.ds_individual, txt) & !others
  f$future <- cnt(future)
  f$aggregate <- cnt(aggregate_hit)
  f$individual <- cnt(individual_hit)
  f$public_src <- cnt(public_hit & src_abs)
  f$open_src <- cnt(open_hit & src_abs)
  f$open_any <- cnt(open_hit)
  f$open_title <- as.integer(.ds_has(paste(.ds_open_resources, collapse = "|"), title, ignore.case = FALSE))
  f$records <- cnt(records_hit)
  f$no_recruit <- cnt(no_recruit)
  f$no_data <- cnt(no_data_hit)
  f$earlier <- cnt(earlier_hit)
  f$existing <- cnt(existing_hit)
  f$existing_src <- cnt(existing_hit & src_head)
  f$named_src <- cnt(named & !others & src_head)
  for (k in names(.ds_body)) f[[paste0(k, "_body")]] <- cnt(.ds_has(.ds_body[[k]], txt))
  f$n_chars <- cnt(nchar(txt))

  # title and abstract, and the headings
  abstract <- vapply(ids, \(i) paste(txt[pid == i & is_abstract], collapse = " "), character(1))
  # recycle0: an empty paper list has no ids, and paste() would still give
  # one " . ", so the table of signs could not be made
  ta <- paste(title, ".", abstract, recycle0 = TRUE)
  for (k in names(.ds_type)) f[[paste0(k, "_ta")]] <- as.integer(.ds_has(.ds_type[[k]], ta))
  f$comment_title <- as.integer(.ds_has(.ds_comment_title, title))
  f$existing_ta <- as.integer(.ds_has(.ds_existing, ta))
  f$named_ta <- as.integer(.ds_has(.ds_named, ta, ignore.case = FALSE))
  heads <- lapply(ids, \(i) { h <- unique(header[pid == i]); h[nzchar(h)] })
  f$head_participants <- vapply(heads, \(h) sum(.ds_has(.ds_head_participants, h)), integer(1))
  f$head_method <- vapply(heads, \(h) sum(.ds_has(.ds_head_method, h)), integer(1))
  f$named_head <- vapply(heads, \(h) sum(.ds_has(.ds_named, h, ignore.case = FALSE)), integer(1))
  m <- .ds_module_signs(prev, ids, if (is.data.frame(paper)) NULL else paper)
  d <- as.data.frame(c(f, m))
  d$paper_id <- ids

  # the rule ----
  nk <- \(suffix, ks = .ds_people_kinds) rowSums(d[, paste0(ks, suffix), drop = FALSE] > 0)
  people <- nk("_own") + (d$head_participants > 0)
  own_n <- people + (d$generic_own > 0) + (d$power_n > 0) + (d$people_cols > 0)
  meth_n <- nk("_meth") + (d$head_participants > 0)
  first_n <- nk("_first", c("consent", "recruit", "interview"))
  contact <- d$recruit_own > 0 | d$pay_own > 0 | d$consent_own > 0
  future_n <- nk("_fut")
  results <- d$p_n > 0 | d$test_n > 0
  review <- d$review_ta > 0
  method <- d$tutorial_ta > 0 | d$simulation_ta > 0
  comment <- d$comment_title > 0 | d$commentary_ta > 0
  typed <- review | method | comment
  typed_body <- d$review_body >= 2 | d$simulation_body >= 3 | d$theory_body > 0 | d$papers_body > 0
  bare <- d$head_method == 0 & d$sample_own == 0 & !results
  # the own-collection evidence the paper type asks for
  own_ok <- ifelse(review, first_n >= 1 & own_n >= 3,
            ifelse(method | comment, contact & own_n >= 2,
                   people >= 1 & (own_n >= 2 | meth_n >= 1)))
  out <- rep("uncertain", nrow(d))
  out[typed & !own_ok] <- "none"
  out[!typed & (typed_body | bare) & own_n <= 1] <- "none"
  out[own_ok] <- "own"
  # existing data: strong signs win unless the authors recruited or paid
  # people themselves; weak signs decide only when nothing else did
  ex_strong <- d$existing_ta > 0 | d$existing_src > 0 | d$no_recruit > 0 | d$dep_others_text > 0 |
    d$named_ta > 0 | d$named_src >= 2 | d$named_head > 0
  ex_weak <- d$existing > 0 | d$earlier > 0 | d$dep_earlier_text > 0
  own_contact <- d$recruit_own > 0 | d$pay_own > 0
  out[ex_strong & !own_contact & !typed] <- "existing"
  out[ex_weak & !typed & out == "uncertain"] <- "existing"
  out[ex_weak & !typed & out == "none" & (d$sample_own > 0 | results)] <- "existing"
  # figures about groups or things, or the paper's statement that it has no
  # data, overrule all but the authors' own contact with people
  agg <- (d$aggregate >= 2 & d$individual == 0) | d$no_data > 0
  out[agg & !contact] <- "none"
  out[d$aggregate >= 2 & d$individual > 0 & !contact & !typed & out == "none"] <- "existing"
  # the other modules: a preregistration about participants, an a-priori
  # power analysis, participant-number columns or study materials in the
  # authors' own deposit, t- or F-tests, or an earlier deposit of their own
  own_mod <- (d$prereg_people > 0) + (d$power_apriori > 0) + (d$id_cols > 0) + (d$material_files > 0) +
    (d$test_n > 0) + (d$dep_earlier > 0)
  out[out == "existing" & own_mod >= 1 & people >= 1 & d$existing_ta == 0 & d$no_recruit == 0] <- "own"
  out[out == "uncertain" & own_mod >= 1 & people >= 1] <- "own"
  out[out == "uncertain" & own_mod == 0 & (d$sim_cols > 0 | d$sim_files > 0)] <- "none"
  # protocols: collection in the future tense and no results
  out[(future_n >= 1 | d$protocol_ta > 0 & d$future >= 2) & !results & out != "own"] <- "own"
  # open resources and obtained records
  open <- d$open_title > 0 | d$open_src > 0 | d$open_any >= 3
  out[out == "own" & open & !own_contact & d$records == 0] <- "open_resource"
  ex <- out == "existing"
  out[ex & d$records > 0] <- "existing_records"
  out[ex & d$records == 0 & (d$public_src > 0 | open)] <- "open_resource"
  out[ex & d$records == 0 & !(d$public_src > 0 | open)] <- "existing_unclear"
  out[d$n_chars == 0] <- "no_text"

  needs <- ifelse(out %in% c("own", "existing_records", "existing_unclear"), TRUE,
           ifelse(out %in% c("open_resource", "none"), FALSE, NA))

  # what the decision rested on, in words: only the signs behind the class
  # the paper got, so that outweighed signs are not listed as reasons ----
  sign_words <- c(consent = "consent", recruit = "recruitment", pay = "payment or a recruitment platform",
                  sample = "a sample description", procedure = "what participants did",
                  interview = "interviews or focus groups", animal = "animal housing")
  signs <- vapply(seq_along(ids), \(i) {
    people_s <- c(sign_words[unlist(d[i, paste0(.ds_people_kinds, "_own")]) > 0],
                  if (d$head_participants[i] > 0) "a Participants or Procedure heading")
    existing_s <- c(if (d$existing_ta[i] + d$existing_src[i] + d$existing[i] > 0) "existing-data wording",
                    if (d$earlier[i] > 0 || d$dep_earlier_text[i] > 0) "an earlier study",
                    if (d$named_ta[i] + d$named_src[i] + d$named_head[i] > 0) "a named study, survey or cohort",
                    if (d$no_recruit[i] > 0) "a statement that nobody was recruited")
    type_s <- c(if (review[i]) "a review or meta-analysis named in the title or abstract",
                if (d$tutorial_ta[i] > 0) "a tutorial or software paper named in the title or abstract",
                if (d$simulation_ta[i] > 0) "a simulation study named in the title or abstract",
                if (comment[i]) "a commentary",
                if (d$review_body[i] >= 2) "a literature search",
                if (d$simulation_body[i] >= 3) "simulations",
                if (d$theory_body[i] > 0) "theorems or proofs",
                if (d$papers_body[i] > 0) "coding of published papers",
                if (d$aggregate[i] >= 2 && d$individual[i] == 0) "figures about groups or things",
                if (d$no_data[i] > 0) "a statement that there are no data",
                if (d$sim_cols[i] + d$sim_files[i] > 0 && own_mod[i] == 0) "simulation files in the authors' deposit")
    s <- switch(out[i],
      own = c(people_s, if (future_n[i] > 0) "collection in the future tense",
              if (own_mod[i] > 0) "results of other modules (preregistration, power analysis, tests or shared data)"),
      existing_records = c(existing_s, "signs of records obtained for this study"),
      existing_unclear = c(existing_s, people_s),
      open_resource = c(if (open[i]) "a well-known open resource",
                        if (d$public_src[i] > 0) "wording that the data are openly available"),
      none = type_s,
      uncertain = c(people_s, existing_s, type_s),
      character(0))
    paste(s, collapse = "; ")
  }, character(1))
  summary <- data.frame(paper_id = ids, data_source = out, needs_ethics = needs, signs = signs)

  # the sentences behind each decision ----
  cls <- stats::setNames(out, ids)[pid]
  sign <- rep(NA_character_, length(txt))
  sign[cls %in% "own" & (own_rows | future)] <- "own"
  sign[cls %in% c("existing_records", "existing_unclear") & (existing_hit | earlier_hit | no_recruit | (named & !others & src_head))] <- "existing"
  sign[cls %in% "existing_records" & records_hit] <- "records"
  sign[cls %in% "open_resource" & (open_hit | (public_hit & src_abs))] <- "open"
  sign[cls %in% "none" & (no_data_hit | aggregate_hit | (is_abstract & .ds_has(paste(unlist(.ds_type[1:4]), collapse = "|"), txt)))] <- "none"
  evidence <- sent[!is.na(sign), , drop = FALSE]
  evidence$sign <- sign[!is.na(sign)]

  list(summary = summary, evidence = evidence, features = d)
}
