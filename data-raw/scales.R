# Build `scales`: a curated dictionary of psychometric instruments used by
# codebook_check to identify scales in shared data. One row per instrument.
#
# Sources:
#   * The `scale_meta` dataset (data-raw/scale_items.R), harvested from the
#     OpenScales .osd definition files: ~1,061 instruments, of which the ones
#     safe to match against prose (`text_ok`) become dictionary rows.
#   * Curated additions (embedded below): widely-used instruments not in
#     OpenScales, hand-selected from published scale indexes (arabpsychology.com
#     CC-BY list; Wikipedia list of psychiatric rating scales).
#
# Why scale_meta and not manifest.json: the manifest lists ~204 instruments,
# while the repo ships ~1,095 .osd definitions. Building from the manifest left
# 862 instruments (70,070 items, 2,838 reverse-keyed) with item-level data that
# NO name lookup could reach — `code` resolved for only 199 rows. Sourcing both
# tables from the same place keeps `scales$code` a working join key into
# scale_meta / scale_items / scale_scoring by construction.
#
# `text_ok` gates entry: it is FALSE for short/generic names and for ALL PhenX
# entries, which are named after the construct measured ("Insomnia", "General
# Well-being") rather than the instrument. Those are real measures with usable
# items, but their names match ordinary methods prose, so they must never enter
# a dictionary that is regex-scanned against a manuscript. Their items stay
# reachable by `code` for anything that already knows which instrument it has.
#
# Columns:
#   name    canonical full name (used to build a tolerant name-match regex)
#   acronym short trigger (>= 2 chars, not a common English word); "" when none
#   code    OpenScales code ("" for curated additions); joins to `scale_meta`,
#           `scale_items` and `scale_scoring` for item wording, subscales and
#           reverse-coding — see data-raw/scale_items.R
#   source  "openscales" | "curated"
#
# NOTE: the manifest fetched here is METADATA ONLY (~204 records, no items).
# The OpenScales repo itself ships ~1,095 .osd definitions carrying item text,
# subscale structure and per-item scoring weights. Those are harvested
# separately by data-raw/scale_items.R; this file stays name/acronym-only
# because that is all codebook_check's matcher needs.
#
# Acronyms are kept even when they COLLIDE (e.g. AQ = Autism Spectrum Quotient
# and Aggression Questionnaire): codebook_check disambiguates a collision from
# the codebook item wording and the paper text, and abstains when it cannot.
# Duplicates (same instrument under a slightly different name) ARE removed;
# genuine different-instrument collisions are KEPT.

# ── OpenScales instruments (from the harvested .osd definitions) ──────────────
# Run data-raw/scale_items.R first: it builds `scale_meta` from the .osd files.
# Falls back to the installed dataset so this script can be run on its own.
`%||%` <- function(a, b) if (is.null(a)) b else a
if (!exists("scale_meta")) {
  f <- file.path("data", "scale_meta.rda")
  if (file.exists(f)) load(f) else
    stop("scale_meta not found. Run data-raw/scale_items.R first.")
}

# Only instruments safe to match against running text (see `text_ok` above).
open_df <- data.frame(
  name    = as.character(scale_meta$name),
  acronym = as.character(scale_meta$acronym),
  code    = as.character(scale_meta$code),
  source  = "openscales"
)[scale_meta$text_ok, , drop = FALSE]
open_df <- open_df[nzchar(trimws(open_df$name)), , drop = FALSE]
rownames(open_df) <- NULL

# ── Curated additions (widely-used instruments not in OpenScales) ─────────────
# Collisions intentional (AQ, MFQ, RAS, SDS, SSS, ...). Duplicates of OpenScales
# entries are dropped by the merge step below, so harmless to list here.
curated <- tibble::tribble(
  ~name, ~acronym,
  # depression / mood
  "Beck Depression Inventory","BDI",
  "Center for Epidemiologic Studies Depression Scale","CES-D",
  "Hamilton Depression Rating Scale","HAM-D",
  "Montgomery-Asberg Depression Rating Scale","MADRS",
  "Mood Disorder Questionnaire","MDQ",
  "Edinburgh Postnatal Depression Scale","EPDS",
  "Zung Self-Rating Depression Scale","SDS",
  "Geriatric Depression Scale","GDS",
  "Quick Inventory of Depressive Symptomatology","QIDS",
  "Young Mania Rating Scale","YMRS",
  "Beck Hopelessness Scale","BHS",
  "Children's Depression Inventory","CDI",
  "Kutcher Adolescent Depression Scale","KADS",
  "Major Depression Inventory","MDI",
  "Mood and Feelings Questionnaire","MFQ",
  "Altman Self-Rating Mania Scale","ASRM",
  "Hypomania Checklist","HCL-32",
  "General Behavior Inventory","GBI",
  # anxiety / stress / worry / trauma
  "Beck Anxiety Inventory","BAI",
  "State-Trait Anxiety Inventory","STAI",
  "Hamilton Anxiety Rating Scale","HAM-A",
  "Hospital Anxiety and Depression Scale","HADS",
  "Penn State Worry Questionnaire","PSWQ",
  "Perceived Stress Scale","PSS",
  "Liebowitz Social Anxiety Scale","LSAS",
  "Social Phobia Inventory","SPIN",
  "Social Interaction Anxiety Scale","SIAS",
  "Social Phobia and Anxiety Inventory","SPAI",
  "Kessler Psychological Distress Scale","K10",
  "Intolerance of Uncertainty Scale","IUS",
  "Connor-Davidson Resilience Scale","CD-RISC",
  "Brief Resilience Scale","BRS",
  "Taylor Manifest Anxiety Scale","TMAS",
  "Zung Self-Rating Anxiety Scale","SAS",
  "Impact of Event Scale-Revised","IES-R",
  "Posttraumatic Growth Inventory","PTGI",
  "Clinician Administered PTSD Scale","CAPS",
  "UCLA PTSD Reaction Index","UCLA-PTSD",
  "Panic Disorder Severity Scale","PDSS",
  # OCD / dissociation
  "Yale-Brown Obsessive Compulsive Scale","Y-BOCS",
  "Obsessive-Compulsive Inventory","OCI",
  "Dissociative Experiences Scale","DES",
  # personality / individual differences
  "NEO Personality Inventory","NEO-PI-R",
  "NEO Five-Factor Inventory","NEO-FFI",
  "Eysenck Personality Questionnaire","EPQ",
  "Narcissistic Personality Inventory","NPI",
  "Barratt Impulsiveness Scale","BIS-11",
  "Short Grit Scale","Grit-S",
  "Need for Cognition Scale","NFC",
  "Frost Multidimensional Perfectionism Scale","FMPS",
  "Almost Perfect Scale-Revised","APS-R",
  "Ten Item Personality Inventory","TIPI",
  "UPPS-P Impulsive Behavior Scale","UPPS-P",
  "Machiavellianism Scale","MACH-IV",
  "Levenson Self-Report Psychopathy Scale","LSRP",
  "Psychopathic Personality Inventory-Revised","PPI-R",
  "Regulatory Focus Questionnaire","RFQ",
  "Regulatory Mode Questionnaire","RMQ",
  "Behavioral Inhibition and Activation Scales","BIS/BAS",
  "Sensation Seeking Scale","SSS",
  "Minnesota Multiphasic Personality Inventory","MMPI",
  "Hare Psychopathy Checklist","PCL-R",
  "Personality Inventory for DSM-5","PID-5",
  "Big Five Inventory","BFI",
  "HEXACO Personality Inventory","HEXACO",
  # judgment / decision-making / cognition
  "Cognitive Reflection Test","CRT",
  "Berlin Numeracy Test","BNT",
  "Need for Cognitive Closure Scale","NFCC",
  "Actively Open-Minded Thinking Scale","AOT",
  "Rational-Experiential Inventory","REI",
  "Maximization Scale","MS",
  "General Decision-Making Style","GDMS",
  # emotion / regulation / wellbeing
  "Difficulties in Emotion Regulation Scale","DERS",
  "Toronto Alexithymia Scale","TAS-20",
  "Mindful Attention Awareness Scale","MAAS",
  "Subjective Happiness Scale","SHS",
  "Oxford Happiness Questionnaire","OHQ",
  "Warwick-Edinburgh Mental Wellbeing Scale","WEMWBS",
  "Meaning in Life Questionnaire","MLQ",
  "Berkeley Expressivity Questionnaire","BEQ",
  "Emotional Contagion Scale","ECS",
  "Flourishing Scale","FS",
  "Scale of Positive and Negative Experience","SPANE",
  "Positive and Negative Affect Schedule","PANAS",
  # self / social / moral / political
  "UCLA Loneliness Scale","UCLA-LS",
  "Multidimensional Scale of Perceived Social Support","MSPSS",
  "Marlowe-Crowne Social Desirability Scale","MCSDS",
  "Balanced Inventory of Desirable Responding","BIDR",
  "Interpersonal Reactivity Index","IRI",
  "Social Dominance Orientation","SDO",
  "Right-Wing Authoritarianism Scale","RWA",
  "Rathus Assertiveness Schedule","RAS",
  "Basic Psychological Needs Scale","BPNS",
  "Intrinsic Motivation Inventory","IMI",
  "Subjective Vitality Scale","SVS",
  "Self-Determination Scale","SDS",
  "Social Value Orientation","SVO",
  "Moral Foundations Questionnaire","MFQ",
  "Belief in a Just World Scale","BJW",
  "System Justification Scale","SJS",
  "Disgust Scale","DS",
  "Identification with All Humanity Scale","IWAH",
  # relationships / attachment
  "Experiences in Close Relationships","ECR",
  "Experiences in Close Relationships-Revised","ECR-R",
  "Relationship Assessment Scale","RAS",
  "Investment Model Scale","IMS",
  "Passionate Love Scale","PLS",
  "Inclusion of Other in the Self Scale","IOS",
  # body image / eating
  "Eating Attitudes Test","EAT-26",
  "Eating Disorder Examination Questionnaire","EDE-Q",
  "Eating Disorder Inventory","EDI",
  "Binge Eating Scale","BES",
  "SCOFF Questionnaire","SCOFF",
  "Body Appreciation Scale","BAS",
  "Drive for Muscularity Scale","DMS",
  # substance / addiction
  "Internet Addiction Test","IAT",
  "CRAFFT Screening Test","CRAFFT",
  "Bergen Shopping Addiction Scale","BSAS",
  # work / organizational
  "Maslach Burnout Inventory","MBI",
  "Utrecht Work Engagement Scale","UWES",
  "Minnesota Satisfaction Questionnaire","MSQ",
  "Organizational Commitment Questionnaire","OCQ",
  # ADHD / autism / child / clinical rating
  "Autism Spectrum Quotient","AQ",
  "Empathy Quotient","EQ",
  "Systemizing Quotient","SQ",
  "Adult ADHD Self-Report Scale","ASRS",
  "Strengths and Difficulties Questionnaire","SDQ",
  "Vanderbilt ADHD Diagnostic Parent Rating Scale","VADPRS",
  "Aggression Questionnaire","AQ",
  "Buss-Perry Aggression Questionnaire","BPAQ",
  "Wender Utah Rating Scale","WURS",
  "Conners Comprehensive Behavior Rating Scale","CBRS",
  "Childhood Autism Rating Scale","CARS",
  "Childhood Autism Spectrum Test","CAST",
  "Autism Diagnostic Observation Schedule","ADOS",
  "Ritvo Autism and Asperger Diagnostic Scale","RAADS",
  # cognition / dementia
  "Addenbrooke's Cognitive Examination","ACE-III",
  "Clinical Dementia Rating","CDR",
  "Mini-Mental State Examination","MMSE",
  "Montreal Cognitive Assessment","MoCA",
  "Informant Questionnaire on Cognitive Decline in the Elderly","IQCODE",
  # borderline / psychosis / general psychiatry
  "McLean Screening Instrument for Borderline Personality Disorder","MSI-BPD",
  "Zanarini Rating Scale for Borderline Personality Disorder","ZAN-BPD",
  "Brief Psychiatric Rating Scale","BPRS",
  "Positive and Negative Syndrome Scale","PANSS",
  "Scale for the Assessment of Positive Symptoms","SAPS",
  "Scale for the Assessment of Negative Symptoms","SANS",
  "Barnes Akathisia Rating Scale","BARS",
  "Comprehensive Psychopathological Rating Scale","CPRS",
  "Global Assessment of Functioning","GAF",
  "Children's Global Assessment Scale","CGAS",
  "Clinical Global Impression","CGI",
  # health / sleep / coping
  "Fatigue Severity Scale","FSS",
  "Insomnia Severity Index","ISI",
  "Pittsburgh Sleep Quality Index","PSQI",
  "Brief COPE","Brief-COPE",
  "Ways of Coping Questionnaire","WCQ"
)
curated$code   <- ""
curated$source <- "curated"
curated <- as.data.frame(curated, stringsAsFactors = FALSE)

# ── Curation helpers (kept identical in spirit to codebook_check's matcher) ────

# Common English / ambiguous 2-3 letter tokens that are never a usable acronym.
common_words <- c("ACE","AIM","CARE","COPE","FACE","FEAR","GAIN","HOPE","LIFE",
  "LOVE","MOOD","PAIN","REST","RISK","SELF","TEAM","TIME","WORK","MI","DT","AS",
  "IS","IT","OR","SO","US","NEW","OLD","MAP","KEY","SET","GET","RUN","WIN","SUM",
  "MAX","MIN","ONE","TWO","BIG")

acr_key <- function(a) toupper(gsub("[^A-Za-z0-9]", "", a %||% ""))
acr_ok  <- function(a) {
  k <- acr_key(a)
  nchar(k) >= 2 & !(k %in% common_words)
}

# Content tokens of a name: alnum words minus generic scale-type words, so two
# DIFFERENT instruments that both contain "Self ... Scale" are not merged.
generic_tok <- c("scale","scales","questionnaire","inventory","test","index",
  "survey","measure","checklist","rating","self","short","form","revised",
  "brief","version","the","of","for","and","a","an","in","to","assessment",
  "schedule","screen","screening")
content_tokens <- function(s) {
  toks <- unlist(strsplit(tolower(s), "[^a-z0-9]+"))
  toks <- toks[nzchar(toks) & !(toks %in% generic_tok)]
  unique(toks)
}

# ── Merge curated onto OpenScales, dropping duplicates but KEEPING collisions ──
# A curated row is a duplicate when its name matches an existing name exactly, OR
# it shares a usable acronym AND >=60% of the shorter name's CONTENT tokens with
# an existing scale carrying that acronym (same instrument, different spelling).
scales <- open_df
norm_name <- function(s) gsub("[^a-z0-9]", "", tolower(s))
seen_norm <- norm_name(scales$name)
# acronym -> names already present (for the collision-vs-duplicate test)
acr_names <- split(scales$name, acr_key(scales$acronym))

is_duplicate <- function(nm, acr) {
  if (norm_name(nm) %in% seen_norm) return(TRUE)
  if (!acr_ok(acr)) return(FALSE)
  existing <- acr_names[[acr_key(acr)]]
  if (is.null(existing)) return(FALSE)
  a <- content_tokens(nm)
  for (ex in existing) {
    b <- content_tokens(ex)
    if (length(a) == 0 || length(b) == 0) next
    ov <- length(intersect(a, b))
    if (ov >= 0.6 * min(length(a), length(b))) return(TRUE)
  }
  FALSE
}

keep <- logical(nrow(curated))
for (i in seq_len(nrow(curated))) {
  nm <- curated$name[i]; acr <- curated$acronym[i]
  if (is_duplicate(nm, acr)) { keep[i] <- FALSE; next }
  keep[i] <- TRUE
  seen_norm <- c(seen_norm, norm_name(nm))
  if (acr_ok(acr)) {
    k <- acr_key(acr)
    acr_names[[k]] <- c(acr_names[[k]], nm)
  }
}
scales <- rbind(scales, curated[keep, , drop = FALSE])

# Blank out acronyms that fail the safety rule (they become name-only entries).
scales$acronym[!vapply(scales$acronym, acr_ok, logical(1))] <- ""
rownames(scales) <- NULL

message(sprintf("scales: %d total (%d openscales + %d curated) | %d with acronym",
                nrow(scales), sum(scales$source == "openscales"),
                sum(scales$source == "curated"), sum(nzchar(scales$acronym))))
message(sprintf("        %d rows carry a code that resolves to item-level data",
                sum(nzchar(scales$code) & scales$code %in% scale_meta$code)))

usethis::use_data(scales, overwrite = TRUE, compress = "xz")
usethis::use_r("scales")
