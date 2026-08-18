#' Behavioural and Cognitive Task Dictionary
#'
#' A dictionary of behavioural and cognitive tasks (Stroop, Implicit Association
#' Task, n-back, Raven's Progressive Matrices), harvested from the Cognitive
#' Atlas task ontology. The task counterpart of [scales].
#'
#' A task is not a different kind of object from a scale. It is expressed in the
#' same OSD format, with trials as items and conditions as dimensions; see
#' `inst/osd/tasks-in-osd.md` for the mapping and `inst/osd/STROOP.osd` for a
#' worked example. There is no separate task format.
#'
#' Entries that OpenScales already defines are dropped, because the Cognitive
#' Atlas catalogues anything a participant does and so lists questionnaires
#' (CES-D, MMSE, Pittsburgh Sleep Quality Index) as "tasks". An OpenScales
#' definition carries items, subscales, scoring weights and often a reliability
#' estimate, where the corresponding Atlas record carries only prose, so the
#' OpenScales record wins and the duplicate is removed. 23 of the 857 Atlas
#' tasks are dropped this way.
#'
#' The Atlas records no design parameters, so no `parameters` block is
#' harvested. Those are authored by hand per task.
#'
#' @format A data frame with one row per task and 13 columns:
#' \describe{
#'   \item{code}{Slug used as the OSD `scale_info$code`: uppercase letters,
#'     digits and hyphens only.}
#'   \item{name}{Canonical task name.}
#'   \item{acronym}{Acronym from the Atlas `alias` field, when that field holds
#'     a real acronym rather than a synonym; empty otherwise.}
#'   \item{atlas_id}{Cognitive Atlas identifier (`trm_*` / `tsk_*`). Populated
#'     for every row, and the key that links a task to its Atlas record and to
#'     an Expfactory implementation, whose `config.json` carries the same id
#'     under `cognitive_atlas_task_id`.}
#'   \item{description}{Atlas definition text. Empty when the Atlas records the
#'     literal string `"None"`.}
#'   \item{citation}{Description of one citation, not necessarily the canonical
#'     one: Atlas citations are unordered, and the Implicit Association Task's
#'     first citation is not Greenwald, McGhee & Schwartz (1998).}
#'   \item{pmid}{PubMed id for `citation`; empty when none.}
#'   \item{url}{Canonical Cognitive Atlas URL for the task.}
#'   \item{n_conditions}{Conditions recorded upstream (e.g. congruent /
#'     incongruent). Zero for most tasks.}
#'   \item{n_contrasts}{Contrasts recorded upstream. These are mostly names
#'     ("response time to incongruent vs. response time to congruent") rather
#'     than computable definitions.}
#'   \item{n_computable}{Contrasts carrying real condition weights, and so
#'     expressible as OSD scoring. Non-zero for 7 tasks in the whole ontology.}
#'   \item{indicators}{Comma-separated normalised indicators
#'     (`response_time`, `accuracy`, `score`, `neural`, `rating`). The upstream
#'     field is free text with 225 distinct values, so it is mapped to this
#'     small vocabulary and dropped when it does not map.}
#'   \item{text_ok}{`TRUE` when the name is safe to match against manuscript
#'     prose. Present for consistency with `scale_meta`; unlike PhenX entries,
#'     Atlas task names are instrument names rather than topic labels, so this
#'     is `TRUE` for every current row.}
#' }
#' @source Cognitive Atlas (\url{https://www.cognitiveatlas.org}), harvested
#'   from its public task API. The Atlas has no per-task licence field: its own
#'   content is CC-BY, but tasks it merely names (WAIS, Conners) are
#'   proprietary. Nothing here grants a right to administer a task.
#'   Rebuild with `data-raw/tasks.R`.
#' @seealso [scales] for questionnaire instruments, `scale_meta` for their
#'   item-level detail.
#' @keywords internal
"tasks"
