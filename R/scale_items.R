#' Psychometric Scale Metadata (item-level)
#'
#' One row per instrument harvested from the OpenScales `.osd` definition files,
#' the item-level companion to [scales]. Where `scales` answers *"is this column
#' block the PANAS?"* from a name or acronym, this table plus [scale_items] and
#' [scale_scoring] describe what the instrument actually contains: its items,
#' subscales, reverse-keyed items and reliabilities.
#'
#' Built from the OpenScales repository rather than its `manifest.json`, which
#' is metadata-only and covers ~200 of the ~1,100 available definitions.
#' Non-commercial / no-derivatives scales (the upstream `restricted/`
#' collection) are excluded so the data can ship under the package licence.
#'
#' @format A data frame with one row per instrument and 13 columns:
#' \describe{
#'   \item{code}{OpenScales code; joins to `scales$code`, `scale_items$code` and
#'     `scale_scoring$code`.}
#'   \item{name}{Canonical instrument name.}
#'   \item{acronym}{Abbreviation; empty when upstream supplies only a catalogue
#'     ID (`PX010201`, `MISS10991`) rather than a real acronym.}
#'   \item{collection}{`"ipip"`, `"phenx"`, `"openscales"` or `"miss"`.}
#'   \item{license}{Upstream licence string (e.g. `"Public Domain"`,
#'     `"CC BY 4.0"`).}
#'   \item{citation}{Source publication, when recorded.}
#'   \item{url}{Upstream reference URL.}
#'   \item{domain}{Broad content domain (e.g. `"Personality"`).}
#'   \item{n_items}{Number of items with resolvable English wording.}
#'   \item{n_reverse}{Number of reverse-keyed items.}
#'   \item{languages}{Comma-separated translation codes available upstream.}
#'   \item{text_ok}{`TRUE` when the name is safe to regex against manuscript
#'     prose — i.e. it names an *instrument* rather than a *topic*. `FALSE` for
#'     **all** PhenX entries, which are named after the construct measured
#'     (`"Insomnia"`, `"General Well-being"`) rather than the instrument;
#'     matching those against running text would fire on ordinary methods
#'     prose. Other collections name real instruments, so short names there
#'     (`"Grit Scale"`, `"COPE Inventory"`) still qualify. Items remain
#'     available for every row; only name matching is withheld.}
#' }
#' @source OpenScales (\url{https://github.com/stmueller/OpenScales}).
#'   Rebuild with `data-raw/scale_items.R`.
#' @seealso [scales] for name/acronym identification, [scale_items] for item
#'   wording, [scale_scoring] for subscale definitions.
#' @keywords internal
"scale_meta"

#' Psychometric Scale Items
#'
#' Item-level wording and reverse-key status for the instruments in
#' [scale_meta]. English wording only: items are resolved through the upstream
#' `translations[["en"]]` map, so scales published solely in another language
#' are absent (`scale_meta$languages` records what each scale offers).
#'
#' `reverse` is **recorded ground truth, not an inference**. OpenScales encodes
#' scoring as per-item weights (`+1` / `-1`) within each subscale; an item is
#' marked reverse here when it carries a `-1` weight in any subscale. Items in
#' a scale with no scoring block default to `FALSE`.
#'
#' @format A data frame with one row per item and 7 columns:
#' \describe{
#'   \item{code}{OpenScales code; joins to `scale_meta$code`.}
#'   \item{item_id}{Item identifier, unique within the instrument.}
#'   \item{text}{English item wording.}
#'   \item{dimension}{Subscale the item belongs to; empty when unassigned.}
#'   \item{type}{Item type (`"likert"`, `"multi"`, ...).}
#'   \item{reverse}{`TRUE` when the item is reverse-keyed (weight `-1`).}
#'   \item{position}{1-based position within the instrument.}
#' }
#' @source OpenScales (\url{https://github.com/stmueller/OpenScales}).
#'   Rebuild with `data-raw/scale_items.R`.
#' @seealso [scale_meta], [scale_scoring]
#' @keywords internal
"scale_items"

#' Psychometric Scale Scoring
#'
#' Subscale definitions for the instruments in [scale_meta]: how each dimension
#' is scored, how many items it draws on, how many of those are reverse-keyed,
#' and its reported reliability.
#'
#' `alpha` is parsed from the upstream free-text `description` (recorded as
#' `"Cronbach's alpha = 0.78"`) and range-checked to `[0, 1]`; out-of-range
#' values, of which upstream has a few, become `NA`. Rows where `n_items` is 0
#' are second-order factors defined over other subscales rather than over items
#' directly.
#'
#' @format A data frame with one row per instrument-subscale and 7 columns:
#' \describe{
#'   \item{code}{OpenScales code; joins to `scale_meta$code`.}
#'   \item{dimension}{Subscale identifier.}
#'   \item{method}{Scoring method (`"sum"`, `"mean_coded"`, ...).}
#'   \item{n_items}{Number of items entering the subscale.}
#'   \item{n_reverse}{Number of those items that are reverse-keyed.}
#'   \item{alpha}{Reported Cronbach's alpha, or `NA`.}
#'   \item{description}{Upstream free-text description.}
#' }
#' @source OpenScales (\url{https://github.com/stmueller/OpenScales}).
#'   Rebuild with `data-raw/scale_items.R`.
#' @seealso [scale_meta], [scale_items]
#' @keywords internal
"scale_scoring"
