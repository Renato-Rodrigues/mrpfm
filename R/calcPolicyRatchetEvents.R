# nolint start
#' Calculate country-level climate-policy ratchet-up events from CAPMF instruments
#'
#' Converts the instrument-level CAPMF stringency paths (LEV3 codes; ~50+
#' instruments per country-year) into discrete \strong{ratchet-up events} — the
#' outcome of the event-history reading of policy feasibility (Tier-1 direction 2
#' in `docs/psm-theoretical-directions.md`): a country-year has an event when any
#' instrument is \emph{adopted} (stringency moves off zero) or \emph{tightened}
#' by at least \code{jumpThreshold} index points. Unlike the smoothed
#' \code{\link{calcPolicyStringency}} index, events are country-level acts of
#' identifiable polities and carry far more identifying variation.
#'
#' Never imputed: a country-year needs a finite instrument value in both t-1 and
#' t to be at risk; years with no observable instrument pair are \code{NA} (not
#' zero). The first panel year is \code{NA} by construction.
#'
#' @param source character; `"official"` (default) or `"expanded"` (simulated
#'   stub — sensitivity only, see [`calcPolicyStringency()`]).
#' @param level character; CAPMF code level to read events from. Default
#'   `"LEV3"` (instruments); `"LEV4"` if present in the source.
#' @param jumpThreshold numeric; minimum stringency increase (index points) of a
#'   single instrument to count as a tightening event. Default `0.5`.
#'
#' @return A list with:
#'   \describe{
#'     \item{x}{[`magpie`][magclass::magclass] `[iso3c, year, variable]` with
#'       `Ratchet Event` (0/1: any event), `Ratchet Count` (number of instruments
#'       with an event) and `Ratchet Intensity` (summed positive stringency jumps
#'       over event instruments).}
#'     \item{weight}{`NULL` — regional aggregation sums counts/intensities (the
#'       0/1 event column then counts event-countries per region); the intended
#'       use is country-level (`aggregate = FALSE`).}
#'     \item{unit}{`"events (0/1); count; index points"`}
#'     \item{description}{character description}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat readSource
#' @importFrom magclass getNames getYears getItems new.magpie mbind
#'
#' @export
calcPolicyRatchetEvents <- function(source = "official", level = "LEV3", jumpThreshold = 0.5) {
  subtype <- switch(source,
    official = "all",
    expanded = "expanded",
    stop("calcPolicyRatchetEvents: unknown source '", source, "' (use \"official\" or \"expanded\")")
  )
  if (source == "expanded") {
    warning("calcPolicyRatchetEvents: the 'expanded' CAPMF database is currently a simulated ",
            "stub (see downloadCAPMF). Use for sensitivity mechanics only.")
  }
  x <- readSource("CAPMF", subtype = subtype, convert = TRUE)
  vars <- grep(paste0("^", level, "_"), magclass::getNames(x), value = TRUE)
  if (length(vars) == 0) {
    stop("calcPolicyRatchetEvents: no '", level, "_*' instrument codes in the CAPMF source. ",
         "Available levels: ",
         paste(sort(unique(sub("_.*$", "", magclass::getNames(x)))), collapse = ", "))
  }

  # Temporal differencing requires ascending years — never trust source order
  # (the CSV appearance order propagates into the magpie).
  years <- sort(magclass::getYears(x, as.integer = TRUE))
  x <- x[, years, ]
  arr <- as.array(x[, , vars])
  regions <- magclass::getItems(x, dim = 1)
  nY <- length(years)
  tol <- 1e-8

  event <- count <- intensity <- matrix(NA_real_, length(regions), nY,
                                        dimnames = list(regions, years))
  for (t in 2:nY) {
    # dimension-safe single-year slice (drops to a vector when only one instrument)
    prev <- matrix(arr[, t - 1, ], nrow = length(regions))
    cur <- matrix(arr[, t, ], nrow = length(regions))
    fin <- is.finite(prev) & is.finite(cur)
    atRisk <- rowSums(fin) > 0
    adopt <- fin & (prev <= tol) & (cur > tol)
    jump <- fin & ((cur - prev) >= jumpThreshold)
    ev <- adopt | jump
    cnt <- rowSums(ev, na.rm = TRUE)
    its <- rowSums(pmax(cur - prev, 0) * ev, na.rm = TRUE)
    cnt[!atRisk] <- NA_real_
    its[!atRisk] <- NA_real_
    count[, t] <- cnt
    intensity[, t] <- its
    event[, t] <- ifelse(is.na(cnt), NA_real_, as.numeric(cnt > 0))
  }

  mk <- function(m, nm) {
    out <- magclass::new.magpie(regions, years, nm, fill = NA)
    out[, , 1] <- as.vector(m)
    out
  }
  out <- magclass::mbind(
    mk(event, "Ratchet Event"),
    mk(count, "Ratchet Count"),
    mk(intensity, "Ratchet Intensity")
  )

  return(list(
    x = out,
    weight = NULL,
    unit = "events (0/1); count; index points",
    description = paste0("CAPMF ", level, " instrument-level ratchet-up events (adoption or ",
                         "stringency jump >= ", jumpThreshold, "); never imputed; first year NA")
  ))
}
# nolint end
