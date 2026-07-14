# nolint start
#' Sector weights for CAPMF Bulk/Diffuse aggregation (T2 aggregation sensitivity)
#'
#' Builds per-cell GHG-, GDP- or final-energy-share weights over the four CAPMF
#' sectors (Electricity, Industry, Buildings, Transport) for use as the `weighting`
#' argument of [`calcPolicyStringency()`], following Metta-Versmessen (2025). Kept
#' in the data layer so the aggregation-sensitivity analysis is executable entirely
#' from \pkg{mrpfm}/\pkg{pfm} (no paper-repo dependency).
#'
#' Sources (decided 2026-07-13, ADR-less T2 scope decision):
#' \itemize{
#'   \item `"ghg"` — sectoral GHG emissions from the EDGAR reader
#'     (`readSource("EDGARghg")`; the same series that disaggregates the effective
#'     carbon price), so the name says what the weights are. Sector map: Power
#'     Industry -> elec; Industrial Combustion + Processes + Fuel Exploitation ->
#'     ind (mirroring the bulk grouping in `calcCarbonPrice`); Buildings ->
#'     buildings; Transport -> transport.
#'   \item `"fe"` — final energy by sector (`calcOutput("FE")`) as an explicit,
#'     honestly-named activity proxy (the former "ghg" source, kept as a free
#'     robustness rung).
#'   \item `"gdp"` — value added by ISIC activity from the OECD annual national
#'     accounts reader (`readSource("OECDValueAdded")`), Metta-Versmessen proxy
#'     mapping: D35 electricity/gas -> elec; C manufacturing -> ind; L real
#'     estate -> buildings; H transport/storage -> transport.
#' }
#' You must confirm ONE thing for a given data setup: the mapping from the source
#' series' sub-sector labels to the four PSM sectors (the `map4` greps below). The
#' function `stop()`s loudly with the available labels if a sector cannot be mapped —
#' it never silently returns wrong weights.
#'
#' @param kind `"ghg"` (sector shares of GHG emissions, EDGAR), `"gdp"` (sector
#'   shares of value added, OECD), or `"fe"` (final-energy activity proxy).
#' @param ref a [`magpie`][magclass::magclass] object whose `iso3c` and `year`
#'   dimensions the weights are aligned to (typically the sector-index object built in
#'   [`calcPolicyStringency()`]).
#' @return A magpie `[iso3c, year, {elec,ind,buildings,transport}]` of per-cell shares
#'   (each cell's four shares sum to ~1 where data exist; `NA` where the source is
#'   missing, so [`calcPolicyStringency()`] falls back to the available members).
#' @author Renato Rodrigues
#' @importFrom madrat calcOutput readSource
#' @importFrom magclass getNames getYears getItems setNames mbind dimSums
#' @export
computeSectorWeights <- function(kind = c("ghg", "gdp", "fe"), ref) {
  kind <- match.arg(kind)
  SEC <- c("elec", "ind", "buildings", "transport")

  # Source the sectoral activity series (see the roxygen for the source decisions).
  src <- switch(kind,
    ghg = tryCatch(readSource("EDGARghg"), error = function(e) NULL),
    fe  = tryCatch(calcOutput("FE", aggregate = FALSE, warnNA = FALSE), error = function(e) NULL),
    gdp = tryCatch(readSource("OECDValueAdded"), error = function(e) NULL)
  )
  if (is.null(src)) {
    stop("computeSectorWeights: could not resolve a '", kind, "' source series. Wire a ",
         switch(kind, ghg = "sectoral-emissions (EDGARghg)",
                gdp = "by-activity value-added (OECDValueAdded)",
                fe = "final-energy"),
         " series here before using weighting = \"", kind, "\".")
  }
  message("computeSectorWeights[", kind, "]: source dim-3 labels: ",
          paste(getNames(src), collapse = ", "))

  # map4: sum the source sub-sectors into the four PSM sectors. CONFIRM these greps
  # against the labels printed above for your data setup.
  map4 <- switch(kind,
    ghg = list(
      elec      = grep("Power Industry",                             getNames(src), value = TRUE),
      ind       = grep("Industrial Combustion|Processes|Fuel Exploitation",
                       getNames(src), value = TRUE),
      buildings = grep("Buildings",                                  getNames(src), value = TRUE),
      transport = grep("Transport",                                  getNames(src), value = TRUE)
    ),
    gdp = list(
      elec      = grep("D35|electricity",     getNames(src), ignore.case = TRUE, value = TRUE),
      ind       = grep("^C$|manufactur",      getNames(src), ignore.case = TRUE, value = TRUE),
      buildings = grep("^L$|real estate",     getNames(src), ignore.case = TRUE, value = TRUE),
      transport = grep("^H$|transport",       getNames(src), ignore.case = TRUE, value = TRUE)
    ),
    fe = list(
      elec      = grep("elec|power|generation|utilit", getNames(src), ignore.case = TRUE, value = TRUE),
      ind       = grep("indust|manufact",              getNames(src), ignore.case = TRUE, value = TRUE),
      buildings = grep("build|resid|commerc|real",     getNames(src), ignore.case = TRUE, value = TRUE),
      transport = grep("transp",                       getNames(src), ignore.case = TRUE, value = TRUE)
    )
  )
  if (kind == "gdp" && length(getNames(src)) <= 1L) {
    stop("computeSectorWeights[gdp]: the value-added series is total-only; supply a ",
         "by-activity series (e.g. OECD/WDI value added) and complete map4.")
  }
  empty <- names(map4)[lengths(map4) == 0]
  if (length(empty)) {
    stop("computeSectorWeights[", kind, "]: no source label mapped to sector(s) ",
         paste(empty, collapse = ", "), ". Available labels: ",
         paste(getNames(src), collapse = ", "), " — complete map4.")
  }
  act <- do.call(mbind, lapply(SEC, function(s)
    setNames(dimSums(src[, , map4[[s]]], dim = 3, na.rm = TRUE), s)))

  # align to ref's iso3c x year, then normalise to per-cell shares over the four sectors
  reg <- intersect(getItems(act, dim = 1), getItems(ref, dim = 1))
  yrs <- intersect(getYears(act), getYears(ref))
  if (length(reg) == 0 || length(yrs) == 0) {
    stop("computeSectorWeights[", kind, "]: the weight source shares no ",
         if (length(reg) == 0) "countries" else "years",
         " with the sector index — cannot build weights (source: ",
         paste(range(getYears(act, as.integer = TRUE)), collapse = "-"), ", ",
         length(getItems(act, dim = 1)), " countries).")
  }
  act <- act[reg, yrs, SEC]
  tot <- dimSums(act, dim = 3, na.rm = TRUE); tot[tot == 0] <- NA
  act / tot
}
# nolint end
