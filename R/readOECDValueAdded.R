# nolint start
#' Read OECD value added by economic activity
#'
#' Reads the CSV downloaded by [`downloadOECDValueAdded()`] into a magpie object
#' `[iso3c, year, activity]`. Rows are filtered to gross value added (transaction
#' `B1G`) and, where a price-base column exists, to current prices (`V`) so the
#' resulting sector shares are shares of nominal value added. Activity variables
#' are named `"<code> <label>"` (dots/commas stripped — dots are magclass
#' subdimension separators), so [`computeSectorWeights()`] can match either the
#' ISIC code (`D35`, `C`, `L`, `H`) or the label text (electricity / manufactur /
#' real estate / transport).
#'
#' @returns A [`magpie`][magclass::magclass] object `[iso3c, year, activity]` of
#'   value added (current prices, national currency or the source unit — only the
#'   within-country cross-activity SHARES are consumed downstream, so the unit
#'   cancels).
#' @author Renato Rodrigues
#' @importFrom utils read.csv
#' @importFrom stats aggregate
#' @importFrom magclass as.magpie getSets<-
#' @export
readOECDValueAdded <- function() {
  files <- list.files(".", pattern = "oecd_value_added.*\\.csv$",
                      full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    stop("readOECDValueAdded: no 'oecd_value_added*.csv' in the source folder — ",
         "run downloadOECDValueAdded() (or place the OECD Table 6 CSV there).")
  }
  raw <- utils::read.csv(files[[1]], stringsAsFactors = FALSE, check.names = FALSE)
  colnames(raw) <- tolower(gsub("[^A-Za-z0-9]+", "_", colnames(raw)))

  need <- c("ref_area", "activity", "time_period", "obs_value")
  missing <- setdiff(need, colnames(raw))
  if (length(missing) > 0) {
    stop("readOECDValueAdded: CSV lacks column(s) ", paste(missing, collapse = ", "),
         " — found: ", paste(colnames(raw), collapse = ", "))
  }
  if ("transaction" %in% colnames(raw)) {
    raw <- raw[toupper(raw$transaction) == "B1G", , drop = FALSE]
  }
  if ("price_base" %in% colnames(raw) && any(toupper(raw$price_base) == "V")) {
    raw <- raw[toupper(raw$price_base) == "V", , drop = FALSE]
  }
  if (nrow(raw) == 0) stop("readOECDValueAdded: no B1G rows left after filtering.")

  # human-readable activity label column, if the CSV-with-labels variant was used
  labelCol <- intersect(c("economic_activity", "activity_label", "activity_name"),
                        colnames(raw))
  act <- if (length(labelCol) > 0 && any(nzchar(raw[[labelCol[1]]]))) {
    paste(raw$activity, raw[[labelCol[1]]])
  } else {
    raw$activity
  }
  # dots are magclass subdimension separators; commas confuse downstream greps
  act <- gsub("[.,]", "", act)

  df <- data.frame(
    region = toupper(raw$ref_area),
    year = as.integer(raw$time_period),
    activity = act,
    value = suppressWarnings(as.numeric(raw$obs_value)),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$value) & !is.na(df$year) & nchar(df$region) == 3, , drop = FALSE]
  if (nrow(df) == 0) stop("readOECDValueAdded: no usable ISO3 country rows found.")
  # collapse duplicates (e.g. residual multiple unit variants) by mean
  df <- stats::aggregate(value ~ region + year + activity, data = df, FUN = mean)

  x <- magclass::as.magpie(df, spatial = "region", temporal = "year", datacol = "value")
  magclass::getSets(x) <- c("iso3c", "year", "activity")
  x
}
# nolint end
