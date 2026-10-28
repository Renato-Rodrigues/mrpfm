#' Download World Bank fossil rents
#'
#' Downloads the World Development Indicators oil, natural gas and coal rents (\% of GDP;
#' `NY.GDP.PETR.RT.ZS`, `NY.GDP.NGAS.RT.ZS`, `NY.GDP.COAL.RT.ZS`) for every country from 1990 on, one
#' indicator per request (the API is slow), and writes them to `wdi-fossil-rents.csv`
#' (columns `iso3c`, `year`, `oil`, `gas`, `coal`). Needs the `WDI` package. Used by pfm's frontier
#' with a heteroscedastic slack (design note 0009 section 8.2).
#'
#' @author Renato Rodrigues
#' @export
downloadWDIRents <- function() {
  if (!requireNamespace("WDI", quietly = TRUE)) {
    stop("downloadWDIRents: needs the 'WDI' package (install.packages('WDI')), or place wdi-fossil-rents.csv ",
         "(iso3c, year, oil, gas, coal) in the WDIRents source folder by hand.")
  }
  old <- options(timeout = 900); on.exit(options(old))
  ind <- c(oil = "NY.GDP.PETR.RT.ZS", gas = "NY.GDP.NGAS.RT.ZS", coal = "NY.GDP.COAL.RT.ZS")
  parts <- lapply(names(ind), function(n) {
    x <- WDI::WDI(country = "all", indicator = stats::setNames(ind[[n]], n), start = 1990,
                  end = as.integer(format(Sys.Date(), "%Y")), extra = FALSE)
    if (!nrow(x)) stop("downloadWDIRents: the download of ", ind[[n]], " returned nothing")
    x[, c("iso3c", "year", n)]
  })
  x <- Reduce(function(a, b) merge(a, b, by = c("iso3c", "year"), all = TRUE), parts)
  x <- x[nchar(x$iso3c) == 3, , drop = FALSE]
  utils::write.csv(x, "wdi-fossil-rents.csv", row.names = FALSE)
  list(url = "https://data.worldbank.org/indicator/NY.GDP.TOTL.RT.ZS",
       doi = NULL, title = "World Development Indicators: oil, natural gas and coal rents (% of GDP)",
       description = "Rents: the value of production at world prices less the total costs of production, % of GDP",
       author = "World Bank", unit = "% of GDP", version = format(Sys.Date()), release_date = format(Sys.Date()),
       license = "CC BY 4.0", reference = "World Bank (2024). World Development Indicators.")
}
