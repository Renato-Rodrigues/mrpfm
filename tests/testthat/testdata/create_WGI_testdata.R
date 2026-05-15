# This script generates minimal synthetic Excel test data for WGI tests.
# Run ONCE from the project root to create testdata/WGIindicator/wgidataset_with_sourcedata-test.xlsx
# The file is committed to the repository so tests can run without internet access.

library(writexl)

indicators <- c("va", "pv", "ge", "rq", "rl", "cc")

make_sheet <- function(dimension) {
  data.frame(
    `ID variable (economy code/ gov. dimension/ year)` = paste0(
      c("DEU", "FRA", "ESP", "ITA", "BRA"), dimension, "2022"
    ),
    `Economy (name)` = c("Germany", "France", "Spain", "Italy", "Brazil"),
    `Economy (code)` = c("DEU", "FRA", "ESP", "ITA", "BRA"),
    Region = c(
      "Europe & Central Asia", "Europe & Central Asia",
      "Europe & Central Asia", "Europe & Central Asia",
      "Latin America & Caribbean"
    ),
    `Income classification` = "High income",
    Year = 2022,
    `Governance dimension` = dimension,
    `Number of sources` = 5L,
    `Governance estimate (approx. -2.5 to +2.5)` = c(1.2, 1.4, 0.9, 1.1, -0.1),
    `Standard error (estimate)` = 0.1,
    `Lower threshold (90% conf. int. estimate)` = c(1.0, 1.2, 0.7, 0.9, -0.3),
    `Upper threshold (90% conf. int. estimate)` = c(1.4, 1.6, 1.1, 1.3, 0.1),
    `Governance score (0-100)` = c(72, 78, 65, 68, 45),
    `Standard error (gov. score)` = 2.0,
    `Lower threshold (90% conf. int. score)` = c(70, 76, 63, 66, 43),
    `Upper threshold (90% conf. int. score)` = c(74, 80, 67, 70, 47),
    check.names = FALSE
  )
}

sheets <- stats::setNames(lapply(indicators, make_sheet), indicators)

out_dir <- file.path("tests", "testthat", "testdata", "WGIindicator")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

writexl::write_xlsx(
  sheets,
  path = file.path(out_dir, "wgidataset_with_sourcedata-test.xlsx")
)

cat("Created WGI test xlsx at:", file.path(out_dir, "wgidataset_with_sourcedata-test.xlsx"), "\n")
