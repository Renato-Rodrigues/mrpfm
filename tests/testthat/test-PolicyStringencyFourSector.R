test_that("calcPolicyStringency sectorResolution='four' emits the four sector indices", {
  mrLocalEnv("CAPMF")
  res <- suppressWarnings(suppressMessages(
    calcPolicyStringency(minCoverage = FALSE, sectorResolution = "four")))
  expect_s4_class(res$x, "magpie")
  expect_true(all(c("Electricity", "Industry", "Buildings", "Transport")
                  %in% getNames(res$x)))
  expect_false(any(c("bulk", "diffuse", "composite") %in% getNames(res$x)))
  # DEU 2020: Electricity = mean(E_MBI 5, E_NMBI 3) = 4; Industry = 2 (single child)
  expect_equal(as.numeric(res$x["DEU", 2020, "Electricity"]), 4.0)
  expect_equal(as.numeric(res$x["DEU", 2020, "Industry"]), 2.0)
  # Bulk (two-sector) = mean(Electricity, Industry) = 3 — consistency check
  two <- suppressWarnings(suppressMessages(calcPolicyStringency(minCoverage = FALSE)))
  expect_equal(as.numeric(res$x["DEU", 2020, "Electricity"]) / 2 +
                 as.numeric(res$x["DEU", 2020, "Industry"]) / 2,
               as.numeric(two$x["DEU", 2020, "bulk"]))
})
