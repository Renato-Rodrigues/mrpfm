test_that("calcPolicyStringency weighting='equal' reproduces the default (regression)", {
  mrLocalEnv("CAPMF")
  a <- suppressWarnings(suppressMessages(calcPolicyStringency(minCoverage = FALSE)))
  b <- suppressWarnings(suppressMessages(
    calcPolicyStringency(minCoverage = FALSE, weighting = "equal")))
  expect_equal(as.numeric(a$x[, , "bulk"]), as.numeric(b$x[, , "bulk"]))
  expect_equal(as.numeric(a$x[, , "diffuse"]), as.numeric(b$x[, , "diffuse"]))
})

test_that("a named-vector weighting reweights Bulk as expected", {
  mrLocalEnv("CAPMF")
  # Fixture: DEU 2020 Electricity = mean(E_MBI 5, E_NMBI 3) = 4; Industry = 2.
  # Equal Bulk = 3; with weights elec:ind = 3:1 → (3*4 + 1*2)/(3+1) = 3.5.
  w <- c(elec = 3, ind = 1, buildings = 1, transport = 1)
  res <- suppressWarnings(suppressMessages(
    calcPolicyStringency(minCoverage = FALSE, weighting = w)))
  expect_equal(as.numeric(res$x["DEU", 2020, "bulk"]), 3.5)
})

test_that("a magpie of equal weights reproduces the equal-weight Bulk (invariance)", {
  mrLocalEnv("CAPMF")
  four <- suppressWarnings(suppressMessages(
    calcPolicyStringency(minCoverage = FALSE, sectorResolution = "four")))
  w <- four$x
  w[, , ] <- 1                                   # equal weights over the four sectors
  getNames(w) <- c(Electricity = "elec", Industry = "ind",
                   Buildings = "buildings", Transport = "transport")[getNames(w)]
  eq <- suppressWarnings(suppressMessages(calcPolicyStringency(minCoverage = FALSE)))
  wt <- suppressWarnings(suppressMessages(
    calcPolicyStringency(minCoverage = FALSE, weighting = w)))
  expect_equal(as.numeric(wt$x["DEU", 2020, "bulk"]),
               as.numeric(eq$x["DEU", 2020, "bulk"]))
})

test_that("an invalid weighting errors clearly", {
  mrLocalEnv("CAPMF")
  expect_error(                                            # not one of equal/ghg/gdp
    suppressWarnings(suppressMessages(
      calcPolicyStringency(minCoverage = FALSE, weighting = "nonsense"))),
    "should be|must be"
  )
  expect_error(                                            # unnamed numeric vector
    suppressWarnings(suppressMessages(
      calcPolicyStringency(minCoverage = FALSE, weighting = c(1, 2)))),
    "must be"
  )
})

test_that("weighting='ghg' routes through the data layer (computeSectorWeights)", {
  mrLocalEnv("CAPMF")
  # In the fixture env there is no EDGAR emissions source, so it must error loudly
  # from the data layer rather than silently mis-weight (documents the wiring).
  expect_error(
    suppressWarnings(suppressMessages(
      calcPolicyStringency(minCoverage = FALSE, weighting = "ghg"))),
    "computeSectorWeights|source|EDGAR|resolve"
  )
})

test_that("weighting='fe' and 'gdp' route through their own data-layer sources", {
  mrLocalEnv("CAPMF")
  # "fe" is the explicit final-energy activity proxy (the pre-2026-07-13 "ghg"
  # source, honestly renamed); "gdp" needs the OECD value-added-by-activity reader.
  expect_error(
    suppressWarnings(suppressMessages(
      calcPolicyStringency(minCoverage = FALSE, weighting = "fe"))),
    "computeSectorWeights|source|final-energy|resolve"
  )
  expect_error(
    suppressWarnings(suppressMessages(
      calcPolicyStringency(minCoverage = FALSE, weighting = "gdp"))),
    "computeSectorWeights|source|OECDValueAdded|resolve"
  )
})
