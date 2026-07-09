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
  expect_error(
    suppressWarnings(suppressMessages(
      calcPolicyStringency(minCoverage = FALSE, weighting = "ghg"))),
    "should be|must be"
  )
  expect_error(
    suppressWarnings(suppressMessages(
      calcPolicyStringency(minCoverage = FALSE, weighting = c(1, 2)))),  # unnamed vector
    "must be"
  )
})
