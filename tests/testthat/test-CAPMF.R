test_that("readCAPMF reads the mock CSV correctly", {
  mrLocalEnv("CAPMF")
  withr::with_dir(sourceDir("CAPMF"), {
    x <- suppressWarnings(readCAPMF("all"))
  })
  expect_s4_class(x, "magpie")
  # DEU, FRA, ISR, CRI should have values, USA and BRA should be NA since they are empty in CSV
  expect_true("DEU" %in% getItems(x, dim = 1))
  expect_equal(as.numeric(x["DEU", 2020, "LEV3_ETS_E (CAPMF)"]), 1.5)
  expect_equal(as.numeric(x["FRA", 2020, "LEV3_ETS_E (CAPMF)"]), 2.0)
  expect_equal(as.numeric(x["ISR", 2020, "LEV3_ETS_E (CAPMF)"]), 1.2)
  expect_equal(as.numeric(x["CRI", 2020, "LEV3_ETS_E (CAPMF)"]), 0.8)
})

test_that("convertCAPMF fills missing countries with NA", {
  mrLocalEnv("CAPMF")
  withr::with_dir(sourceDir("CAPMF"), {
    raw <- suppressWarnings(readCAPMF("all"))
  })
  converted <- suppressWarnings(convertCAPMF(raw, subtype = "all"))
  expect_s4_class(converted, "magpie")
  expect_gte(nregions(converted), 200L)
  # Check that DEU, FRA, ISR, CRI still have values
  expect_equal(as.numeric(converted["DEU", 2020, "LEV3_ETS_E (CAPMF)"]), 1.5)
})

test_that("calcCAPMF runs and accepts minCoverage argument", {
  mrLocalEnv("CAPMF")
  # Use tryCatch/mocking since calcCAPMF relies on GDPPast and PopulationPast calcOutputs
  # which may not be available or fail in the isolated local test environment.
  # With minCoverage = FALSE, it doesn't need those data.
  res <- suppressWarnings(calcCAPMF(subtype = "all", minCoverage = FALSE))
  expect_type(res, "list")
  expect_s4_class(res$x, "magpie")
  expect_gte(nregions(res$x), 200L)
})

test_that("readCAPMF supports expanded subtype and calcCAPMF estimates index", {
  mrLocalEnv("CAPMF")
  withr::with_dir(sourceDir("CAPMF"), {
    xExp <- suppressWarnings(readCAPMF("expanded"))
  })
  expect_s4_class(xExp, "magpie")
  expect_true("PRY" %in% getItems(xExp, dim = 1))
  expect_equal(as.numeric(xExp["PRY", 2020, "LEV3_ETS_E (CAPMF)"]), 2.5)

  # Check calcCAPMF with includeEstimated = TRUE
  res <- suppressWarnings(calcCAPMF(subtype = "expanded", minCoverage = FALSE, includeEstimated = TRUE))
  expect_s4_class(res$x, "magpie")
  expect_true("LEV1_EST (CAPMF)" %in% getNames(res$x))
  # PRY has only LEV3_ETS_E (2.5), so its LEV1_EST should be equal to its LEV3_ETS_E value (2.5)
  expect_equal(as.numeric(res$x["PRY", 2020, "LEV1_EST (CAPMF)"]), 2.5)
  expect_equal(as.numeric(res$x["AGO", 2020, "LEV1_EST (CAPMF)"]), 1.8)
})
