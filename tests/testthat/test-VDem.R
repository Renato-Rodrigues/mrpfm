test_that("readVDem returns 4 indicators for subtype='all'", {
  mrLocalEnv("VDem")
  withr::with_dir(sourceDir("VDem"), {
    x <- suppressWarnings(readVDem("all"))
  })
  expect_s4_class(x, "magpie")
  expect_equal(ndata(x), 4L)
  expectedNames <- c(
    "Rule of Law (VDem)", "Vertical Accountability (VDem)",
    "Horizontal Accountability (VDem)", "Diagonal Accountability (VDem)"
  )
  expect_true(all(expectedNames %in% getNames(x)))
  expect_true(nregions(x) > 0)
})

test_that("readVDem returns 1 predefined indicator with correct label", {
  mrLocalEnv("VDem")
  withr::with_dir(sourceDir("VDem"), {
    x <- suppressWarnings(readVDem("v2x_rule"))
  })
  expect_s4_class(x, "magpie")
  expect_equal(ndata(x), 1L)
  expect_equal(getNames(x), "Rule of Law (VDem)")
})

test_that("readVDem returns arbitrary indicator with code-based label", {
  mrLocalEnv("VDem")
  withr::with_dir(sourceDir("VDem"), {
    x <- suppressWarnings(readVDem("v2x_polyarchy"))
  })
  expect_s4_class(x, "magpie")
  expect_equal(ndata(x), 1L)
  expect_equal(getNames(x), "v2x_polyarchy (VDem)")
})

test_that("readVDem rejects unknown column with helpful error", {
  mrLocalEnv("VDem")
  withr::with_dir(sourceDir("VDem"), {
    expect_error(readVDem("v2x_nonexistent"), "not found in CSV")
  })
})

test_that("convertVDem fills missing countries with NA", {
  mrLocalEnv("VDem")
  withr::with_dir(sourceDir("VDem"), {
    raw <- suppressWarnings(readVDem("v2x_rule"))
  })
  converted <- suppressWarnings(convertVDem(raw, subtype = "v2x_rule"))
  expect_s4_class(converted, "magpie")
  expect_gte(nregions(converted), 200L)
})

test_that("calcOutput VDem returns 4 indicators with correct unit", {
  mrLocalEnv("VDem")
  result <- suppressWarnings(calcOutput("VDem", aggregate = FALSE))
  expect_s4_class(result, "magpie")
  expect_equal(ndata(result), 4L)
  expectedNames <- c(
    "Rule of Law (VDem)", "Vertical Accountability (VDem)",
    "Horizontal Accountability (VDem)", "Diagonal Accountability (VDem)"
  )
  expect_true(all(expectedNames %in% getNames(result)))
})

test_that("calcOutput VDem subtype returns single arbitrary indicator", {
  mrLocalEnv("VDem")
  result <- suppressWarnings(calcOutput("VDem", subtype = "v2x_polyarchy", aggregate = FALSE))
  expect_s4_class(result, "magpie")
  expect_equal(ndata(result), 1L)
  expect_equal(getNames(result), "v2x_polyarchy (VDem)")
})

test_that("listVDemIndicators returns v2x codes from CSV header", {
  mrLocalEnv("VDem")
  indicators <- listVDemIndicators()
  expect_type(indicators, "character")
  expect_true(all(grepl("^v2", indicators)))
  expect_true("v2x_rule" %in% indicators)
  expect_true("v2x_corr" %in% indicators)
  expect_true(length(indicators) >= 5L)
})
