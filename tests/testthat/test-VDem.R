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
    x <- suppressWarnings(readVDem("v2x_partipdem"))
  })
  expect_s4_class(x, "magpie")
  expect_equal(ndata(x), 1L)
  expect_equal(getNames(x), "v2x_partipdem (VDem)")
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
  result <- suppressWarnings(calcOutput("VDem", subtype = "v2x_partipdem", aggregate = FALSE))
  expect_s4_class(result, "magpie")
  expect_equal(ndata(result), 1L)
  expect_equal(getNames(result), "v2x_partipdem (VDem)")
})

test_that("calcOutput VDem subtype democracy returns the three candidate interaction weights", {
  mrLocalEnv("VDem")
  result <- suppressWarnings(calcOutput("VDem", subtype = "democracy", aggregate = FALSE))
  expect_setequal(getNames(result), c("Electoral Democracy (VDem)", "Liberal Democracy (VDem)",
                                      "Freedom of Expression (VDem)"))
  expect_true(all(result >= 0 & result <= 1, na.rm = TRUE))
})

test_that("calcOutput VDem subtype accountabilityOverall returns the overall accountability index", {
  mrLocalEnv("VDem")
  result <- suppressWarnings(calcOutput("VDem", subtype = "accountabilityOverall", aggregate = FALSE))
  expect_equal(getNames(result), "Overall Accountability (VDem)")
  expect_true(all(result >= 0 & result <= 1, na.rm = TRUE))
  # the default set is unchanged
  all4 <- suppressWarnings(calcOutput("VDem", aggregate = FALSE))
  expect_false("Overall Accountability (VDem)" %in% getNames(all4))
  expect_equal(ndata(all4), 4L)
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
