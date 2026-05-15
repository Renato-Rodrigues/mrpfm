test_that("readFAOLandArea returns a 1-variable magpie object", {
  mr_local_env("FAOLandArea")
  withr::with_dir(source_dir("FAOLandArea"), {
    x <- readFAOLandArea()
  })
  expect_s4_class(x, "magpie")
  expect_equal(ndata(x), 1L)
  expect_true("Land Area" %in% getNames(x))
  # year should be 2023
  expect_true("y2023" %in% getYears(x))
  # all non-NA values should be positive
  vals <- x[!is.na(x)]
  expect_true(all(vals > 0))
})

test_that("convertFAOLandArea fills missing countries with NA", {
  mr_local_env("FAOLandArea")
  withr::with_dir(source_dir("FAOLandArea"), {
    raw <- readFAOLandArea()
  })
  converted <- convertFAOLandArea(raw)
  expect_s4_class(converted, "magpie")
  # should have standard ISO3c country count
  expect_gte(nregions(converted), 200L)
})

test_that("calcFAOLandArea returns single-variable magpie with positive values", {
  mr_local_env("FAOLandArea")
  x <- calcOutput("FAOLandArea", aggregate = FALSE)
  expect_s4_class(x, "magpie")
  expect_equal(ndata(x), 1L)
  nonNa <- x[!is.na(x)]
  expect_true(length(nonNa) > 0)
  expect_true(all(nonNa > 0))
})
