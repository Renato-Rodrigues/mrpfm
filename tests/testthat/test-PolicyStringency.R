test_that("calcPolicyStringency builds bulk/diffuse/composite outcomes without imputation", {
  mrLocalEnv("CAPMF")
  res <- suppressWarnings(suppressMessages(calcPolicyStringency(minCoverage = FALSE)))
  expect_type(res, "list")
  expect_s4_class(res$x, "magpie")
  expect_true(all(c("bulk", "diffuse", "composite") %in% getNames(res$x)))
  expect_identical(res$aggregationArguments$zeroWeight, "setNA")

  # DEU 2020: bulk = mean(ELEC 4, IND 2) = 3; diffuse = mean(BUILDINGS 3, TRANSPORT 1) = 2;
  # composite = mean(LEV1_SEC 3, LEV1_CROSS_SEC 2, LEV1_INT 1) = 2
  expect_equal(as.numeric(res$x["DEU", 2020, "bulk"]), 3.0)
  expect_equal(as.numeric(res$x["DEU", 2020, "diffuse"]), 2.0)
  expect_equal(as.numeric(res$x["DEU", 2020, "composite"]), 2.0)
  expect_equal(as.numeric(res$x["DEU", 2019, "bulk"]), 2.0)
  expect_equal(as.numeric(res$x["FRA", 2020, "bulk"]), 4.0)
  expect_equal(as.numeric(res$x["FRA", 2020, "diffuse"]), 3.0)
  expect_equal(as.numeric(res$x["FRA", 2020, "composite"]), 3.0)

  # ISR 2020 has only the Electricity sector index: bulk = mean of available children = 2
  expect_equal(as.numeric(res$x["ISR", 2020, "bulk"]), 2.0)
  expect_equal(as.numeric(res$x["ISR", 2020, "diffuse"]), 2.0)

  # Missing outcomes are NEVER imputed: they are encoded as value 0 with weight 0
  # (the aggregation device) — CRI has no Buildings/Transport data.
  expect_equal(as.numeric(res$x["CRI", 2020, "diffuse"]), 0)
  expect_equal(as.numeric(res$weight["CRI", 2020, "diffuse"]), 0)
  # USA has no sector data at all: zero weight everywhere.
  expect_true(all(as.numeric(res$weight["USA", , ]) == 0))
  # Data-bearing rows carry positive weight.
  expect_gt(as.numeric(res$weight["DEU", 2020, "bulk"]), 0)
})

test_that("calcPolicyStringency rejects unknown source and warns on the simulated expanded stub", {
  mrLocalEnv("CAPMF")
  expect_error(calcPolicyStringency(source = "bogus"), "unknown source")
  expect_warning(
    try(suppressMessages(calcPolicyStringency(source = "expanded", minCoverage = FALSE)), silent = TRUE),
    "simulated stub"
  )
})
