test_that("calcPolicyRatchetEvents detects adoptions and jumps, never imputes", {
  mrLocalEnv("CAPMF")
  res <- suppressWarnings(suppressMessages(calcPolicyRatchetEvents(jumpThreshold = 0.5)))
  expect_type(res, "list")
  expect_s4_class(res$x, "magpie")
  expect_true(all(c("Ratchet Event", "Ratchet Count", "Ratchet Intensity")
                  %in% getNames(res$x)))

  # DEU 2020: LEV3_ETS_E 0.0 -> 1.5 (adoption + jump), LEV3_TREATY flat
  expect_equal(as.numeric(res$x["DEU", 2020, "Ratchet Event"]), 1)
  expect_equal(as.numeric(res$x["DEU", 2020, "Ratchet Count"]), 1)
  expect_equal(as.numeric(res$x["DEU", 2020, "Ratchet Intensity"]), 1.5)
  # FRA 2020: ETS_E +0.4 below the 0.5 threshold, TREATY flat -> no event
  expect_equal(as.numeric(res$x["FRA", 2020, "Ratchet Event"]), 0)
  expect_equal(as.numeric(res$x["FRA", 2020, "Ratchet Count"]), 0)
  # ISR 2020: ETS_E 0.5 -> 1.2 = tightening jump (not an adoption)
  expect_equal(as.numeric(res$x["ISR", 2020, "Ratchet Event"]), 1)
  expect_equal(as.numeric(res$x["ISR", 2020, "Ratchet Intensity"]), 0.7)
  # CRI has no 2019 instrument values -> not at risk -> NA, never a fabricated 0
  expect_true(is.na(as.numeric(res$x["CRI", 2020, "Ratchet Event"])))
  # first panel year is NA by construction
  expect_true(all(is.na(as.numeric(res$x[, 2019, ]))))

  # threshold sensitivity: at 0.3 the FRA +0.4 becomes an event
  res2 <- suppressWarnings(suppressMessages(calcPolicyRatchetEvents(jumpThreshold = 0.3)))
  expect_equal(as.numeric(res2$x["FRA", 2020, "Ratchet Event"]), 1)
})

test_that("calcPolicyRatchetEvents rejects unknown source and missing levels", {
  mrLocalEnv("CAPMF")
  expect_error(calcPolicyRatchetEvents(source = "bogus"), "unknown source")
  expect_error(suppressWarnings(calcPolicyRatchetEvents(level = "LEV9")), "no 'LEV9")
})
