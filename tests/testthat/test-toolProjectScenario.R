# toolProjectScenario and toolIPFDownscale write through plain arrays since 2026-10-02 (a ~38 s and
# a ~25 s saving per coupling call). Pinned here against hand-computed values.

test_that("toolProjectScenario: history interpolated, targets reached, above-target countries kept", {
  x <- new.magpie(c("AAA", "BBB", "CCC"), c(2000, 2010, 2020), "v", fill = NA)
  x["AAA", , ] <- c(0.1, NA, 0.3)   # gap: interpolated
  x["BBB", , ] <- c(0.5, 0.5, 0.5)
  x["CCC", , ] <- c(0.9, 0.9, 0.9)  # above the target: kept
  y <- c(2000, 2005, 2020, 2060, 2100)
  out <- toolProjectScenario(x, y, mode = "global_percentile", percentile = 50,
                             convergenceYear = 2100, shape = "linear")
  expect_identical(getYears(out, as.integer = TRUE), as.integer(y))
  expect_equal(as.numeric(out["AAA", 2005, ]), 0.15)
  # target = median at 2020 = 0.5; AAA linear from 0.3 at 2020 to 0.5 at 2100
  expect_equal(as.numeric(out["AAA", 2060, ]), 0.3 + 0.5 * (0.5 - 0.3))
  expect_equal(as.numeric(out["AAA", 2100, ]), 0.5)
  expect_equal(as.numeric(out["CCC", c(2060, 2100), ]), c(0.9, 0.9))
  hold <- toolProjectScenario(x, y, mode = "constant")
  expect_equal(as.numeric(hold["AAA", c(2060, 2100), ]), c(0.3, 0.3))
})

test_that("toolIPFDownscale: country values reaggregate exactly to the regional input", {
  mapping <- data.frame(CountryCode = c("A1", "A2", "B1"), RegionCode = c("A", "A", "B"))
  prior <- new.magpie(c("A1", "A2", "B1"), 2010:2012, c("pecoal", "petotal"), fill = 0)
  prior["A1", , "pecoal"] <- 3
  prior["A2", , "pecoal"] <- 1
  prior["A1", , "petotal"] <- 6
  prior["A2", , "petotal"] <- 2
  prior["B1", , "petotal"] <- 5
  remind <- new.magpie(c("A", "B"), c(2030, 2050), c("pecoal", "petotal"), fill = 0)
  remind["A", , "pecoal"] <- c(8, 4)
  remind["A", , "petotal"] <- c(20, 16)
  remind["B", , "petotal"] <- c(10, 12)
  out <- toolIPFDownscale(prior, remind, groups = list(pe = list(vars = "pecoal", denom = "petotal")),
                          mapping = mapping, nHistYears = 3L)
  expect_equal(as.numeric(out[c("A1", "A2"), 2030, "pecoal"]), c(6, 2))   # 3:1 historical split
  agg <- function(v, t) sum(out[c("A1", "A2"), t, v])
  expect_equal(c(agg("pecoal", 2030), agg("petotal", 2030), agg("petotal", 2050)), c(8, 20, 16))
  expect_equal(as.numeric(out["B1", c(2030, 2050), "petotal"]), c(10, 12))
})
