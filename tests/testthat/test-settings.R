context("Testing settings")

master <- list(
  "Species" = "WCGBTS_Anoplopoma_fimbria",
  "ObsModelcondition" = c(2, 1),
  "nknots" = 250,
  "strata" = data.frame("STRATA" = "All_areas"),
  "depth" = c("no", "linear", "squared")[1],
  "version" = FishStatsUtils::get_latest_version(),
  "Passcondition" = FALSE)

test_that("Testing that pass must be a logical value", {
  aa <- master
  aa$Passcondition <- 2
  testthat::expect_error(get_settings(aa))
  aa$Passcondition <- 0
  testthat::expect_false(get_settings(aa)$Passcondition)
})

rm(master)
