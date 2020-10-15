context("Testing minimal example")

Sys.setenv(PATH = gsub("rtools40|rtools40\\\\mingw64", "Rtools", Sys.getenv("PATH")))

wd.old <- getwd()
temp_path <- file.path(tempdir(), "eg")
dir.create(temp_path, showWarnings = FALSE)
setwd(temp_path)

Sim_Settings <- list(
  "Species" = "WCGBTS_Anoplopoma_fimbria",
  "ObsModelcondition" = c(2, 0),
  "nknots" = 500,
  "overdispersion" = NULL,
  "strata" = data.frame("STRATA" = c("All_areas", "North", "South"),
    north_border = c(49, 36, 49),
    south_border = c(32, 32, 36),
    shallow_border = rep(55, 3),
    middle_border = rep(183, 3),
    deep_border = rep(1280, 3)),
  "Passcondition" = TRUE,
  "fine_scale" = TRUE)

test_that("Testing database is saved in VAST_condition", {
  skip_on_cran()
  data("Database", package = "VASTWestCoast")
  test <- VAST_condition(
    conditiondir = temp_path,
    data = Database,
    settings = Sim_Settings, spp = Sim_Settings$Species)
  expect_true(file.exists("Save.RData"))
  e1 <- new.env()
  base::load(file = "Save.RData", envir = e1)
  newnames <- ls(envir = e1)
  expect_true(all(c("conditiondir", "Database", "info", "out", "settings",
      "spp", "survey") %in% newnames),
    info = "names in the file saved after conditioning changed")
  expect_equivalent(
    get("out", envir = e1)$parameter_estimates$AIC,
    59641,
    tolerance = 1e-5, info = "checking the AIC of the sablefish run")
  expect_equivalent(get("out", envir = e1)$spatial_list$n_x, Sim_Settings$nknots)
  expect_equivalent(get("out", envir = e1)$years_to_plot, 1:16)
  expect_equivalent(get("out", envir = e1)$year_labels, 2003:2018)
  expect_equivalent(sum(grepl("beta",
    get("out", envir = e1)$parameter_estimates$diagnostics$Param)), 16*2)
  expect_equivalent(sum(grepl("H_input",
    get("out", envir = e1)$parameter_estimates$diagnostics$Param)), 2)
  expect_equivalent(sum(grepl("kappa",
    get("out", envir = e1)$parameter_estimates$diagnostics$Param)), 2)
  expect_equivalent(sum(grepl("_eta|epsilon|kappa",
    get("out", envir = e1)$parameter_estimates$diagnostics$Param)), 6)
  expect_equivalent(sum(grepl("Sigma",
    get("out", envir = e1)$parameter_estimates$diagnostics$Param)), 1)
  dyn.unload(TMB::dynlib(gsub("\\.dll", "", getLoadedDLLs()[[
    grep(get("out", envir = e1)$settings$Version,
      names(getLoadedDLLs()))]][["path"]])))
})

setwd(wd.old)
unlink(temp_path, recursive = TRUE)
