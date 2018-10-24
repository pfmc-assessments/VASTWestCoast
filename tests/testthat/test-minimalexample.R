context("Testing minimal example")

wd.old <- getwd()
temp_path <- file.path(tempdir(), "eg")
dir.create(temp_path, showWarnings = FALSE)
setwd(temp_path)
version <- tail(gsub(".cpp", "", 
    dir(system.file("executables", package = "VAST"))), 1)

Sim_Settings <- list(
  "Species" = "WCGBTS_Anoplopoma_fimbria",
  "ObsModelcondition" = c(2, 1),
  "nknots" = 250,
  "strata" = data.frame("STRATA" = "All_areas"),
  "depth" = c("no", "linear", "squared")[1],
  "version" = version,
  "Passcondition" = FALSE)

test_that("Testing database is saved in VAST_condition", {
  skip_on_cran()
  test <- VAST_condition(
    conditiondir = temp_path,
    settings = Sim_Settings, spp = Sim_Settings$Species,
    datadir = temp_path,
    overdisperion = NULL)
  expect_true(file.exists("Save.RData"))
  e1 <- new.env()
  base::load(file = "Save.RData", envir = e1)
  newnames <- ls(envir = e1)
  expect_true(all(newnames %in%
    c("AIC", "error", "Index", "Obj", "Opt", "ParHat",
      "Report", "sdreport", "tables", "TmbData", "TmbList")),
    info = "names in the file saved after conditioning changed")
  expect_equivalent(get("AIC", envir = e1), 56879,
    tolerance = 1, info = "checking the AIC of the sablefish run")
})

setwd(wd.old)
dyn.unload(getLoadedDLLs()[[
  grep(version, names(getLoadedDLLs()))]][["path"]])

unlink(temp_path, recursive = TRUE)
