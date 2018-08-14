library(VASTWestCoast)
Sim_Settings <- list(
  "Species" = "WCGBTS_Anoplopoma_fimbria",
  "ObsModelcondition" = c(2, 1),
  "nknots" = 250,
  "strata" = data.frame("STRATA" = "All_areas"),
  "depth" = c("no", "linear", "squared")[1],
  "version" = "VAST_v4_0_0",
  "Passcondition" = FALSE)

downloaddir <- getwd()

VAST_condition(
  conditiondir = "d:/stockAssessment/VAST_sablefish",
  settings = Sim_Settings, spp = Sim_Settings$Species,
  datadir = downloaddir,
  overdisperion = NULL)

