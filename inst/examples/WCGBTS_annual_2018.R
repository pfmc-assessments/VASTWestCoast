devtools::install_github("pfmc-assessments/VASTWestCoast")
library(VASTWestCoast)
Sim_Settings <- list(
  "Species" = "WCGBTS_Anoplopoma_fimbria",
  "ObsModelcondition" = c(2, 1),
  "nknots" = 250,
  "strata" = data.frame("STRATA" = "All_areas"),
  "depth" = c("no", "linear", "squared")[1],
  "Passcondition" = FALSE)

downloaddir <- getwd()

test <- VAST_condition(
  conditiondir = downloaddir,
  settings = Sim_Settings, spp = Sim_Settings$Species,
  datadir = downloaddir,
  overdispersion = NULL)
VAST_diagnostics(downloaddir)
