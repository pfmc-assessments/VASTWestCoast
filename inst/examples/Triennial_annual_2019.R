devtools::install_github("pfmc-assessments/VASTWestCoast")
library(VASTWestCoast)

Sim_Settings <- list(
  "Species" = "Triennial_Anoplopoma_fimbria",
  # lognormal
  "ObsModelcondition" = c(1, 0),
  # gamma
  # "ObsModelcondition" = c(2, 0),
  # Poisson link
  # "ObsModelcondition" = c(2, 1),
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
mapply(VAST_diagnostics, 
  c(downloaddir, list.dirs(downloaddir)))