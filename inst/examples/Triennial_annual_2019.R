devtools::install_github("nwfsc-assess/VASTWestCoast")
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
  # Use extrapolation to reduce extrapolation footprint
  # to that sampled by the Triennial survey
  "extrapolation" = "propInTriennial",
  "Passcondition" = FALSE)

downloaddir <- getwd()

test <- VAST_condition(
  conditiondir = downloaddir,
  settings = Sim_Settings, spp = Sim_Settings$Species,
  datadir = downloaddir,
  overdispersion = NULL)
VAST_diagnostics(downloaddir)
