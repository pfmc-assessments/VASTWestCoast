#' Check that all of the settings are in the list
#'
#' @param settings A list of the current settings where each object in the
#' list must be named. Those names that are not found in the stored list will
#' be added. The default value of \code{NULL} leads to a full list being returned.
#' @param verbose A logical value specifying if the message should be output
#' to the screen or not.
#'
#' @return A list of setting for running a conditioning model or a simulation
#' with \code{\link{VAST}}.
#'
#' @author Kelli Faye Johnson
#' @export
#'
#' @examples
#' get_settings(list("SigmaM" = 3))
#' names(get_settings(c("yes" = 2), verbose = TRUE))
get_settings <- function(settings = NULL, verbose = FALSE) {
  if (is.vector(settings)) settings <- as.list(settings)
  Settings = list(
    "beta1_mean"=0, "beta2_mean"=0,
    "beta1_slope"=0, "beta2_slope"=0,
    "beta1_sd"=0, "beta2_sd"=0,
    "Nyears"=10, "Nsamp_per_year"=600,
    "Depth1_km"=0, "Depth1_km2"=0,
    "Depth2_km"=0, "Depth2_km2"=0,
    "Dist1_sqrtkm"=0, "Dist2_sqrtkm"=0,
    "SigmaO1"=0.5, "SigmaO2"=0.5,
    "SigmaE1"=0.5, "SigmaE2"=0.5,
    "SigmaV1"=0, "SigmaV2"=0, "SigmaVY1"=0, "SigmaVY2"=0,
    "Range1"=1000, "Range2"=500,
    "SigmaM"=1,
    "Nages"=1, "M"=Inf, "K"=Inf, "Linf"=1,
    "W_alpha"=1, "W_beta"=3,
    "Selex_A50_mean"=0, "Selex_A50_sd"=0, "Selex_Aslope"=Inf )
  Settings_add <- list(
    "ObsModelEM" = c(2, 0),
    "ObsModelcondition" = c(2, 0),
    "nknots" = 250,
    "strata" = data.frame("STRATA" = "All_areas"),
    "depth" = c("no", "linear", "squared")[1],
    "Species" = "WCGBTS_Anoplopoma_fimbria",
    # Extrapolation to the entire California Current grid area
    # is not necessary. You can specify a smaller footprint here. 
    "extrapolation" = NULL, 
    "version" = FishStatsUtils::get_latest_version(),
    "changepar" = c(
      "SigmaO1", "SigmaO2", "SigmaE1", "SigmaE2",
      "Range1", "Range2"),
    "replicates" = 0,
    "Passcondition" = FALSE,
    "field" = NULL,
    "rho" = NULL,
    "comp" = FALSE)
  Settings_all <- c(Settings, Settings_add)
  need <- !names(Settings_all) %in% names(settings)
  if (verbose) {
    message("Adding the following objects to settings:\n",
      paste(names(Settings_all[need]), collapse = "\n"), "\n",
      appendLF = TRUE)
  }
  Settings_all <- c(settings, Settings_all[need])
  if (!"replicatesneeded" %in% names(Settings_all)) {
    Settings_all$replicatesneeded <- max(Settings_all$replicates)
  }

  # Pass
  if (Settings_all$Passcondition %in% c("T", 1)) {
    Settings_all$Passcondition <- TRUE
  }
  if (Settings_all$Passcondition %in% c("F", 0, F)) {
    Settings_all$Passcondition <- FALSE
  }
  if (!Settings_all$Passcondition %in% c(TRUE, FALSE)) {
    stop("Passcondition must be a logical value")
  }

  # Obs Model
  if (length(Settings_all$ObsModelcondition) != 2 |
      length(Settings_all$ObsModelcondition) != 2) {
    stop("ObsModelEM and ObsModelcondition must be vectors of two numbers")
  }

  # Version
  if (grepl("cpp", Settings_all$version)) {
    stop("Must remove .cpp from the version number of VAST that you specify")
  }

  return(Settings_all)
}
