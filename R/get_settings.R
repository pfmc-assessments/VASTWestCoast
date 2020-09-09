#' Add Default Settings To User List
#'
#' Get default settings for VASTWestCoast where users do not have
#' to input a single value, but if they do, then \code{get_settings}
#' will only add those values that are not already included in the list.
#'
#' @param settings A list of the current settings where each object in the
#' list must be named. Names that are in the stored list but not the list
#' provided in this argument will be added to the value returned.
#' The default value of \code{NULL} leads to a full list being returned.
#' @template verbose
#'
#' @return A list of setting for running a conditioning model or a simulation
#' with \code{\link{VAST}}.
#'
#' @author Kelli Faye Johnson
#' @export
#'
#' @examples
#' get_settings(list("nknots" = 300))
#' names(get_settings(c("yes" = 2), verbose = TRUE))
get_settings <- function(settings = NULL, verbose = FALSE) {
  if (is.vector(settings)) settings <- as.list(settings)

  Settings_all <- list(
    "ObsModelcondition" = c(2, 0),
    "nknots" = 250,
    "strata" = data.frame("STRATA" = "All_areas"),
    "Species" = "WCGBTS_Anoplopoma_fimbria",
    "version" = FishStatsUtils::get_latest_version(),
    "Passcondition" = FALSE,
    "field" = NULL,
    "rho" = NULL,
    "fine_scale" = FALSE,
    "overdispersion" = NULL)
  need <- !names(Settings_all) %in% names(settings)
  if (verbose) {
    message("Adding the following objects to settings:\n",
      paste(names(Settings_all[need]), collapse = "\n"), "\n",
      appendLF = TRUE)
  }
  Settings_all <- c(settings, Settings_all[need])

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

  RhoConfig <- c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)
  if (!is.null(Settings_all$rho)) {
    if (length(Settings_all$rho) == 1) {
      Settings_all$rho <- rep(Settings_all$rho, 4)
    }
      RhoConfig[1] <- Settings_all$rho[1]
      RhoConfig[2] <- Settings_all$rho[2]
      RhoConfig[3] <- Settings_all$rho[3]
      RhoConfig[4] <- Settings_all$rho[4]
  }
  Settings_all$RhoConfig <- RhoConfig
  Settings_all$FieldConfig <- c(Omega1 = 1, Epsilon1 = 1, Omega2 = 1, Epsilon2 = 1)

  # Overdispersion
  if (is.null(Settings_all[["overdispersion"]])) {
    Settings_all[["overdispersion"]] <- switch(
      get_spp(Settings_all$Species)["survey"],
      WCGBTS = c("eta1" = 1, "eta2" = 1),
      AFSC.Slope = c("Delta1" = 0, "Delta2" = 0),
      NWFSC.Slope = c("Delta1" = 0, "Delta2" = 0),
      # todo: think about how to write an if statement for this in VAST_do
      Triennial = c("Delta1" = 0, "Delta2" = 0))
  }

  return(Settings_all)
}
