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
    "fine_scale" = TRUE,
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

  Settings_all$RhoConfig <- get_settings_single(Settings_all$rho,
    default = c(Beta1 = 0, Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0))
  Settings_all$FieldConfig <- get_settings_single(Settings_all$field,
    default = c(Omega1 = 1, Epsilon1 = 1, Omega2 = 1, Epsilon2 = 1))

  # Overdispersion
  if (is.null(Settings_all[["overdispersion"]])) {
    Settings_all[["overdispersion"]] <- switch(
      get_spp(Settings_all$Species)["survey"],
      WCGBTS = c("eta1" = 1, "eta2" = 1),
      AFSC.Slope = c("Delta1" = 0, "Delta2" = 0),
      NWFSC.Slope = c("Delta1" = 0, "Delta2" = 0),
      # todo: think about how to write an if statement for this in VAST_do
      Triennial = c("Delta1" = 0, "Delta2" = 0),
      # provide a default to not estimate a vessel-year catchability
      c(eta1 = 0, eta2 = 0))
  }

  return(Settings_all)
}

#' Get Settings For a Single Parameter
#' @param par A vector of values that do not need to be named.
#' If a single value is provided, it will be repeated for each needed value.
#' @param default A vector of default values that will be used if
#' \code{par = NULL}. The names from this vector are always copied
#' over to the vector that is returned, so name them well.
#' @return A vector of values the same length as \code{default} and with the
#' same names as those given to \code{default}.
get_settings_single <- function(par, default) {
  if (is.null(par)) {
    return(default)
  } else {
    if (length(par) == 1) {
      par <- rep(par, length(default))
    } else {
      if (length(par) != length(default)) {
        stop("Input vector par must be of length 1 or same length as",
          " default input vector,\nwhich is ", length(default), ".",
          call. = TRUE)
      }
    }
    names(par) <-  names(default)
    return(par)
  }
}
