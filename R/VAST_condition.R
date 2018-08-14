#' Condition an operating model based on fits to empirical data
#'
#' @param conditiondir A \code{file.path}, either full or relative, that will be used
#' to save the results. The directory will be created if it doesn't already exist.
#' @param settings A list of settings used to run the spatiotemporal model.
#' @param spp A character value that includes the survey acronym and the species name
#' in latin with all words separated by an underscore.
#' For example, \code{"WCGBTS_Sebastes_crameri"}.
#' @param datadir The directory where the database is stored.
#' Also, the kmean information specific to the given survey
#' used to collect the data will be saved inside \code{datadir} in a folder named
#' the same as the leading characters of \code{spp}. For example, \code{"WCGBTS"} for a
#' \code{spp = "WCGBTS_Sebastes_crameri"}, leading to
#' \code{file.path(datadir, "WCGBTS_Sebastes_crameri")}. The character value for the
#' directory should not have a trailing separator.
#' @param overdisperion A vector of overdispersion parameters to include
#' in the conditioning. The default is \code{NULL} and will enable the determination
#' of overdispersion from the \code{spp} based on its survey. If the survey occurs in
#' Alasksa (i.e., EBSBTS) then overdispersion will be based on the default value in
#' \code{\link[VAST]{Data_Fn}}of \code{c("eta1" = 0, "eta2" = 0)}. If the survey occurs
#' of the US West Coast, then overdispersion will be modelled for both components using a
#' vessel-year effect labeled delta.
#'
#' @return Nothing is returned by the function, but the function saves two \code{.RData}
#' structures to the disk in the code{conditiondir}.
#' @author Kelli Faye Johnson
#' @export
#'
VAST_condition <- function(conditiondir, settings, spp,
  datadir, overdisperion = NULL) {
  # Start the OM
  if (!is.list(settings)) stop("settings must be a list")
  survey <- strsplit(spp, "_")[[1]][1]
  if (!survey %in% c("EBSBTS", "WCGBTS")) stop("Survey must be EBSBTS or WCGBTS")
  if (!file.exists(datadir)) stop("The datadir, ", datadir, ", doesn't exist.")

  if (is.null(overdisperion)) {
    overdispersion <- switch(survey,
      EBSBTS = c("eta1" = 0, "eta2" = 0),
      WCGBTS = c("Delta1" = 1, "Delta2" = 1))
  }

  dir.create(conditiondir, showWarnings = FALSE, recursive = TRUE)
  kmeandir <- file.path(datadir, survey)
  dir.create(kmeandir, showWarnings = FALSE, recursive = TRUE)

  # Make the data work for VAST
  Database <- FishData::download_catch_rates(
    survey = survey,
    species_set = gsub("_", " ", gsub("[A-Z]{3}BTS_", "", spp)),
    # species_set = 25,
    error_tol = 0.01, localdir = paste0(datadir, .Platform$file.sep))
  # Make the vessel column as a vessel-year entry
  if ("Vessel" %in% names(Database)) {
    Database$Vessel <- as.factor(
      paste(Database$Vessel, Database$Year, sep = "_"))
  } else {
    Database <- cbind(Database, "Vessel" = 1)
  }
  Database <- ThorsonUtilities::rename_columns(
    Database[, c("Sci", "Wt", "Year", "Long", "Lat", "Vessel")],
    newname = c("Sci", "Catch_KG", "Year", "Lon", "Lat", "Vessel"))
  # WCGBTS and all AFSC surveys are in KG/Hectare
  Database <- cbind(Database, "AreaSwept_km2" = 0.01)
  Database <- na.omit(Database)
  save(Database, file = file.path(conditiondir, "DatabaseSave.RData"))

  info <- VAST_setup(data = Database,
    dir = kmeandir,
    regionacronym = survey,
    strata = settings$strata.limits,
    nknots = settings$nknots)
  save(info, conditiondir, settings, spp, datadir, overdisperion, Database,
    file = file.path(conditiondir, "setup.RData"))

  # todo: I don't think I need these here, but the code is useful for other functions
  # Plot settings
  # Year_Set <- seq(min(Database[, "Year"]), max(Database[, "Year"]))
  # Years2Include <- which(Year_Set %in% sort(unique(Database[, "Year"])))

  VAST_run(datalist = info, depth = settings$depth,
    # Need to change the next three args
    overdispersion = overdispersion,
    obsmodel = settings$ObsModel,
    rundir = datadir,
    Version = settings$version, calcs = rep(0, 8),
    strata = settings$strata,
    savefile = file.path(conditiondir, "Save.RData"))

}
