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
#' @param data A data frame that can be passed to the conditioning function
#' such that no data will be downloaded and the passed data frame will be used
#' instead. todo: document the columns that are needed.
#'
#' @return Nothing is returned by the function, but the function saves two \code{.RData}
#' structures to the disk in the \code{conditiondir}.
#' @author Kelli Faye Johnson
#' @importFrom JRWToolBox dataWareHouseTrawlCatch
#' @export
#'
VAST_condition <- function(conditiondir, settings, spp,
  datadir, overdisperion = NULL, data = NULL) {
  # Start the OM
  if (!is.list(settings)) stop("settings must be a list")
  settings <- get_settings(settings)
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
  if (is.null(data)) {
    # Database <- FishData::download_catch_rates(
    #   survey = survey,
    #   species_set = gsub("_", " ", gsub("[A-Z]{3}BTS_", "", spp)),
    #   # species_set = 25,
    #   error_tol = 0.01, localdir = paste0(datadir, .Platform$file.sep))
    # Database <- ThorsonUtilities::rename_columns(
    #   Database[, c("Sci", "Wt", "Year", "Long", "Lat", "Vessel")],
    #   newname = c("Sci", "Catch_KG", "Year", "Lon", "Lat", "Vessel"))
    # another way to download the data
    # todo: don't hardwire the years
    if (TRUE) {
      # Database <- JRWToolBox::WCGBTS_Combo_Catch_Wt(
      #   Species =  paste(strsplit(settings$Species, "_")[[1]][2:3], collapse = " "),
      #   YearRange = c(2003, 2017))
      # Database$Sci <- Database$Scientific_Name
      # Database$Catch_KG <- Database$Total_sp_wt_kg
      # Database$AreaSwept_km2 <- Database$Area_Swept_ha / 100
      # Database$Lon <- Database$Longitude_dd
      # Database$Lat <- Database$Latitude_dd
      #todo: change  more column names or delete this
      Database <- JRWToolBox::dataWareHouseTrawlCatch(
        YearRange = c(2003, 2017),
        Species =  paste(strsplit(settings$Species, "_")[[1]][2:3], collapse = " "),
        project = "WCGBTS.Combo")
      Database$Sci <- Database$Scientific_Name
      Database$Lon <- Database$Longitude_dd
      Database$Lat <- Database$Latitude_dd
      Database$Catch_KG <- Database$Total_sp_wt_kg
      Database$AreaSwept_km2 <- Database$Area_Swept_ha / 100
    }
    # Make the vessel column as a vessel-year entry
    if ("Vessel" %in% names(Database)) {
      Database$Vessel <- as.factor(
        paste(Database$Vessel, Database$Year, sep = "_"))
    } else {
      Database <- cbind(Database, "Vessel" = 1)
    }
    # WCGBTS and all AFSC surveys are in KG/Hectare
    # todo: check how i set this before, where I think I should have divided
    # Database <- cbind(Database, "AreaSwept_km2" = 0.01)
    # Database <- na.omit(Database)
  } else {
    Database <- data
  }
  save(Database, file = file.path(conditiondir, "DatabaseSave.RData"))

  info <- VAST_setup(data = Database,
    dir = kmeandir,
    regionacronym = survey,
    strata = settings$strata,
    nknots = settings$nknots)
  save(info, conditiondir, settings, spp, datadir, overdisperion, Database,
    file = file.path(conditiondir, "setup.RData"))

  VAST_run(datalist = info, depth = settings$depth,
    # Need to change the next three args
    overdispersion = overdispersion,
    obsmodel = settings$ObsModelcondition,
    rundir = datadir,
    Version = settings$version, calcs = rep(0, 8),
    strata = settings$strata,
    pass = settings$Passcondition,
    savefile = file.path(conditiondir, "Save.RData"))

}
