#' Condition an operating model based on fits to empirical data
#'
#' Fits \code{data} stored on your computer or from the cloud
#' to a \code{\link[VAST]{Build_TMB_Fn}} model using
#' \code{\link[TMBhelper]{Optimize}} as specified using the available
#' arguments.
#' The function was specifically written as a wrapper for
#' \code{\link[VAST]{Data_Fn}} and \code{\link[VAST]{Build_TMB_Fn}}
#' to be used by scientists at the Northwest Fisheries Science Center.
#' For more details on how \code{\link[VAST]{Build_TMB_Fn}}
#' is configured see the sections below.
#'
#' @section Overdispersion:
#' Overdispersion controls the number of catchability factor for each
#' model component. The default is to ignore catchability, but for the
#' US West Coast we assume a vessel-year effect.
#' \enumerate{
#'   \item Alaska (i.e., EBSBTS) where overdispersion will be the default
#' value in \code{\link[VAST]{Data_Fn}}of \code{c("eta1" = 0, "eta2" = 0)}.
#'   \item US West Coast (i.e., WCGBTS) where overdispersion will be modelled
#' for both components using a vessel-year effect labeled delta
#' (i.e., \code{c("Delta1" = 1, "Delta2" = 1)}).
#' }
#'
#' @section Pass:
#' The US West Coast has operated using two passes of the survey area, where
#' each pass is done during a different time of year. If you think that migration
#' happens temporally and this migration will affect spatial density, then you
#' should include pass as a covariate. The default is to ignore pass, but I
#' presume that many US West Coast users will want to change this using the
#' \code{settings} argument.
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
#' of overdispersion from the \code{spp} based on its survey.
#' See the section below on Overdispersion above for more details.
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
  if (!survey %in% c("EBSBTS", "WCGBTS", "WCGOP")) {
    stop("Survey must be EBSBTS or WCGBTS")
  }
  if (survey == "WCGOP" & is.null(data)) {
    stop("Must supply data when the survey is WCGOP")
  }
  if (!file.exists(datadir)) stop("The datadir, ", datadir, ", doesn't exist.")

  if (is.null(overdisperion)) {
    overdispersion <- switch(survey,
      EBSBTS = c("eta1" = 0, "eta2" = 0),
      WCGBTS = c("Delta1" = 1, "Delta2" = 1),
      WCGOP = c("eta1" = 0, "eta2" = 0))
  }

  dir.create(conditiondir, showWarnings = FALSE, recursive = TRUE)
  kmeandir <- file.path(datadir, survey)
  dir.create(kmeandir, showWarnings = FALSE, recursive = TRUE)

  # Make the data work for VAST
  if (is.null(data)) {
    # todo: add ability to subset the years
    if (survey == "EBSBTS") {
      Database <- FishData::download_catch_rates(
        survey = survey,
        species_set = gsub("_", " ", gsub("[A-Z]{3}BTS_", "", spp)),
        # species_set = 25,
        error_tol = 0.01, localdir = paste0(datadir, .Platform$file.sep))
      Database <- ThorsonUtilities::rename_columns(
        Database[, c("Sci", "Wt", "Year", "Long", "Lat", "TowID")],
        newname = c("Sci", "Catch_KG", "Year", "Lon", "Lat", "TowID"))
    }
    if (survey == "WCGBTS") {
      Database <- JRWToolBox::dataWareHouseTrawlCatch(
        species =  paste(strsplit(settings$Species, "_")[[1]][2:3], collapse = " "),
        project = switch(survey, WCGBTS = "WCGBTS.Combo", AFSC = "AFSC.Shelf"), 
        verbose = FALSE)
      Database$Sci <- Database$Scientific_Name
      Database$Lon <- Database$Longitude_dd
      Database$Lat <- Database$Latitude_dd
      Database$Catch_KG <- Database$Total_sp_wt_kg
      Database$AreaSwept_km2 <- Database$Area_Swept_ha / 100
    }
    # Groundfish Triennial Shelf Survey
    if (survey == "AFSC") {
      Test <- JRWToolBox::dataWareHouseTrawlCatch("Sebastes flavidus", 
        verbose = TRUE, project = "AFSC.Shelf")
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
    Version = settings$version,
    strata = settings$strata,
    pass = settings$Passcondition,
    savefile = file.path(conditiondir, "Save.RData"))

}
