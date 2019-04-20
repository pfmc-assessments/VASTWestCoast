#' Estimate Parameters by Fitting to Empirical Data
#'
#' Fits data loaded in your R session
#' or data that is downloaded from the cloud
#' to a \code{\link[VAST]{Build_TMB_Fn}} model using
#' \code{\link[TMBhelper]{Optimize}}.
#' \code{VAST_condition} was specifically written as a wrapper for
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
#' @param conditiondir A directory, either full or relative, that will be used
#' to save the results. The directory will be created if it doesn't already exist.
#' @param settings A list of settings used to run the spatiotemporal model. The full
#' list of necessary settings can be seen by running \code{\link{get_settings()}}, and
#' any settings that are not included in the list supplied to this argument
#' will be added at their default values. 
#' @param spp A character value that includes the survey acronym and the species name
#' in latin with all words separated by an underscore.
#' For example, \code{"WCGBTS_Sebastes_crameri"}.
#' @param datadir A directory where you want the database to be stored.
#' Also, the kmean information specific to the given survey
#' used to collect the data will be saved here in a folder named after the survey.
#' The directory should not have a trailing separator.
#' @param overdispersion A vector of overdispersion parameters to include
#' in the conditioning. The default is \code{NULL} and will enable the determination
#' of overdispersion from the \code{spp} based on its survey.
#' See the section below on Overdispersion above for more details.
#' @param data A data frame that can be passed to the conditioning function
#' such that no data will be downloaded. todo: document the columns that are needed.
#'
#' @return Nothing is returned by the function, but the function saves two \code{.RData}
#' structures to the disk in the \code{conditiondir}.
#' @author Kelli Faye Johnson
#' @importFrom ThorsonUtilities rename_columns
#' @export
#'
VAST_condition <- function(conditiondir, settings, spp,
  datadir, overdispersion = NULL, data = NULL) {
  # Start the OM
  if (!is.list(settings)) stop("settings must be a list")
  settings <- get_settings(settings)
  surveyspp <- get_spp(spp)
  survey <- surveyspp["survey"]

  dir.create(datadir, showWarnings = FALSE, recursive = TRUE)

  if (is.null(overdispersion)) {
    overdispersion <- switch(survey,
      EBSBTS = c("eta1" = 0, "eta2" = 0),
      WCGBTS = c("Delta1" = 1, "Delta2" = 1),
      WCGOP = c("eta1" = 0, "eta2" = 0),
      Triennial = c("Delta1" = 0, "Delta2" = 0))
  }

  dir.create(conditiondir, showWarnings = FALSE, recursive = TRUE)
  kmeandir <- file.path(datadir, survey)
  dir.create(kmeandir, showWarnings = FALSE, recursive = TRUE)

  # Make the data work for VAST
  if (is.null(data)) {
    Database <- get_data(survey = survey, species = surveyspp["species"])
  } else {
    Database <- get_data(data = Database)
  }
  save(Database, file = file.path(conditiondir, "DatabaseSave.RData"))

  # todo: make this code better, somehow use an ifelse statement
  # or something similar so that I don't have two calls to the
  # exact same function
  setup_spatial <- "info"
  if (!is.null(settings$extrapolation)) {
    info <- VAST_setup(data = Database,
      dir = kmeandir,
      regionacronym = survey,
      surveyname = settings$extrapolation,
      strata = settings$strata,
      nknots = settings$nknots)
    if (survey == "Triennial") {
      info_early <- VAST_setup(
        data = Database[Database$Year < 1993, ],
        dir = kmeandir,
        regionacronym = survey,
        surveyname = settings$extrapolation,
        strata = settings$strata,
        nknots = settings$nknots)
      info_late <- VAST_setup(
        data = Database[Database$Year >= 1993 , ],
        dir = kmeandir,
        regionacronym = survey,
        surveyname = settings$extrapolation,
        strata = settings$strata,
        nknots = settings$nknots)
      setup_spatial <- c(setup_spatial, "info_early", "info_late")
    }
  } else {
    info <- VAST_setup(data = Database,
      dir = kmeandir,
      regionacronym = survey,
      strata = settings$strata,
      nknots = settings$nknots)
    if (survey == "Triennial") {
      info_early <- VAST_setup(
        data = Database[Database$Year < 1993, ],
        dir = kmeandir,
        regionacronym = survey,
        strata = settings$strata,
        nknots = settings$nknots)
      info_late <- VAST_setup(
        data = Database[Database$Year >= 1993 , ],
        dir = kmeandir,
        regionacronym = survey,
        strata = settings$strata,
        nknots = settings$nknots)
      setup_spatial <- c(setup_spatial, "info_early", "info_late")
    }
  }
  save(list = setup_spatial, 
    file = file.path(conditiondir, "setup_spatial.RData"))

  save(info, conditiondir, settings, spp, datadir, overdispersion, Database,
    file = file.path(conditiondir, "setup.RData"))

  VAST_run(datalist = info, depth = settings$depth,
    overdispersion = overdispersion,
    obsmodel = settings$ObsModelcondition,
    rundir = datadir,
    Version = settings$version,
    strata = settings$strata,
    pass = settings$Passcondition,
    savefile = file.path(conditiondir, "Save.RData"),
    field = switch(survey, WCGOP = "IID", NULL),
    rho = settings$rho,
    # calcs = rep(0, 9), # todo: make this part of settings
    comp = settings$comp)
  
  if (survey == "Triennial") {
    conditiondir_tri <- file.path(conditiondir, c("early", "late"))
    ignore <- mapply(dir.create, conditiondir_tri,
      MoreArgs = list(showWarnings = FALSE))
    
    info_all <- info
    Database_all <- Database
    Database <- Database_all[Database_all$Year < 1993, ]
    info <- info_early
    save(Database, 
      file = file.path(conditiondir_tri[1], "DatabaseSave.RData"))
    save(info, 
      conditiondir, settings, spp, datadir, 
      overdispersion, Database,
      file = file.path(conditiondir_tri[1], "setup.RData"))
    info <- info_late
    Database <- Database_all[Database_all$Year >= 1993, ]
    save(Database, 
      file = file.path(conditiondir_tri[2], "DatabaseSave.RData"))
    save(info, 
      conditiondir, settings, spp, datadir, 
      overdispersion, Database,
      file = file.path(conditiondir_tri[2], "setup.RData"))
    info <- info_all
    Database <- Database_all
    
    mapply(VAST_run, 
      datalist = list(info_early, info_late),
      savefile = file.path(conditiondir_tri, "Save.RData"),
      MoreArgs = list(
        depth = settings$depth,
        overdispersion = overdispersion,
        obsmodel = settings$ObsModelcondition,
        rundir = datadir,
        Version = settings$version,
        strata = settings$strata,
        pass = settings$Passcondition,
        field = switch(survey, WCGOP = "IID", NULL),
        rho = settings$rho,
        # calcs = rep(0, 9), # Use to make run estimate faster
        comp = settings$comp)
      )
  }

}
