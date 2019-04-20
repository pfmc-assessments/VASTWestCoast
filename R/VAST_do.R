#' Shortened Code for VAST
#' 
#' Major workhorse to run a VAST model
#' 
#' @param Database A data base ran through \code{\link{get_data}}.
#' @param settings A list of settings used to run the spatiotemporal model. The full
#' list of necessary settings can be seen by running \code{\link{get_settings()}}, and
#' any settings that are not included in the list supplied to this argument
#' will be added at their default values. 
#' @param conditiondir A directory, either full or relative, that will be used
#' to save the results. The directory will be created if it doesn't already exist.
#' @param datadir A directory where you want the database to be stored.
#' Also, the kmean information specific to the given survey
#' used to collect the data will be saved here in a folder named after the survey.
#' The directory should not have a trailing separator.
#'
VAST_do <- function(Database, settings, conditiondir, datadir) {
	
	spp <- settings[["Species"]]
	survey <- get_spp(spp)["survey"]
	overdispersion <- settings[["overdispersion"]]
  kmeandir <- file.path(datadir, survey)
  
  dir.create(kmeandir, showWarnings = FALSE, recursive = TRUE)
  dir.create(conditiondir, showWarnings = FALSE, recursive = TRUE)
  info <- VAST_setup(
	  data = Database,
	  dir = kmeandir,
	  survey = survey,
	  strata = settings[["strata"]],
	  nknots = settings[["nknots"]])

  VAST_run(datalist = info, 
  	depth = settings[["depth"]],
    overdispersion = overdispersion,
    obsmodel = settings[["ObsModelcondition"]],
    rundir = datadir,
    Version = settings[["version"]],
    strata = settings[["strata"]],
    pass = settings[["Passcondition"]],
    savefile = file.path(conditiondir, "Save.RData"),
    field = switch(survey, WCGOP = "IID", NULL),
    rho = settings[["rho"]],
    # calcs = rep(0, 9), # todo: make this part of settings
    comp = settings[["comp"]])
	
	save(info, conditiondir, settings, spp, datadir, overdispersion, Database,
    file = file.path(conditiondir, "setup.RData"))
	save(Database, file = file.path(conditiondir, "DatabaseSave.RData"))
	return(NULL)
}