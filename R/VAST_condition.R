#' Estimate Parameters by Fitting VAST to Empirical Data
#'
#' Fits data loaded in your R session
#' or data that is downloaded from the 
#' Northwest Fisheries Science Center datawarehouse
#' to a \code{\link[VAST]{Build_TMB_Fn}} model using
#' \code{\link[TMBhelper]{Optimize}}.
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
      NWFSC.Slope = c("Delta1" = 0, "Delta2" = 0),
      Triennial = c("Delta1" = 0, "Delta2" = 0))
  }

  dir.create(conditiondir, showWarnings = FALSE, recursive = TRUE)

  # Make the data work for VAST
  if (is.null(data)) {
    Database <- get_data(survey = survey, species = surveyspp["species"])
  } else {
    Database <- get_data(data = data)
  }

  settings[["overdispersion"]] <- overdispersion
  
  VAST_do(
    Database = Database,
    settings = settings,
    conditiondir = conditiondir,
    datadir = datadir)
  
  if (survey == "Triennial") {
    mapply(VAST_do,
      Database = list(
        Database[Database[, "Year"] <  1993, ],
        Database[Database[, "Year"] >= 1994, ],
        Database[Database[, "Year"] <  2004, ],
        Database[Database[, "Depth_m"] <=  366, ]),
      conditiondir = lapply(c("early", "late", "noNWFSC", "shallow"),
        function(x) file.path(conditiondir, x)),
      MoreArgs = list(
        settings = settings,
        datadir = datadir)
    )
    rhosettings <- settings
    rhosettings[["rho"]] <- c(0, 0, 4, 4)
    VAST_do(
      Database = Database,
      conditiondir = file.path(conditiondir, "ar1"),
      settings = rhosettings,
      datadir = datadir)
    VAST_do(
      Database = Database[Database[, "Depth_m"] <=  366, ],
      conditiondir = file.path(conditiondir, "ar1_shallow"),
      settings = rhosettings,
      datadir = datadir)
  }

}
