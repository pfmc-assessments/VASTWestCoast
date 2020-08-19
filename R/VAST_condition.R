#' Estimate Parameters by Fitting VAST to Empirical Data
#'
#' Fits data loaded in your R session
#' or data that is downloaded from the 
#' Northwest Fisheries Science Center datawarehouse
#' to a \code{\link[VAST]{make_model}} using
#' \code{\link[TMBhelper]{fit_tmb}}.
#' For more details on how the VAST model
#' is configured see the sections below.
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
#' list of necessary settings can be seen by running \code{\link{get_settings}()}, and
#' any settings that are not included in the list supplied to this argument
#' will be added at their default values. 
#' @param spp A character value that includes the survey acronym and the species name
#' in latin with all words separated by an underscore.
#' For example, \code{"WCGBTS_Sebastes_crameri"}.
#' @template overdispersion
#' @param data A data frame that can be passed to the conditioning function
#' such that no data will be downloaded. todo: document the columns that are needed.
#' @param sensitivity logical; run sensitivity analyses specific to a given survey.
#' For example, the Triennial survey can be split into two separate surveys and limited
#' to 366 m depth. 
#' 
#' @return Nothing is returned by the function, but the function saves two \code{.RData}
#' structures to the disk in the \code{conditiondir}.
#' @author Kelli Faye Johnson
#' @export
#'
VAST_condition <- function(conditiondir, settings, spp,
  overdispersion = NULL, data = NULL,
  sensitivity = TRUE) {

  if (!is.list(settings)) stop("settings must be a list")
  settings <- get_settings(settings)
  surveyspp <- get_spp(spp)
  survey <- surveyspp["survey"]

  conditiondir <- normalizePath(conditiondir, mustWork = FALSE)

  if (is.null(overdispersion)) {
    overdispersion <- switch(survey,
      WCGBTS = c("eta1" = 1, "eta2" = 1),
      AFSC.Slope = c("Delta1" = 0, "Delta2" = 0),
      NWFSC.Slope = c("Delta1" = 0, "Delta2" = 0),
      Triennial = c("Delta1" = 1, "Delta2" = 1))
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
    conditiondir = conditiondir)

  if (survey == "Triennial") {
    mapply(VAST_do,
      Database = list(
        Database[Database[, "Year"] <  1993, ],
        Database[Database[, "Year"] >= 1994, ]),
      conditiondir = lapply(c("early", "late"),
        function(x) file.path(conditiondir, x)),
      MoreArgs = list(
        settings = settings)
    )
  }
  if (survey == "Triennial" & sensitivity) {
    mapply(VAST_do,
      Database = list(
        Database[Database[, "Year"] <  2004, ],
        Database[Database[, "Depth_m"] <=  366, ]),
      conditiondir = lapply(c("noNWFSC", "shallow"),
        function(x) file.path(conditiondir, x)),
      MoreArgs = list(
        settings = settings)
    )
    rhosettings <- settings
    rhosettings[["RhoConfig"]] <- c(
      "Beta1" = 0, "Beta2" = 0,
      "Epsilon1" = 4, "Epsilon2" = 4)
    VAST_do(
      Database = Database,
      conditiondir = file.path(conditiondir, "ar1"),
      settings = rhosettings)
    VAST_do(
      Database = Database[Database[, "Depth_m"] <=  366, ],
      conditiondir = file.path(conditiondir, "ar1_shallow"),
      settings = rhosettings)
  }

}
