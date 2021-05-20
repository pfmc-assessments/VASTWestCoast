#' Run \pkg{VAST} models for a species sampled by west coast surveys
#'
#' [VAST_spp] is the highest-level function in `VASTWestCoast`.
#' This function runs \pkg{VAST} for each available data set in the
#' [NWFSC data warehouse](https://www.webapp.nwfsc.noaa.gov/data)
#' and is a wrapper for [VAST_condition()]; the function
#' can take hours to run.
#'
#' @details
#' [VAST_spp()] is the highest-level function available in `VASTWestCoast`.
#' It will run combinations of data sets and model configurations that are
#' seen as standard for a given species.
#' The input argument `dist` is the only argument available to the user to
#' change which models are run. If you require more user functionality, e.g.,
#' want to run just a single model or a single survey please see
#' [VAST_do()] or [VAST_condition()], respectively.
#' By default, a model with a gamma distribution for positive catch rates and a
#' model with a lognormal distribution for positive catch rates are both run.
#' Though this can be changed by altering `dist`.
#'
#' Resulting folders saved to the disk are structured hierarchically
#' based on the species name. Inside the main folder, in `dir`,
#' will be a folder called data that contains one folder for each VAST run.
#' VAST runs are named according to the survey that collected the data and the
#' distribution used for positive catch rates.
#' This structure is thought to match that used by most assessment authors for
#' a single-species stock assessment.
#' All R objects are saved in `'Save.RData'`
#' so you can access them to rerun a model or manipulate the output.
#'
#' This function, as well as others within VASTWestCoast, rely heavily on
#' \pkg{nwfscSurvey} to find out more information about the species
#' of interest, get the necessary data, and set various inputs to VAST.
#' * Find surveys of interest in the data warehouse.
#' [VAST_spp()] runs models in the following order:
#'   * NWFSC Groundfish Slope Survey,
#'   * NWFSC West Coast Groundfish Bottom Trawl Survey,
#'   * AFSC Slope Survey, and
#'   * Triennial Survey.
#' * Find the strata for the species.
#' * Run [VAST_condition()] for each survey x distribution combo.
#'
#' @param dir An existing directory where you want the new folder that is created
#' by \code{VAST_spp} to be saved. Here, \code{VAST_spp} will create a folder with
#' its name based on \code{species} input argument,
#' then this folder will be populated with a folder called data.
#' Inside data, there will be individual folders that contain VAST fits.
#' @param species A common name for the given species of interest. You can use
#' spaces as separators or underscores. See the example below for
#' Pacific ocean perch.
#' @param dist A vector of distributional assumptions that you want to include
#' for the positive catch-rate model. Options are listed in the default call.
#' Please contact the package maintainer if you wish to use other distributions
#' because names other than those listed will not work.
#' @param anistropy Boolean, defaults to FALSE, whether to include anistropitc covariance
#' @param vessel_re Boolean, defaults to FALSE, whether to include vessel random effects
#'
#' @author Kelli Faye Johnson, w/Eric Ward's edits
#' @export
#' @return Nothing is returned from this function. Called functions save
#' information (e.g., plots and RData files) to the disk for viewing, further
#' research, or inclusion in reports.
#' @seealso
#' See [VAST_condition()] and [VAST_do()], in that
#' order, for how to run VAST for a survey or a single VAST model, respectively.
#' 
#' See [check_rtools()] and [check_TMB()] if you cannot
#' get this function to run because, more than likely, rtools or TMB are improperly
#' installed. This is mainly relevant for those new to R 4.0+.
#'
#' @examples
#' \dontrun{
#'
#' # Run both lognormal and gamma for sablefish
#' VAST_spp(dir = getwd(), species = "sablefish")
#'
#' # Run just the gamma for POP, either line below will work
#' VAST_spp(dir = getwd(), species = "pacific_ocean_perch", dist = "gamma")
#' VAST_spp(dir = getwd(), species = "Pacific ocean perch", dist = "gamma")
#'
#' # See a list of available species (takes a long time)
#' sppnames <- nwfscSurvey::PullSpp.fn()
#' # For Pacific ocean perch, you can use any of these options
#' sppnames[grep("ocean perch", sppnames[, "common"]), ]
#' # Vector of all available skates
#' grep("skate", sppnames[, "common"], value = TRUE)
#' }
#'VAST_spp_combo(dir=getwd(), species="sablefish", dist="lognormal", anisotropy = FALSE)
VAST_spp_combo <- function(dir, species,
  dist = c("lognormal", "gamma"), anisotropy = FALSE, vessel_re = FALSE) {

  #### Get species, survey, and strata info
  info <- nwfscSurvey::GetSpp.fn(species)
  surveys <- rev(grep("Tri|Combo|Slope",
    nwfscSurvey::createMatrix()[, 1], value = TRUE))
  strata.limits <- convert_strata4vast(overridedepth = TRUE,
    strata = nwfscSurvey::GetStrata.fn(info[, "strata"])
  )
  # EW: only run full model, no lat strata
  strata.limits <- strata.limits[1,]
  
  sppdir <- file.path(normalizePath(dir, mustWork = FALSE),
    info[, "common_name"], "data")
  compiledir <- file.path(sppdir, "VASTcompiled")
  dir.create(sppdir, recursive = TRUE, showWarnings = FALSE)

  #### Check that VAST dll is not already loaded in a different directory
  check <- clean_unload(searchfor = "VAST", keep = species)

  ####
  # EW: only run combo survey
  survey = "NWFSC.Combo"
  for (obs in dist) {
    obs_model <- switch(obs,
      lognormal = c(1, 0),
      gamma = c(2, 0)
    )
    modeldir <- file.path(sppdir, paste(survey, obs, sep = "_"))
    settings <- list(
      Species = paste(survey, info[, "scientific_name"], sep = "_"),
      ObsModelcondition = obs_model,
      nknots = 500,
      strata = strata.limits,
      Passcondition = grepl("Combo|WCGBTS", survey),
      fine_scale = TRUE
    )
    # EW override overdispersion in VAST_condition
    if(vessel_re==FALSE) {
      settings$overdispersion <- c("eta1" = 0, "eta2" = 0)
    }
    
    test <- VAST_condition(
      conditiondir = modeldir,
      compiledir = compiledir,
      settings = settings,
      spp = settings[["Species"]],
      anisotropy = anisotropy)

    VAST_diagnostics(modeldir)
  }

  ignore <- clean_unload(searchfor = species)

  return()

}


