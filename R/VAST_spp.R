#' Run VAST Models for a Species
#' 
#' @details
#' \itemize{
#' \item Find surveys of interest in the data warehouse.
#' \item Find the strata for the species.
#' \item Run VAST_condition for each survey x distribution combo.
#' }
#' @param dir The directory where you want a folder named \code{species}
#' to be saved. The resulting folder will house all of the results from
#' VASTWestCoast in a directory called data.
#' @param species A common name for the given species of interest.
#' @param dist A vector of distributional assumptions that you want to include
#' for each species. Options are listed in the default call. Please contact
#' the package maintainer if you wish to use other distributions because names
#' other than those listed will not work.
#'
#' @author Kelli Faye Johnson
#' @export
#' @return Nothing is returned from this function. Called functions save
#' information (e.g., plots and RData files) to the disk for viewing, further
#' research, or inclusion in reports.
#'
VAST_spp <- function(dir, species,
  dist = c("lognormal", "gamma")) {

  #### Get species, survey, and strata info
  info <- nwfscSurvey::GetSpp.fn(species)
  surveys <- grep("Tri|Combo|N.+Slope",
    nwfscSurvey::createMatrix()[, 1], value = TRUE)
  strata.limits <- convert_strata4vast(overridedepth = TRUE,
    strata = nwfscSurvey::GetStrata.fn(info[, "strata"])
  )
  sppdir <- file.path(normalizePath(dir, mustWork = FALSE), species, "data")
  compiledir <- file.path(sppdir, "VASTcompiled")
  dir.create(sppdir, recursive = TRUE, showWarnings = FALSE)

  ####
  for (obs in dist) {
  for (survey in surveys) {
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

    test <- VAST_condition(
      conditiondir = modeldir,
      compiledir = compiledir,
      settings = settings,
      spp = settings[["Species"]])

    VAST_diagnostics(modeldir)
  }}

  ignore <- clean_unload(searchfor = species)

}
