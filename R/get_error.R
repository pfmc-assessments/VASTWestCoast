#' Get R objects from model with error
#'
#' Load two `RData` objects, one with the error message
#' and one with the R objects used to produce the model.
#'
#' @details
#' This function will replace all objects in your current environment with
#' those found using [base::load] if any names overlap. As recommended in
#' `?load`, it is safer to use the `envir =` parameter to load
#' into a different environment or use `attach` as a wrapper for
#' [base::load()]. We leave this up to the user and use a default of loading
#' everything into the current environment. We recommend running
#' `ls()` prior to running this function so you can see
#' which objects are new and which were previously in your environment.
#'
#' @template conditiondir
#' @param envir The environment where the data should be loaded.
#' The default, `parent.frame()` will load all objects to your current
#' environment.
#' @export
#' @author Kelli Faye Johnson
#' @return Several R objects are loaded into the specified `envir`,
#' including the model results, which is called 'out'. Additionally, a web
#' browser is opened navigating users to the code of [VAST_do]
#' to facilitate rapid rerunning of the model using the loaded objects.
#'
#' @examples
#' # Create a new environment
#' env1 <- new.env()
#' # Load objects into this environment and see the error message from fit_model
#' \dontrun{
#' get_error(conditiondir = , envir = env1)
#' ls(envir = env1)
#' get("out", envir = env1)
#' }
#' rm(env1)
get_error <- function(conditiondir, envir = parent.frame()) {
  rdataobjfn <- dir(conditiondir, pattern = "Save\\.RData", full.names = TRUE)
  load(rdataobjfn, envir = envir)
  utils::browseURL(
    "https://raw.githubusercontent.com/pfmc-assessments/VASTWestCoast/master/R/VAST_spp.R")
  message("Please follow the steps below:\n",
    "(1) Manipulate the object named settings,\n",
    "(2) Re-run 'VAST_mesh',\n",
    "(3) Re-run 'FishStatsUtils::make_settings', and\n",
    "(4) Re-run 'FishStatsUtils::fit_model'.")
}
