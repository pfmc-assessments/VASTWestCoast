#' Estimate parameters by fitting VAST to empirical data
#'
#' Fits data loaded in your R session or data that is downloaded from the
#' Northwest Fisheries Science Center data warehouse
#' to a [VAST::make_model] using [TMBhelper::fit_tmb].
#' See the sections below for more details on how the model is configured.
#'
#' @section Pass:
#' The US West Coast has operated using two passes of the survey area, where
#' each pass is done during a different time of year. If you think that migration
#' happens temporally and this migration will affect spatial density, then you
#' should include pass as a covariate. The default is to ignore pass, but I
#' presume that many US West Coast users will want to change this using the
#' `settings` argument.
#'
#' @template conditiondir
#' @param settings A list of settings used to run the spatiotemporal model. The full
#' list of necessary settings can be seen by running [get_settings()], and
#' any settings that are not included in the list supplied to this argument
#' will be added at their default values.
#' @param spp A character value that includes the survey acronym and the species name
#' in latin with all words separated by an underscore.
#' For example, `"WCGBTS_Sebastes_crameri"`.
#' @template overdispersion
#' @param data A data frame that can be passed to the conditioning function
#' such that no data will be downloaded. todo: document the columns that are needed.
#' @param sensitivity logical; run sensitivity analyses specific to a given survey.
#' For example, the Triennial survey can be split into two separate surveys and limited
#' to 366 m depth.
#' @param anistropy Boolean, defaults to FALSE, whether to include anistropitc covariance

#' @template compiledir
#'
#' @return Nothing is returned by the function, but the function saves `.RData`
#' files to the disk in `conditiondir`.
#' @author Kelli Faye Johnson
#' @export
#' @seealso See [get_settings()] for a list of default settings. Anything not
#' included in your own list supplied to the settings argument will be taken from this
#' default list.
#'
#' @examples
#' \dontrun{
#' # Use the default settings to run the model for WCGBTS - sablefish
#' VAST_condition(conditiondir = getwd(), settings = get_settings(),
#'   spp = settings$Species, sensitivity = FALSE)
#' }
#'
VAST_condition <- function(conditiondir, settings, spp,
  overdispersion, data = NULL,
  sensitivity = TRUE, compiledir = conditiondir, anisotropy=FALSE) {

  if (!is.list(settings)) stop("settings must be a list")
  settings <- get_settings(settings)
  surveyspp <- get_spp(spp)
  survey <- surveyspp["survey"]

  conditiondir <- normalizePath(conditiondir, mustWork = FALSE)

  if (!missing(overdispersion)) {
    stop("'overdispersion' is deprecated,\nplease place overdispersion in",
      " the settings list.")
  }

  dir.create(conditiondir, showWarnings = FALSE, recursive = TRUE)
  dir.create(compiledir, showWarnings = FALSE, recursive = TRUE)

  # Make the data work for VAST
  if (is.null(data)) {
    Database <- get_data(survey = survey, species = surveyspp["species"])
  } else {
    Database <- get_data(data = data)
  }

  check <- VAST_do(
    Database = Database,
    settings = settings,
    conditiondir = conditiondir,
    compiledir = compiledir, anisotropy=anisotropy, anisotropy=anisotropy)

  if (survey == "Triennial" & sensitivity) {
    #### early
    #todo: make the prediction map smaller to the spatial
    # footprint that is covered
    check <- VAST_do(
      Database = Database[Database[, "Year"] <  1993, ],
      conditiondir = paste(conditiondir, "early", sep = "_"),
      settings = settings,
      compiledir = compiledir, anisotropy=anisotropy, anisotropy=anisotropy)

    #### late
    check <- VAST_do(
      Database = Database[Database[, "Year"] >= 1994, ],
      conditiondir = paste(conditiondir, "late", sep = "_"),
      settings = settings,
      compiledir = compiledir, anisotropy=anisotropy, anisotropy=anisotropy)

    #todo: do a better job of integrating these two surveys into a single survey

    #### Shallow
    #todo: make the prediction map smaller to the spatial
    # footprint that is covered
    #todo: only run this if less than a certain percentage of positive
    # tows are in deep waters?
    shallsettings <- settings
    shallsettings[["strata"]][, "deep_border"] <- 366
    check <- VAST_do(
      Database = Database,
      conditiondir = paste(conditiondir, "shallow", sep = "_"),
      settings = shallsettings,
      compiledir = compiledir, anisotropy=anisotropy)

    #### No NWFSC suvey in 2004
    # todo: decide if we should be running this one
    # maybe we should have a different catchability or something instead
    # At least for sablefish and some other species,
    # a vessel-year effect is not significant, so I don't see why it would
    # be for a single year here.
    check <- VAST_do(
      Database = Database[Database[, "Year"] <  2004, ],
      conditiondir = paste(conditiondir, "noNWFSC", sep = "_"),
      settings = settings,
      compiledir = compiledir, anisotropy=anisotropy)

    #### Run AR(1) structure for Triennial b/c of missing years
    rhosettings <- settings
    rhosettings[["RhoConfig"]] <- c(
      "Beta1" = 0, "Beta2" = 0,
      "Epsilon1" = 4, "Epsilon2" = 4)
    check <- VAST_do(
      Database = Database,
      conditiondir = paste(conditiondir, "ar4", sep = "_"),
      settings = rhosettings,
      compiledir = compiledir, anisotropy=anisotropy)
    # Only run random walk if AR(1) structure cannot be estimated
    # i.e., parameter is more than likely going to one
    if (any(grepl("simpleError", class(check)))) {
      rhosettings[["RhoConfig"]][3:4] <- 2
      check <- VAST_do(
        Database = Database,
        conditiondir = paste(conditiondir, "ar2", sep = "_"),
        settings = rhosettings,
        compiledir = compiledir, anisotropy=anisotropy)
    }
  }

}
