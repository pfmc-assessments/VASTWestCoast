#' Obtain Data for a VAST Model
#'
#' Get and clean data to use in a VAST model. The information
#' can be pulled from a data warehouse or supplied using the
#' \code{data} argument. 
#' 
#' @template survey
#' @template species
#' @param data A data frame to be cleaned rather than pulling
#' information from a warehouse.
#' 
#' @importFrom FishData download_catch_rates
#' @importFrom nwfscSurvey createMatrix PullCatch.fn
#' @export
#' 
get_data <- function(survey, species, data = NULL) {
  # todo: add ability to subset the years
  if (!is.null(data)) return(clean_data(data))

  if (survey == "WCGOP") {
    stop("Must supply data argument if survey is WCGOP.")
  }  
  if (survey == "EBSBTS") {
    # Sci Year TowID Lat Long Wt
    data <- FishData::download_catch_rates(
      survey = survey,
      species_set = species,
      error_tol = 0.01, localdir = NULL)
  }
  if (survey %in% c("WCGBTS", nwfscSurvey::createMatrix()[, 1])) {
    data <- nwfscSurvey::PullCatch.fn(
      SciName = species,
      SurveyName = switch(survey,
        WCGBTS = "NWFSC.Combo",
        survey),
      SaveFile = FALSE, Dir = NULL, verbose = FALSE)
  }
  return(clean_data(data))
}
