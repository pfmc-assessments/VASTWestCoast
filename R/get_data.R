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
#' @export
#' 
get_data <- function(survey, species, data = NULL) {
  # todo: add ability to subset the years
  if (!is.null(data)) return(clean_data(data))

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
