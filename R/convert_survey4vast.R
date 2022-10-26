#' Convert survey name to VAST column name
#' 
#' Convert a character string that specifies the survey name in terms
#' of \pkg{nwfscSurvey} to that used in the [get_inputgrid]
#' column of the grid provided by \pkg{VAST}.
#' 
#' @template survey
#' @export
#' @author Kelli F. Johnson
#' @return A vector of character values, one for each input value, that
#' specifies the column of the grid data to use for the area sampled.
#' The default is to return the NWFSC WCGBTS name, thus even values that
#' don't pertain to a survey will return `"propInWCGBTS"`.
#' @examples
#' test <- convert_survey4vast(c("Triennial", "NWFSC.Combo"))
#' \dontshow{
#' testthat::expect_true(all(c("propInTriennial", "propInWCGBTS") == test))
#' }

convert_survey4vast <- function(survey) {
  internalfn <- function(x) {
    switch(x,
        Triennial = "propInTriennial",
        AFSC.Slope = "propInSlope98_00",
        NWFSC.Slope = "propInSlope02",
        NWFSC.Combo = "propInWCGBTS",
        WCGBTS = "propInWCGBTS",
        #default
        "propInWCGBTS")
  }
  unlist(lapply(survey, internalfn))
}
