#' @param lab.survey A character entry specifying the name of the survey that
#' gathered the data. This can be text of your choice or a single word used
#' to represent the survey name as defined in the first column of
#' \code{\link[nwfscSurvey]{createMatrix}()}.
#' See \code{\link[nwfscSurvey]{GetSurveyAbb.fn}()}
#' for how this shortened name is linked to a longer abbreviation.
#' Either the results of this function
#' based on your input or your direct input if no match is found will be
#' used as text on the plot or in the resulting caption.
#' The default is the word \code{"survey"} which will fit the bill for a
#' generic specification.
