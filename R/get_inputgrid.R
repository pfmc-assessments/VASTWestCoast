#' Get the Input Grid for a Given Survey
#'
#' Define the input grid for a given survey, where positive values for
#' each coordinate pair define the area in square kilometers to predict to.
#'
#' @template survey
#'
#' @author Kelli Faye Johnson
#' @export
get_inputgrid <- function(survey) {
  surveyname <- convert_survey4vast(survey)
  utils::data(california_current_grid, package = "FishStatsUtils" )
  Data_Extrap <- california_current_grid

  # Do some local fixes
  Data_Extrap[is.na(Data_Extrap[, "Cowcod"]), "Cowcod"] <- 0
  Data_Extrap[Data_Extrap[, "Lat"] < 34.5, "propInTriennial"] <- 0

  Data_Extrap[, "Area_km2"] <- 4 *
    apply(Data_Extrap[, surveyname, drop = FALSE], MARGIN = 1, FUN = min)
  Data_Extrap[, "Depth"] <- (-1000) * Data_Extrap[, "Depth_km"]
  # Local version of Include
  Data_Extrap[Data_Extrap[, "Cowcod"] != 0, "Area_km2"] <- 0
  Data_Extrap[Data_Extrap[, "Ngdc_m"] > (-35), "Area_km2"] <- 0
  Data_Extrap[, "surveyname"] <- survey

  class(Data_Extrap) <- c("data.frame", "inputgrid")
  return(Data_Extrap)
}
