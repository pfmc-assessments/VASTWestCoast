#' Get the input grid for a given survey
#'
#' Define the input grid for a given survey, where positive values for
#' each coordinate pair define the area in square kilometers to predict to.
#'
#' @details Grid cells within the cowcod conservation area
#' denoted by positive values in the `Cowcod` column and grid cells
#' with a depth less than 35 m are removed from the prediction area
#' for all surveys. The remaining grid cells are chosen for inclusion in
#' the predicted area based on the `"prop..."` column that is relevant
#' to the survey of interest. See [convert_survey4vast()] for
#' column names for a given survey.
#' @template survey
#' @template range.depth
#' @template range.lat
#'
#' @author Kelli F. Johnson
#' @export
#' @return A data frame of grid points with associated metadata for each
#' box. The data frame is an augmented version of the california current
#' data frame available in the \pkg{FishStatsUtils} package.
#' The most relevant columns will be those for Latitude (Lat),
#' Longitude (Lon), Depth (in meters), and Area_km2.
#' For Area_km2, a column created by this function,
#' only grid cells that pertain to the survey of interest will
#' have positive values. These grid cells with positive values will be
#' included in the prediction area for the results.
#'
get_inputgrid <- function(survey,
  range.depth = c(-35, -Inf), range.lat = c(-Inf, Inf)) {

  surveyname <- convert_survey4vast(survey)
  utils::data(california_current_grid, package = "FishStatsUtils")
  # Change the name to Data_Extrap as in FishStatsUtils to make things
  # more intuitive and searchable
  Data_Extrap <- california_current_grid

  # Do some local fixes
  Data_Extrap[is.na(Data_Extrap[, "Cowcod"]), "Cowcod"] <- 0
  Data_Extrap[Data_Extrap[, "Lat"] < 34.5, "propInTriennial"] <- 0

  Data_Extrap[, "Area_km2"] <- 4 *
    apply(Data_Extrap[, surveyname, drop = FALSE], MARGIN = 1, FUN = min)
  Data_Extrap[, "Depth"] <- (-1000) * Data_Extrap[, "Depth_km"]
  # Local version of Include
  Data_Extrap[Data_Extrap[, "Cowcod"] != 0, "Area_km2"] <- 0
  Data_Extrap[Data_Extrap[, "Ngdc_m"] > range.depth[1], "Area_km2"] <- 0
  Data_Extrap[Data_Extrap[, "Ngdc_m"] < range.depth[2], "Area_km2"] <- 0
  Data_Extrap[Data_Extrap[, "Lat"] < range.lat[1], "Area_km2"] <- 0
  Data_Extrap[Data_Extrap[, "Lat"] > range.lat[2], "Area_km2"] <- 0

  return(Data_Extrap)
}
