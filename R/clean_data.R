#' Clean Database for VAST
#'
#' Many specific column names are needed for VAST but the data
#' are downloaded from several repositories, thus making cleaning
#' the data a necessary step before running VAST. This function 
#' attempts to standardize the columns to work with VAST. 
#' 
#' todo: figure out area swept for the different surveys
#' WCGBTS and all AFSC surveys are in KG/Hectare but early on
#' I used "AreaSwept_km2" = 0.01
#' todo: ensure that there are no \code{NA} values, i.e., na.omit(data)
#' 
#' @param data A data frame that you want to use for a 
#' VAST model.
#' 
#' @export
#' @author Kelli Faye Johnson
clean_data <- function(data) {
  cols <- colnames(data)
  if ("year" %in% cols) {
    data$Year <- data$year
  }
  if ("Scientific_name" %in% cols) {
    data$Sci <- data$Scientific_name
  }
  if ("Longitude_dd" %in% cols) {
    data$Lon <- data$Longitude_dd
  }
  if ("Long" %in% cols) {
    data$Lon <- data$Long
  }
  if ("Latitude_dd" %in% cols) {
    data$Lat <- data$Latitude_dd 
  }
  if ("total_catch_wt_kg" %in% cols) {
    data$Catch_KG <- data$total_catch_wt_kg
  }
  if ("Wt" %in% cols) {
    data$Catch_KG <- data$Wt
  }
  if ("Area_Swept_ha" %in% cols) {
    data$AreaSwept_km2 <- data$Area_Swept_ha / 100
  }
  if ("Vessel" %in% cols) {
    if (!all(grepl("_[0-9]{4}", as.character(data$Vessel)))) {
      data$Vesselraw <- data$Vessel
      data$Vessel <- as.factor(
        paste(data$Vessel, data$Year, sep = "_"))
      }
  } else {
    data <- cbind(data, "Vessel" = 1)
  }
  return(data)
}
