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
  colnames(data) <- gsub("year", "Year", colnames(data))
  colnames(data) <- gsub("entific_name|itude_dd", "", colnames(data))
  colnames(data) <- gsub("Long", "Lon", colnames(data))
  colnames(data) <- gsub("total_catch_wt_kg|Wt", "Catch_KG", colnames(data))
  if ("Area_Swept_ha" %in% cols) {
    data[, "AreaSwept_km2"] <- data[, "Area_Swept_ha"] / 100
  }
  if (!"Vessel" %in% cols) data[, "Vessel"] <- 1
  data[, "Vesselraw"] <- data[, "Vessel"]

  # Clean Triennial
  if ("Project" %in% cols) {
    if (unique(data[, "Project"]) == "Triennial") {
      data <- data[!data[, "Year"] %in% 1977, ]
    }
  }
  if (!all(grepl("_[0-9]{4}", as.character(data[, "Vessel"])))) {
    data[, "Vessel"] <- as.factor(
      paste(data[, "Vessel"], data[, "Year"], sep = "_"))
  }
  return(data)
}
