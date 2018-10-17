#' West Coast Groundfish Observer Program data preparation
#'
#' Prepare the data coming from the West Coast Groundfish Observer
#' Program (WCGOP) to be used in a \link{VAST} model.
#' Function was written as an example
#'
#' @param data A data frame of WCOP data.
#' @param species A vector of character entries that specify the
#' species, using scientific names, that you want to keep.
#' @param gear A vector of character entries that specify the gear
#' types you want to keep. Vector can include values from both the
#' gear column and the GEAR_TYPE column.
#'
#' @export
#' @return A n invisible data frame with the following columns
#' \enumerate{
#'   \item Catch_KG
#'   \item Retained_KG
#'   \item Discarded_KG
#'   \item Year
#'   \item Vessel
#'   \item Lat
#'   \item Lon
#'   \item Species
#' }
#'
#' @author Kelli Faye Johnson
#'
WCGOP_clean <- function(data, species, gear) {
  # access to start and end locations, which could be helpful for
  # giving the amount of area covered.
  # "HAUL_DURATION"  (Up - Set) * 24
  # todo: think about how this unit of measurement would be different
  # than if one assumed that the correction is in terms of space rather than time
  # Also could get names of the vessels to estimate the random effect of vessel
  gear <- tolower(gear)
  data$GEAR_TYPE <- tolower(data$GEAR_TYPE)
  data$gear <- tolower(data$gear)
  if ("GEAR_TYPE" %in% colnames(data) &&
      any(gear %in% unique(data$GEAR_TYPE))) {
    data <- data[data$GEAR_TYPE %in% gear, ]
  }
  if ("gear" %in% colnames(data) &&
      any(gear %in% unique(data$gear))) {
    data <- data[data$gear %in% gear, ]
  }

  if (!is.list(species)) {
    names(species) <- species
    species <- as.list(species)
  }
  # todo: determine which column name to use
  data$SCIENTIFIC_NAME <- gsub("^[[:space:]]+", " ", data$SCIENTIFIC_NAME)
  data$scientific_name <- gsub(x = data$scientific_name,
    "Coryphaenoides pectoralis", "Albatrossia pectoralis")
  data$scientific_name <- gsub(x = data$scientific_name,
    "Isopsetta isolepis", "Pleuronectes isolepis")
  spptest <- with(data, table(SCIENTIFIC_NAME, scientific_name))
  spptest <- apply(spptest, 1, function(x) {
      if (length(x[which(x > 0)]) == 1) NULL else(x[which(x > 0)])
    })
  # Listname is the SCIENTIFIC_NAME
  # vector names are from scientific_name
  spptest <- spptest[sapply(spptest, length) != 0]

  # todo(maybe): make a long data frame with an additional column if the CATCH_KG
  # is from retrained or discarded so that you can combine the two columns
  # of spp and this additional column to have category be retained vs. discard.
  # Would need a switch to shorten the data frame if you don't want unique spp
  # rows for each catch category.
  new <- NULL
  for (ii in seq_along(species)) {
    xx <- split(data, data$SCIENTIFIC_NAME %in% species[[ii]])
    xx[["FALSE"]][, c("DIS_MT", "RET_MT")] <- 0
    xx[["FALSE"]][, c("SCIENTIFIC_NAME", "scientific_name")] <- names(species)[ii]
    xx[["TRUE"]][, c("SCIENTIFIC_NAME", "scientific_name")] <- names(species)[ii]
    xx <- do.call("rbind", xx)
    xx$Catch_KG <- (xx$DIS_MT + xx$RET_MT)  * 1000
    xx <- xx[order(xx$Catch_KG, decreasing = TRUE), ]
    xx <- xx[!duplicated(paste(xx$SET_YEAR, xx$HAUL_ID)), ]

    new <- rbind(new, xx)
    rm(xx)
  }
  data <- new

  data <- data.frame(
    "Catch_KG" = data$Catch_KG,
    "Retained_KG" = data$RET_MT * 1000,
    "Discarded_KG" = data$DIS_MT * 1000,
    "Year" = data$SET_YEAR,
    "Vessel" = 1,
    "AreaSwept_km2" = data$HAUL_DURATION,
    "Lat" = data$AVG_LAT,
    "Lon" = data$AVG_LONG,
    "Sci" = data$SCIENTIFIC_NAME)
  data <- data[data$AreaSwept_km2 != 0, ]
  invisible(data)
}
