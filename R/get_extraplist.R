#' Get Extrapolation List to Expand Results
#' 
#' Use \code{\link[FishStatsUtils]{make_extrapolation_info}}
#' sub functions to prepare the extrapolation grid.
#' 
#' @template survey 
#' @param strata A data frame specifying the strata.
#' 
#' @import FishStatsUtils
#' 
#' @author Kelli Faye Johnson
#' 
get_extraplist <- function(survey, strata = NULL) {
  
  extraplist <- switch(survey,
    EBSBTS = c(FishStatsUtils::Prepare_EBS_Extrapolation_Data_Fn(
    strata.limits = strata,
    zone = NA), 
    "region" = "eastern_bering_sea"),
    c(FishStatsUtils::Prepare_WCGBTS_Extrapolation_Data_Fn(
    strata.limits = strata,
    surveyname = switch(survey,
      Triennial = "propInTriennial", 
      NWFSC.Slope = "propInSlope02", 
                  "propInWCGBTS"),
    zone = NA), 
    "region" = "california_current"))
  colnames(extraplist[["Data_Extrap"]]) <- gsub("Mid_Depth", "Depth_km",
    colnames(extraplist[["Data_Extrap"]]))
  extraplist[["Data_Extrap"]][, "Depth_km2"] <- 
    extraplist[["Data_Extrap"]][, "Depth_km"]^2
  extraplist[["Data_Extrap"]][, "indkm"] <- extraplist[["Data_Extrap"]][, "Depth_km"]
  extraplist[["Data_Extrap"]][, "indkms"] <- extraplist[["Data_Extrap"]][, "Depth_km2"]
  return(extraplist)
}
