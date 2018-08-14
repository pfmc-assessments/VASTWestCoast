#' Set up the spatial information for a \link{[VAST]} run.
#'
#' @param dir A directory where you want to save the kmeans information to. The
#' character value can be a full path or relative to your current working directory.
#'
VAST_setup <- function(data, dir, regionacronym, strata, nknots) {
  Extrapolation_List <- switch(regionacronym,
    EBSTS = "eastern_bering_sea",
    WCGBTS = "California_current")
  if (length(Extrapolation_List) == 0) stop("Survey not recognized.")
  Extrapolation_List <- suppressMessages(SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn(
    Region = Extrapolation_List,
    strata.limits = strata))
  if (regionacronym == "EBSBTS") {
    ignore <- which(colnames(Extrapolation_List$Data_Extrap)) == "Mid_Depth"
    if (length(ignore) == 1) colnames(Extrapolation_List$Data_Extrap)[ignore] <- "Depth_km"
  }
  # Standardize the depth data
  # White paper that advocates for standardizing before squaring for a quadratic
  # term to reduce the correlation between the variables.
  unstandarddepth <- Extrapolation_List$Data_Extrap[, "Depth_km"]
  Extrapolation_List$Data_Extrap[, "Depth_km"] <-
    (Extrapolation_List$Data_Extrap[, "Depth_km"] -
     mean(Extrapolation_List$Data_Extrap[, "Depth_km"])
    ) / sd(Extrapolation_List$Data_Extrap[, "Depth_km"])
  Spatial_List <- suppressMessages(SpatialDeltaGLMM::Spatial_Information_Fn(
    grid_size_km = 25,
    n_x = nknots,
    Method = "Mesh",
    Lon = data[, "Lon"], Lat = data[, "Lat"],
    Extrapolation_List = Extrapolation_List,
   # Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
    randomseed = 1,
    nstart = 100,
    iter.max = 1e3,
    DirPath = dir))
  data <- cbind(data, "knot_i" = Spatial_List$knot_i)

  # Generate covariate
  Year_Set <- seq(min(data[, "Year"]), max(data[, "Year"]))
  X_xtp <- get_depth(Extrapolation_List$Data_Extrap,
    Spatial_List$PolygonList$NN_Extrap$nn.idx,
    max(which(Year_Set %in% sort(unique(data[, "Year"])))))
  # Change depth in Extrapolation_List so that it matches EM
  Extrapolation_List$Data_Extrap[, "Depth_km"] <- X_xtp[,1,1][Spatial_List$PolygonList$NN_Extrap$nn.idx]
  return(list(
    "data" = data,
    "Extrapolation_List" = Extrapolation_List,
    "unstandarddepth" = unstandarddepth,
    "Spatial_List" = Spatial_List,
    "X_xtp" = X_xtp))
}
