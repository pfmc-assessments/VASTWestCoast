#' Set up the spatial information for a \link{[VAST]} run.
#'
#' @param data A data frame with columns for Latitude and Longitude labeled using
#' abbreviations "Lat" and "Lon" and column labeled "Year".
#' @param dir A directory where you want to save the kmeans information to. The
#' character value can be a full path or relative to your current working directory.
#' @param regionacronym A character value supplying the region acronym. Currently,
#' only "EBSTS" and "WCGBTS" are supported.
#' @param strata A data frame specifying the strata.
#' @param nknots The number of knots you want in your spatial field.
#'
#' @return A list of spatial information used to run the model.
#'
#' @author Kelli Faye Johnson
#' @export
#'
VAST_setup <- function(data, dir, regionacronym, strata = NULL, nknots) {
  Extrapolation_List <- switch(regionacronym,
    EBSBTS = "eastern_bering_sea",
    WCGBTS = "California_current",
    NULL)
  if (is.null(Extrapolation_List)) stop("The survey,",
    regionacronym, ", was not recognized.")

  Extrapolation_List <- suppressMessages(FishStatsUtils::make_extrapolation_info(
    Region = Extrapolation_List,
    strata.limits = strata))
  if (regionacronym == "EBSBTS") {
    ignore <- which(colnames(Extrapolation_List$Data_Extrap) == "Mid_Depth")
    if (length(ignore) == 1) colnames(Extrapolation_List$Data_Extrap)[ignore] <- "Depth_km"
    Extrapolation_List$Data_Extrap$Depth_km2 <- Extrapolation_List$Data_Extrap$Depth_km^2
  }
  # Standardize the depth data
  # White paper that advocates for standardizing before squaring for a quadratic
  # term to reduce the correlation between the variables.
  unstandarddepth <- Extrapolation_List$Data_Extrap[, "Depth_km"]
  Extrapolation_List$Data_Extrap$indkm <- Extrapolation_List$Data_Extrap[, "Depth_km"]
  Extrapolation_List$Data_Extrap$indkms <- Extrapolation_List$Data_Extrap[, "Depth_km2"]
  # Before I standardized and then took the mean
  # Extrapolation_List$Data_Extrap[, "Depth_km"] <-
  #   (Extrapolation_List$Data_Extrap[, "Depth_km"] -
  #    mean(Extrapolation_List$Data_Extrap[, "Depth_km"])
  #   ) / sd(Extrapolation_List$Data_Extrap[, "Depth_km"])
  # Extrapolation_List$Data_Extrap[, "Depth_km2"] <-
  #   Extrapolation_List$Data_Extrap[, "Depth_km"]^2
  Spatial_List <- suppressMessages(FishStatsUtils::make_spatial_info(
    n_x = nknots,
    Method = "Mesh",
    Lon_i = data[, "Lon"], Lat_i = data[, "Lat"],
    Extrapolation_List = Extrapolation_List,
   # Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
    randomseed = 1,
    nstart = 100,
    iter.max = 1e3,
    # Argument passed to Calc_Kmeans
    DirPath = dir))
  data <- cbind(data, "knot_i" = Spatial_List$knot_i)

  # Generate covariate
  # 1. Mean is taken per knot of the input data.
  # 2. The mean per knot is standardized to have a mean of zero and sd of 1
  # 3. The standardized mean is squared
  Year_Set <- seq(min(data[, "Year"]), max(data[, "Year"]))
  depthperknot <- suppressMessages(FishStatsUtils::format_covariates(
    Lat_e = Extrapolation_List$Data_Extrap$Lat,
    Lon_e = Extrapolation_List$Data_Extrap$Lon,
    t_e = data$Year[
      match(Spatial_List$NN_Extrap$nn.idx, data$knot_i)
      ],
    Cov_ep = Extrapolation_List$Data_Extrap[, c("Depth_km", "Depth_km2")],
    Extrapolation_List = Extrapolation_List,
    Spatial_List = Spatial_List,
    FUN = mean,
    Year_Set = Year_Set,
    na.omit = "time-average"))
  X_xtp <- apply(depthperknot$Cov_xtp, 2:3, scale)
  X_xtp[, , 2] <- X_xtp[, , 1]^2
  dimnames(X_xtp)[[1]] <- dimnames(depthperknot$Cov_xtp)[[1]]
  # Old code to square after calculating for a mean per knot
  # get_depth <- function(a, b, n, squared = TRUE) {
  #   xx <- tapply(a[, "Depth_km"], INDEX = b, FUN = mean)
  #   X_xtp = xx %o% rep(1, n) %o% 1
  #   if (squared) {
  #     X_xtp <- array(c(X_xtp, X_xtp^2), dim = c(dim(X_xtp)[1:2], 2))
  #   }
  #   X_xtp
  # }
  # X_xtp <- get_depth(Extrapolation_List$Data_Extrap,
  #   Spatial_List$PolygonList$NN_Extrap$nn.idx,
  #   max(which(Year_Set %in% sort(unique(data[, "Year"])))))

  # Change depth in Extrapolation_List so that it matches EM
  Extrapolation_List$Data_Extrap[, "Depth_km"] <-
    X_xtp[, 1, 1][Spatial_List$PolygonList$NN_Extrap$nn.idx]
  Extrapolation_List$Data_Extrap[, "Depth_km2"] <-
    X_xtp[, 1, 2][Spatial_List$PolygonList$NN_Extrap$nn.idx]
  return(list(
    "data" = data,
    "Extrapolation_List" = Extrapolation_List,
    "unstandarddepth" = unstandarddepth,
    "depthperknot" = depthperknot,
    "Spatial_List" = Spatial_List,
    "X_xtp" = X_xtp))
}
