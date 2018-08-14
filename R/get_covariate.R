#' Generate covariates
#'
get_covariate <- function() {
  # Get covariate other than depth
}
#'
#' Get depth from downloaded data
#'
#' @examples
#'  exdepth <- get_depth(Extrapolation_List$Data_Extrap,
#'    Spatial_List$PolygonList$NN_Extrap$nn.idx, max(Data_Geostat[, "Year"]))
get_depth <- function(a, b, n, squared = TRUE) {
  # todo: square then take the mean
  depth2 <- a[, "Depth_km"] * a[, "Depth_km"]
  xx <- tapply(a[, "Depth_km"], INDEX = b, FUN = mean)
  X_xtp = xx %o% rep(1, n) %o% 1
  if (squared) {
    X_xtp <- array(c(X_xtp, X_xtp^2), dim = c(dim(X_xtp)[1:2], 2))
  }
  return(X_xtp)
}
