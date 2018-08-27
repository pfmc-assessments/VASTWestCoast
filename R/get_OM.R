#' Get information on parameters from a simulated data set
#'
#' The function can be ran on multiple operating iterations using
#' \code{dir(folderx, )}
#'
#' @param file An R file with information saved from \code{\link{VAST_OM}}.
#'
#' @return A data frame with parameter values. Parameter names will be
#' prefixed with "om".
#'
#' @author Kelli Faye Johnson
#' @export
#'
get_OM <- function(file) {
  # todo: add number of knots
  if (!file.exists(file)) return(NULL)
  load(file)
  index <- Sim$B_tl[,1]/1000
  names(index) <- paste0("om_index_", seq_along(index))
  sets <- Sim$Sim_Settings[c("Range1", "Range2", "SigmaO1",
    "SigmaO2", "SigmaE1", "SigmaE2", "Depth1_km", "Depth2_km",
    "Depth1_km2", "Depth2_km2")]
  names(sets) <- paste0("om_", tolower(names(sets)))
  sets$om_type <- ifelse(Sim$Sim_Settings$ObsModel[2] == 0,
    "Conventional delta", "OM=Poisson-process link")
  sets$om_depth <- ifelse(all(
     Sim$Sim_Settings[grep("Depth", names(Sim$Sim_Settings))] == 0),
     "nodepth", "depth")
  sets$om_depthtype <- ifelse(sets$om_depth == "nodepth",
    "no depth in OM",
    ifelse(all(Sim$Sim_Settings[grep("_km2",
      names(Sim$Sim_Settings))] == 0), "linear", "quadratic"))
  sets$n_t <- length(index)
  temp <- lm(log(index) ~ 1 + I(seq_along(index)))$coef
  names(temp) <- NULL
  sets$linear_om <- temp[2]
  logratio <- get_logratio(index)
  OM <- gsub(
    paste0(".+\\", .Platform$file.sep, "([0-9]+)OM.+"),
    "\\1", file)
  sim <- gsub(
    paste0(".+\\", .Platform$file.sep, "([0-9]+)_VAST_simulation.+"),
    "\\1", file)
  rep <- gsub(
    paste0(".+OM\\", .Platform$file.sep, "([0-9]+).+"),
    "\\1", file)
  finalinfo <- data.frame(c(
    index,
    sets,
    "rep" = rep,
    "sim" = sim,
    "OM" = OM,
    "om_logratio" = logratio))
  return(finalinfo)
}
