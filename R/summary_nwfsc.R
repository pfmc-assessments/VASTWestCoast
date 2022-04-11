#' Summarize a VAST model for Northwest Fisheries Science Center
#'
#' Generate output tables requested by the Pacific Fisheries 
#' Management Council Science and Statistical Committee
#'
#' @param obj A compiled object from Template Model Builder (TMB)
#' after estimating the maximum likelihood estimate of the fixed effects.
#' @param parameter_estimates Output from \code{\link[FishStatsUtils]{fit_model}}.
#' @param savedir A directory to save the resulting tables, which will
#' be in csv format.
#' @return Tagged list containing objects for running a VAST model
#' \describe{
#'   \item{TableA}{Table of settings}
#'   \item{TableB}{Table of estimated parameters}
#'   \item{TableC}{Table of MLE of fixed effects}
#' }
#' 
#' @author James T. Thorson
#' @export
#' 
summary_nwfsc <- function(obj, parameter_estimates, savedir = NULL) {

  f <- function(num, threshold = 0.000001) {
    ifelse(num<threshold, paste0("< ", threshold), num)
  }

  # Table of settings
  TableA <- data.frame("Setting_name" = rep(NA, 9), "Setting_used" = NA)
  TableA[1, ] <- c("Number of knots", obj$env$data$n_x)
  TableA[2, ] <- c("Maximum gradient",
    formatC(f(parameter_estimates$max_gradient),
    format = "f", digits = 6))
  TableA[3, ] <- c("Is hessian positive definite?", 
    ifelse(is.null(parameter_estimates$SD), "No", "Yes"))
  TableA[4, ] <- c("Was bias correction used?", 
    ifelse(
      all(is.na(parameter_estimates$SD[["unbiased"]][["value"]])), 
      "No", 
      "Yes"))
  TableA[5, ] <- c("Distribution for measurement errors", 
    ifelse(as.character(obj$env$data$ObsModel[2]) == 1,
    "Poisson-link", 
    switch(as.character(obj$env$data$ObsModel[1]),
      "1" = "Lognormal",
      "2" = "Gamma",
      "10" = "Tweedie")))
  TableA[6, ] <- c("Spatial effect for encounter probability", 
    ifelse(obj$env$data$FieldConfig[1, 2] < 0, "No", "Yes"))
  TableA[7, ] <- c("Spatio-temporal effect for encounter probability", 
    ifelse(obj$env$data$FieldConfig[3, 2] < 0, "No", "Yes"))
  TableA[8, ] <- c("Spatial effect for positive catch rate",
    ifelse(obj$env$data$FieldConfig[2, 2] < 0, "No", "Yes"))
  TableA[9, ] <- c("Spatio-temporal effect for positive catch rate", 
    ifelse(obj$env$data$FieldConfig[4, 2] < 0, "No", "Yes"))

  # Print number of parameters
  fe <- names(obj$env$last.par[-obj$env$random])
  re <- names(obj$env$last.par[obj$env$random])
  TableB <- data.frame(
    Coefficient_name = c(names(table(fe)), names(table(re))),
    Number_of_coefficients = utils::type.convert(c(table(fe), table(re)), as.is = TRUE),
    Type = c(rep("Fixed", length(unique(fe))), rep("Random", length(unique(re))))
  )
  rm(fe, re)

  # Print table of MLE of fixed effects
  TableC <- TMB::summary.sdreport(parameter_estimates$SD, "fixed")

  # Return
  Return <- list("TableA" = TableA, "TableB" = TableB, "TableC" = TableC)
  if(!is.null(savedir)) {
    for(i in 1:3) {
      utils::write.csv(Return[[i]],
        file = file.path(savedir, paste0(names(Return)[i], ".csv")),
        row.names = c(FALSE, FALSE, TRUE)[i])
    }
  }
  return(Return)
}
