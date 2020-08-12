#' Summarize a VAST model for Northwest Fisheries Science Center
#'
#' Generate output tables requested by the Pacific Fisheries 
#' Management Council Science and Statistical Committee
#'
#' @param obj A compiled object from Template Model Builder (TMB)
#' after estimating the maximum likelihood estimate of the fixed effects.
#' @param sdreport Output from \code{\link[TMB]{sdreport}}
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
summary_nwfsc <- function(obj, sdreport, savedir = NULL) {

  f <- function(num, threshold = 0.000001) {
    ifelse(num<threshold, paste0("< ", threshold), num)
  }

  # Table of settings
  TableA <- data.frame("Setting_name" = rep(NA, 9), "Setting_used" = NA)
  TableA[1, ] <- c("Number of knots", obj$env$data$n_x)
  TableA[2, ] <- c("Maximum gradient", 
    formatC(f(max(abs(obj$gr(
      TMB::summary.sdreport(sdreport,"fixed")[, "Estimate"])))),
    format = "f", digits = 6))
  TableA[3, ] <- c("Is hessian positive definite?", 
    switch(as.character(sdreport$pdHess),
      "FALSE" = "No",
      "TRUE" = "Yes"))
  TableA[4, ] <- c("Was bias correction used?", 
    ifelse(
      all(is.na(sdreport[["unbiased"]][["value"]])), 
      "No", 
      "Yes"))
  TableA[5, ] <- c("Distribution for measurement errors", 
    ifelse(as.character(obj$env$data$ObsModel[2]) == 1,
    "Poisson-link", 
    switch(as.character(obj$env$data$ObsModel[1]),
      "1" = "Lognormal",
      "2" = "Gamma")))
  TableA[6, ] <- c("Spatial effect for encounter probability", 
    switch(as.character(obj$env$data$FieldConfig[1]),
      "-1" = "No",
      "1" = "Yes"))
  TableA[7, ] <- c("Spatio-temporal effect for encounter probability", 
    switch(as.character(obj$env$data$FieldConfig[2]),
      "-1" = "No",
      "1" = "Yes"))
  TableA[8, ] <- c("Spatial effect for positive catch rate", 
    switch(as.character(obj$env$data$FieldConfig[3]),
      "-1" = "No",
      "1" = "Yes"))
  TableA[9, ] <- c("Spatio-temporal effect for positive catch rate", 
    switch(as.character(obj$env$data$FieldConfig[4]),
      "-1" = "No",
      "1" = "Yes"))

  # Print number of parameters
  fe <- names(obj$env$last.par[-obj$env$random])
  re <- names(obj$env$last.par[obj$env$random])
  TableB <- data.frame(
    Coefficient_name = c(names(table(fe)), names(table(re))),
    Number_of_coefficients = type.convert(c(table(fe), table(re)), as.is = TRUE),
    Type = c(rep("Fixed", length(unique(fe))), rep("Random", length(unique(re))))
  )
  rm(fe, re)

  # Print table of MLE of fixed effects
  TableC <- TMB::summary.sdreport(sdreport, "fixed")

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
