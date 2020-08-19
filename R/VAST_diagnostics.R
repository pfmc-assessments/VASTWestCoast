#' Run Diagnostic Plots for VASTWestCoast Output
#' 
#' @param dir The directory where the output from 
#' \code{\link{VAST_condition}} is saved. Can be a relative
#' or an absolute path.
#' 
#' @export
#' @author Kelli Faye Johnson
#' @import FishStatsUtils
#' @importFrom pander pandoc.table
#' 
#' @return todo: Make a list of the tables and figures that are 
#' saved to the disk from VAST_diagnostics
#' 
VAST_diagnostics <- function(dir = getwd()) {
  
  dev.off.new <- function(keep = NULL) {
  	if (is.null(keep)) grDevices::graphics.off()
  	keep <- c(1, keep)
    all <- grDevices::dev.list()
    while (any(!all %in% keep)) {
    	if (!all[1] %in% keep) {
    		grDevices::dev.off(all[1])
    	}
    	all <- all[-1]
    }
  }
  devices <- grDevices::dev.list()
  on.exit(dev.off.new(keep = devices))
  
  # Load the saved file
  savedfile <- dir(dir, pattern = "^Save.RData", full.names = TRUE)
  datafile <- dir(dir, pattern = "DatabaseSave.RData", full.names = TRUE)
  if (length(savedfile) == 0) stop("The file Save.RData doesn't exist")
  if (length(datafile) == 0) stop("The file DatabaseSave.RData doesn't exist")
  base::load(savedfile)
  base::load(datafile)

  if (out$parameter_estimates$Convergence_check !=
      "There is no evidence that the model is not converged") {
    warning("The warning message from the optimization",
      " routine indicates the model \nmight not be converged. Check the following",
      " message:\n", out$parameter_estimates$Convergence_check, call. = FALSE)
    return(NULL)
  } else {
    if (!is.null(out$parameter_estimates$SD)) {
    warning(call. = FALSE, "The model in, ", dir,
      " appears to have found a solution but didn't produce a hessian")
    return(NULL)
  }}
  if (is.null(info$region)) info$region <- "california_current"

  # Check convergence
  cat(file = file.path(dir, "convergence_gradient.txt"),
    pander::pandoc.table.return(
      out$parameter_estimates$diagnostics[,
        c("Param","Lower","MLE","Upper","final_gradient")]))

  #plot index
  plot_index(dir)

  invisible()
}
