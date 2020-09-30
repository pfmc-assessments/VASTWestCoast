#' Run Diagnostic Plots for VASTWestCoast Output
#' 
#' @param dir The directory where the output from 
#' \code{\link{VAST_condition}} is saved. Can be a relative
#' or an absolute path.
#' 
#' @export
#' @author Kelli Faye Johnson
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
  if (length(savedfile) == 0) return(NULL)
  base::load(savedfile)
  if ("simpleError" %in% class(out)) return(NULL)

  # Check convergence
  cat(file = file.path(dir, "convergence_gradient.txt"),
    pander::pandoc.table.return(
      out$parameter_estimates$diagnostics[,
        c("Param","Lower","MLE","Upper","final_gradient")]))

  return(invisible())
}
