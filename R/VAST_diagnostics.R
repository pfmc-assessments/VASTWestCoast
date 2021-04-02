#' Run diagnostic plots for VASTWestCoast output
#' 
#' @param dir The directory where the output from 
#' [VAST_condition] is saved. Can be a relative
#' or an absolute path.
#' 
#' @export
#' @author Kelli Faye Johnson
#' 
#' @return Nothing is returned.
#' * convergence_gradient.txt: Parameter estimates
#'
VAST_diagnostics <- function(dir = getwd()) {

  # Load the saved file
  # Use master dir b/c loading the saved files overwrites dir
  masterdir <- dir
  savedfile <- dir(masterdir, pattern = "^Save.RData", full.names = TRUE)
  if (length(savedfile) == 0) return(NULL)
  base::load(savedfile)
  if ("simpleError" %in% class(out)) return(NULL)

  # Check convergence
  cat(file = file.path(masterdir, "convergence_gradient.txt"),
    pander::pandoc.table.return(
      out$parameter_estimates$diagnostics[,
        c("Param","Lower","MLE","Upper","final_gradient")]))

  return(invisible())
}
