#' Unload VAST dll
#'
#' Unload a dll library with the VAST name.
#'
#' @param searchfor A file name of the dll to search for.
#'
#' @export
#' @author Kelli Faye Johnson
#' @return The name(s) of the unloaded libraries are returned.
#'
clean_unload <- function(searchfor = "VAST") {
  dlls <- getLoadedDLLs()
  filenames <- lapply(dlls, "[[", "path")
  remove <- gsub("\\.dll", "", grep(searchfor, filenames, value = TRUE))
  if (length(remove) < 1) return(NULL)
  ignore <- lapply(remove, function(x) dyn.unload(TMB::dynlib(x)))
  return(list(removed = remove, available = filenames))
}
