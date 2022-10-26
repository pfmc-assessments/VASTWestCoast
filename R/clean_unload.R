#' Unload VAST dll
#'
#' Unload a dll library with the VAST name.
#'
#' @param searchfor A file name of the dll to search for.
#' The files that are found will be removed from the loaded libraries.
#' @param keep A file name (or partial name) of the dll to search
#' for that you want to keep. For example, multiple libraries may
#' be loaded for VAST but you might only want to keep a certain one.
#' Put that certain one here, or some pattern that will match in a
#' regular expression search.
#'
#' @export
#' @author Kelli F. Johnson
#' @return The name(s) of the unloaded libraries are returned as
#' the first element of the list and the second element is all
#' dlls that were found.
#'
clean_unload <- function(searchfor = "VAST", keep = "VASTisCOOL") {
  dlls <- getLoadedDLLs()
  filenames <- lapply(dlls, "[[", "path")
  filekeep <- filenames[!grepl(keep, filenames, ignore.case = TRUE)]
  remove <- gsub("\\.dll", "", grep(searchfor, filekeep, value = TRUE))
  if (length(remove) < 1) return(list(removed = NULL, available = filenames))
  ignore <- lapply(remove, function(x) dyn.unload(TMB::dynlib(x)))
  return(list(removed = remove, available = filenames))
}
