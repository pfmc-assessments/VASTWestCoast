#' Save Detailed Session Info to the Disk
#'
#' Save details about the R session used to run the model to the disk.
#' Information regarding packages and their versions is saved to
#' help those trying to recreate results in the future.
#'
#' @template savefile
#'
#' @return A list with information about the current R session. Also,
#' the same list is written to a text file and an \code{RData} file.
#' @author Kelli Faye Johnson
#' @export
#' @examples
#' info <- summary_session()
#' \dontrun{
#' names(info)
#' }
#'
summary_session <- function(savefile = "session.txt") {

  #### get information
  enviro <- Sys.getenv()
  info <- utils::sessionInfo()
  out <- list(
    date = Sys.time(),
    environment = enviro,
    PATHpretty = strsplit(enviro[["PATH"]], ";")[[1]],
    session = info,
    pkgdesc = c(info$otherPkgs, info$loadedOnly),
    writenby = sys.call())

  #### write files to the disk
  utils::capture.output(out, file = savefile)
  save(out, file = gsub("txt$", "RData", savefile))

  #### end the function with a return call
  return(out)
}
