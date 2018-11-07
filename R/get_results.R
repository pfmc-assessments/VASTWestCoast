#' Get results from OM and EM from a simulation
#'
#' @param omdir The directory that houses the operating model and runs.
#' @param write2disk A logical value specifying whether or not to write
#' the results to the disk. If \code{"results.csv"} already exists one
#' directory up from the \code{omdir} the results will be appended to this
#' file.
#'
#' @return A data frame of results with one row per simulation replicate.
#'
#' @author Kelli Faye Johnson
#' @importFrom utils read.csv write.table
#' @export
#'
get_results <- function(omdir, write2disk = TRUE) {
  omframe <- lapply(
    dir(omdir, pattern = "Sim.RData", recursive = TRUE, full.names = TRUE),
    get_OM)
  emframe <- lapply(
    dir(omdir, pattern = "EMfit.RData", recursive = TRUE, full.names = TRUE),
    get_EM)
  allframe <- merge(
    do.call("rbind", omframe),
    do.call("rbind", emframe),
    all = TRUE, by = c("rep", "OM"))
  allframe$coverage <- get_coverage(allframe)
  allframe$delta <- get_coefficient(allframe)

  # 06 Write the results to the disk
  file <- file.path(omdir, "results.csv")
  if (write2disk){
    if (file.exists(file)) {
      readin <- utils::read.csv(file, header = TRUE)
      allframe <- rbind(readin, allframe)
    }
    utils::write.table(allframe, file = file,
      sep = ",", row.names = FALSE)
  }
  return(allframe)
}
