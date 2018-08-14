#' Run the operating model (OM) for a simulation.
#'
#' @param reps A vector of replicates you want to run in the OM folder.
#' @param dir A directory to place the simulated replicates. If specified
#' as \code{NULL}, which is the default, then the OM folder will be placed
#' in the conditioning directory and labeled sequentially with two numerical
#' integers followed by the letters OM. For example, \code{"02OM"} if there is
#' already an OM folder in the conditioning directory.
#' @param conditioning A list of conditioning specifications.
#' @param ncluster A integer value specifying the number of cores to use when
#' generating the simulated data.
#'
#' @import doParallel
#' @importFrom foreach %dopar%
#' @return Nothing is returned from the function. Objects are saved
#' to the disk within the \code{\link{VAST_OMrepi}} function.
#'
#' @author Kelli Faye Johnson
#' @export
#'
VAST_OM <- function(reps, dir = NULL, conditioning, ncluster = 2) {
  # Make the folder
  if (is.null(dir)) {
    numbers <- dir(conditioning$folder, pattern = "OM")
    numbers <- ifelse(length(numbers) == 0, "00", numbers)
    new <- formatC(max(as.numeric(substring(numbers, 1, 2))) + 1, width = 2, flag = "0")
    dir <- file.path(conditioning$folder, paste0(new, "OM"))
  }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  # Bring in the data
  e1 <- new.env()
  load(dir(conditioning$folder, pattern = "setup.RData",
    full.names = TRUE),
    envir = e1)
  conditioning$data <- get("info", envir = e1)$data
  conditioning$extrapolation <- get("info", envir = e1)$Extrapolation_List

  clin <- makeCluster(ncluster)
  on.exit(stopCluster(clin))
  registerDoParallel(clin)
  ignore <- foreach::foreach(
    i = reps,
    .export = ls(pattern = "VAST_", envir = .GlobalEnv),
    .packages = c("SpatialDeltaGLMM")) %dopar% {
    VAST_OMrepi(omdir = dir, rep = i, settings = conditioning)
  }
}
#'
#' @param omdir A file path where the \code{rep} should be saved, which will
#' lead to \code{file.path(omdir, rep)}.
#' @param rep A single numerical value specifying the replicate number.
#' @param settings A list of settings for the simulation.
#' @param plotindex A logical value specifying if the time-series trajectory
#' of the simulated data should be plotted in the \code{omdir}.
#'
#' @return Objects are saved in an \code{RData} file within the directory created
#' for the replicate. Also a plot is saved if \code{plotindex == TRUE} of the
#' time-series trajectory.
#'
#' @author Kelli Faye Johnson
#'
VAST_OMrepi <- function(omdir, rep, settings,
  maxtries = 10, plotindex = TRUE) {

  if (length(rep) != 1 | !is.numeric(rep)) stop("rep must be a single numeric value")
  # todo: check that settings has all of the appropriate names that are needed.

  repdir <- file.path(omdir, rep)
  dir.create(repdir, showWarnings = FALSE, recursive = TRUE)

  # Check encounter rate is not all zero or all 1 for a given year across
  # all locations
  Prop_t <- 0
  counter <- 0
  multiplier <- ifelse(maxtries > 10, maxtries, 10)
  while(counter < maxtries && any(any(Prop_t == 0) | any(Prop_t == 1))) {
    counter <- counter + 1
    set.seed(rep * multiplier + counter)
    # Confirmed here that I am simulating data specific to the locations
    # in the empirical data and not to the larger grid, which you can do
    # by not supplying an argument to Data_Geostat.
    Sim <- SpatialDeltaGLMM::Geostat_Sim(Sim_Settings = settings,
      Extrapolation_List = settings$extrapolation,
      Data_Geostat = settings$data)
    Prop_t <- tapply(Sim$Data_Geostat[, "Catch_KG"],
      INDEX = Sim$Data_Geostat[, "Year"], FUN = function(vec){mean(vec>0)})
  }
  save(Sim, file = file.path(repdir, "Sim.RData"))

  if (plotindex) {
    png(filename = file.path(repdir, "Index-Sim.png"),
      width = 5, height = 5, res = 200, units = "in")
      plot(
        x = seq(min(Sim$Data_Geostat[, "Year"]), max(Sim$Data_Geostat[, "Year"])),
        y = Sim$B_tl[, 1]/1000,
        type = "b", ylim = c(0, max(Sim$B_tl[, 1]/1000)),
        xlab = "year", ylab = "index")
    dev.off()
  }
}
