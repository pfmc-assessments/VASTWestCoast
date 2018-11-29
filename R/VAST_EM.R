#' Run the full estimation method that loops over parameterizations
#'
#' @param reps A vector of replicate numbers
#' @param settings A list of settings.
#' @param directory The directory for the given simulation.
#' @param n_cluster An integer value specifying the number of cores to use.
#' @param getdatafrom The directory that stores the data.
#'
#' @return Saves information to the disk.
#'
#' @import doParallel
#' @import TMB
#' @importFrom foreach %dopar%
#' @importFrom parallel makeCluster stopCluster
#'
#' @author Kelli Faye Johnson
#' @export
#'
VAST_EM <- function(reps, settings, directory, n_cluster, getdatafrom) {
  # todo: write code that will determine which type of models to run and
  # defines the settings
  settings <- get_settings(settings)
  settingsa <- settingsb <- settingsc <- settings
  settingsa$depth <- FALSE
  settingsb$depth <- "linear"
  settingsc$depth <- "squared"

  # Run the replicates in parallel
  replicates <- sapply(reps,
    function(x) dir(directory, pattern = paste0("^", x, "$"),
      full.names = TRUE))
  clem <- parallel::makeCluster(n_cluster)
  on.exit(parallel::stopCluster(clem))
  registerDoParallel(clem)
  i <- NULL
  ignore <- foreach::foreach(
    i = replicates,
    .packages = c(
      "TMB", "TMBhelper",
      "VASTWestCoast")) %dopar% {
    Sim <- NULL
    load(dir(i, pattern = "Sim.R", full.names = TRUE))
    simdata <- Sim$Data_Geostat
    VAST_EMrepi(settings = settingsa, data = simdata, datadir = getdatafrom,
      emdir = file.path(i, "EM01"))
    VAST_EMrepi(settings = settingsb, data = simdata, datadir = getdatafrom,
      emdir = file.path(i, "EM02"))
    VAST_EMrepi(settings = settingsc, data = simdata, datadir = getdatafrom,
      emdir = file.path(i, "EM03"))
  }
}
#' Run the estimation method for a given set of "settings"
#'
#' @param settings A list of settings.
#' @param data A matrix of data
#' @param datadir A directory where the kmeans information is housed.
#' @param emdir A directory where the EM information will be saved.
#' @param overdispersion A vector of values specifying if a vessel_year
#' effect should be included. The default \code{c("eta1" = 0, "eta2" = 0)}
#' does not estimate an effect.
#' @param rerun A logical value specifying if the EM should be overwritten
#' with a new estimation process.
#'
#' @return Saves information to the disk.
#'
#' @import TMB
#'
#' @author Kelli Faye Johnson
#'
VAST_EMrepi <- function(settings, data, datadir, emdir,
  overdispersion = c("eta1" = 0, "eta2" = 0),
  rerun = FALSE) {
  settings <- get_settings(settings)
  survey <- strsplit(settings$Species, "_")[[1]][1]

  dir.create(emdir, showWarnings = FALSE, recursive = TRUE)

  # Check to see if the model has already been tried, i.e., there
  # will be a EMspecs.RData file
  if(file.exists(file.path(emdir, "EMspecs.RData")) & !rerun) return()

  kmeandir <- file.path(datadir, survey)
  info <- VAST_setup(data = data,
    dir = kmeandir,
    regionacronym = survey,
    strata = settings$strata,
    nknots = settings$nknots)
  save(info, settings, kmeandir,data, emdir, datadir,
    file = file.path(emdir, "EMspecs.RData"))

  # Set the vessel to 1, which will then set it to zero in
  # VAST_run if the overdispersion is not included.
  if (all(overdispersion == 0)) info$data[, "Vessel"] <- 1

  VAST_run(datalist = info, depth = settings$depth,
    overdispersion = overdispersion,
    obsmodel = settings$ObsModelEM,
    rundir = datadir,
    Version = settings$version, calcs = rep(0, 8),
    strata = settings$strata,
    savefile = file.path(emdir, "EMfit.RData"))
}
