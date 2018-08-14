#' Run a complete simulation using \link{VAST}
#'
#' @param maindir The main directory that will house all of the results using a
#' specific folder structure, where the conditioning, operating model, and estimation
#' model results will all be housed in this directory.
#' @param conditiondir A directory for the conditioning. Only specify if the
#' conditioning has already been ran or you want to name the directory.
#' @param globalsettings A list of settings for the simulation.
#' @param n_cluster The number of clusters you want to use for parallel computing.
#'
#' @return
#'
#' @author Kelli Faye Johnson
#' @export
#'
#' @examples
#' VAST_simulation(globalsettings = Sim_Settings, n_cluster = 1)
#'
VAST_simulation <- function(maindir = getwd(), conditiondir = NULL,
  globalsettings, n_cluster) {

  # todo: make the reps a vector so you can specify which reps to run
  # todo: make the check for the max a switch to turn on and off
  globalsettings <- get_settings(globalsettings)

  # Make the dirs
  # Check which ones exist and add a single digit to the maximum value
  if (is.null(conditiondir)) {
    ffnames <- dir(maindir, "[[:digit:]]+_VAST_simulation")
    ffnames <- ifelse(length(ffnames) == 0, "01_VAST_simulation",
      paste(
        formatC(as.numeric(strsplit(tail(ffnames, 1), "_")[[1]][1]) + 1,
          flag = "0", width = 2),
        "VAST", "simulation", sep = "_"))
    conditiondir <- file.path(maindir,
      ifelse(!is.null(globalsettings$folder), "VAST_simulation",
        ffnames))
  }
  dir.create (conditiondir, showWarnings = FALSE, recursive = TRUE)
  datadir <- dir(maindir, "data|downloads", full.names = TRUE)

  # 01 Condition
  if (!file.exists(file.path(conditiondir, "parameter_estimates.txt"))) {
    VAST_condition(conditiondir, settings = globalsettings,
      spp = globalsettings$Species,
      datadir = datadir, overdisperion = NULL)
  }

  # 02 OM
  # Read in a few things to change the settings
  # Total settings are
  # Settings = list("beta1_mean"=0, "beta2_mean"=0,
  # "beta1_slope"=0, "beta2_slope"=0,
  # "beta1_sd"=0, "beta2_sd"=0,
  # "Nyears"=10, "Nsamp_per_year"=600,
  # "Depth1_km"=0, "Depth1_km2"=0, "Dist1_sqrtkm"=0,
  # "Depth2_km"=0, "Depth2_km2"=0, "Dist2_sqrtkm"=0,
  # "SigmaO1"=0.5, "SigmaO2"=0.5, "SigmaE1"=0.1, "SigmaE2"=0.1,
  # "SigmaV1"=0, "SigmaV2"=0, "SigmaVY1"=0, "SigmaVY2"=0,
  # "Range1"=1000, "Range2"=500, "SigmaM"=1, "ObsModel"=c(2,0),
  # "Nages"=1, "M"=Inf, "K"=Inf, "Linf"=1, "W_alpha"=1, "W_beta"=3,
  # "Selex_A50_mean"=0, "Selex_A50_sd"=0, "Selex_Aslope"=Inf)
  # todo: add settings that are not specified

  conditioning <- globalsettings
  conditioning$folder <- conditiondir
  env1 <- new.env()
  load(dir(conditiondir, pattern = "parameter_estimates.RData",
    full.names = TRUE), envir = env1)
  load(dir(conditiondir, pattern = "^Save.RData",
    full.names = TRUE), envir = env1)
  pars <- (get("parameter_estimates", envir = env1)$par)
  allpar <- get("Obj", envir = env1)$env$parList()

  # Get mean and sd of the intercepts
  # todo: use a vector to specify which to change or some other way where
  # you can specify what parameters are changed if they are a certain value
  # or if they are conditioned. The code below works, but the code could have
  # more checks.
  conditioning$beta1_mean <- mean(pars[grep("beta1", names(pars))])
  conditioning$beta2_mean <- mean(pars[grep("beta2", names(pars))])
  conditioning$beta1_sd <- sd(pars[grep("beta1", names(pars))])
  conditioning$beta2_sd <- sd(pars[grep("beta2", names(pars))])
  # Get observation error
  conditioning$SigmaM <- exp(pars[grep("logSigmaM", names(pars))])
  # Get linear covariate
  conditioning$Depth1_km <- mean(allpar$gamma1_ctp[, , 1])
  conditioning$Depth2_km <- mean(allpar$gamma2_ctp[, , 1])
  # Set the depth-squared term if it was conditioned for or a second covariate
  if (dim(allpar$gamma1_ctp)[3] > 1) {
    conditioning$Depth1_km2 <- mean(allpar$gamma1_ctp[, , 2])
    conditioning$Depth2_km2 <- mean(allpar$gamma2_ctp[, , 2])
  }
  # Define spatial and spatio-temporal variation
  # I'm using lower values than observed so that its less likely to have replicates with 0% or 100% encounter rates
  conditioning[["SigmaO1"]] <-
    ifelse("SigmaO1" %in% globalsettings$changepar,
      abs(allpar$L_omega1_z), globalsettings[["SigmaO1"]])
  conditioning[["SigmaO2"]] <-
    ifelse("SigmaO2" %in% globalsettings$changepar,
      abs(allpar$L_omega2_z), globalsettings[["SigmaO2"]])
  conditioning[["SigmaE1"]] <-
    ifelse("SigmaE1" %in% globalsettings$changepar,
      abs(allpar$L_epsilon1_z), globalsettings[["SigmaE1"]])
  conditioning[["SigmaE2"]] <-
    ifelse("SigmaE2" %in% globalsettings$changepar,
      abs(allpar$L_epsilon2_z), globalsettings[["SigmaE2"]])
  conditioning[["Range1"]] <-
    ifelse("Range1" %in% globalsettings$changepar,
      get("Report", envir = env1)$Range_raw1, globalsettings[["Range1"]])
  conditioning[["Range2"]] <-
    ifelse("Range2" %in% globalsettings$changepar,
      get("Report", envir = env1)$Range_raw2, globalsettings[["Range2"]])

  ignore <- dir(conditiondir, pattern = "OM")
  if (length(ignore) == 0) {
    omdir <- file.path(conditiondir, "01OM")
  } else {
    omdir <- file.path(conditiondir, paste0(formatC(as.numeric(
      strsplit(tail(ignore, 1), "_|OM")[[1]][1]) + 1,
      width = 2, flag = 0), "OM"))
  }

  VAST_OM(reps = 1:conditioning$replicates,
    dir = omdir, conditioning = conditioning, ncluster = n_cluster)

  # 03 EM
  VAST_EM(reps = 1:conditioning$replicates, settings = conditioning,
    directory = omdir, n_cluster = n_cluster, getdatafrom = datadir)

  # 04 Check that there are converged replicates
  # Run more replicates if not the max number of desired replicates
  while(sum(!apply(plyr::rbind.fill.matrix(lapply(sapply(dir(omdir,
    pattern = "[[:digit:]]+", full.names = TRUE), dir, pattern = "parameter_estimates.txt",
    recursive = TRUE), matrix, nrow = 1)), 1, anyNA)) <
    max(conditioning$replicates)) {
    nn <- sum(!apply(plyr::rbind.fill.matrix(lapply(sapply(dir(omdir,
      pattern = "[[:digit:]]+", full.names = TRUE), dir,
      pattern = "parameter_estimates.txt",
      recursive = TRUE), matrix, nrow = 1)), 1, anyNA))
    maxrep <- length(dir(omdir,
      pattern = "[[:digit:]]+", full.names = TRUE))
    needrep <- (maxrep + 1):(maxrep + 1 + conditioning$replicates - nn)
    VAST_OM(reps = needrep,
      dir = omdir, conditioning = conditioning, ncluster = n_cluster)
    VAST_EM(reps = needrep, settings = conditioning,
      directory = omdir, n_cluster = n_cluster, getdatafrom = datadir)
  }

  # 05 Read in the results
  allframe <- get_results(omdir = omdir, write2disk = TRUE)

  return(allframe)

}
