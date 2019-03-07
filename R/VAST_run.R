#' Estimate parameters from a spatiotemporal index-standardization model
#'
#' @param datalist A list of data used for the model.
#' @param depth A character value of either no, linear, or squared specifying how
#' the covariate should be included in the model.
#' @param overdispersion A vector of length two that specifies how overdispersion
#' is modelled. Typical options include \code{c("eta1"=0, "eta2"=0)} and
#' \code{c("Delta1"=1, "Delta2"=1)}, where the latter is for the US West Coast
#' and the former is for Alaska.
#' @param obsmodel A numerical vector of length two that specifies the distributions
#' for the observation and rate models. No default is specified, but the typical value
#' used on the US West Coast is \code{c(2, 0)}.
#' @param rundir A directory where the \code{.cpp} file will be compiled to.
#' @param Version The version of the \code{.cpp} file to use.
#' @param calcs A vector of length one or nine specifying which calculations
#' to perform. If a single integer of zero or one is given then this value will be used
#' for all eight calculations. Zero turns them off and one turns them on.
#' @param strata Strata specifications for calculating the index of abundance.
#' @param pass A logical value specifying if Pass should be include to estimate
#' catchability in the model. The default is to not use pass, i.e., \code{pass = FALSE}.
#' @param savefile The file name that you want the results saved to. A full path can
#' be specified, else the \code{.RData} file will be saved in the current directory.
#' @param field A vector of length four or one, where the latter situation 
#' will result in the value being repeated four times, 
#' to specify the number of factors or structure for spatial and spatiotemporal
#' effects for each model in turn, i.e.,
#' Omega1, Epsilon1, Omega2, and Epsilon2. The vector need not be named because
#' the names will be assigned in that order.
#' The default is a single value of \code{NULL} that results in searching through
#' the data for the appropriate configuration.
#' todo: Place more details in 'Details' section.
#' @param rho A vector of length four or one, where the latter situation 
#' will result in the value being repeated four times, 
#' to specify whether either intercepts (Beta1 and Beta2) and 
#' spatio-temporal variation (Epsilon1 and Epsilon2) are structured among 
#' time intervals. Options include \code{0:4}, where
#' if 0, then each year is modelled as a fixed effect; 
#' if 1, then each year is modelled as random following IID distribution; 
#' if 2, then each year is modelled as random following a random walk; 
#' if 3, then constant among years as fixed effect; and
#' if 4, then each year is modelled as random following an AR1 process).
#' todo: Place more details in 'Details' section.
#' @param comp A logical value specifying if the data are composition data.
#' The default is \code{FALSE}, where the data are assumed to be catch-rate data
#' used for an index of abundance rather than composition data.
#' 
#' @return Nothing is returned directly, instead files are saved to the disk.
#'
#' @import FishStatsUtils
#' @import TMB
#' @import VAST
#' @importFrom TMBhelper Optimize
#' @importFrom utils tail
#'
#' @author Kelli Faye Johnson
#' @export
#'
VAST_run <- function(datalist, depth = c("no", "linear", "squared"),
  overdispersion, obsmodel, rundir,
  Version = NULL, calcs = c(1, rep(0, 6), 0, 1),
  strata, pass = FALSE, savefile,
  field = NULL, rho = NULL, comp = FALSE) {

  if (!exists("compile", where = globalenv()) &&
      !any(grepl("^package:TMB$|^package:TMBdebug$", search()))) {
    e <- new.env()
    e$compile <- TMB::compile
    e$MakeADFun <- TMB::MakeADFun
    base::attach(e, name = "VASTWestCoast_shims", warn.conflicts = FALSE)
  }

  if (is.null(Version)) Version <- gsub("\\.cpp", "",
    utils::tail(list.files(R.home(
    file.path("library", "VAST", "executables"))), 1))
  if (depth == "FALSE") depth <- "no"
  depth <- match.arg(depth)
  if (length(calcs) == 1) calcs <- rep(calcs, 9)

  Options <- c(
    "SD_site_density" = 0, 
    "SD_site_logdensity" = 0, 
    "Calculate_Range" = 0, 
    "SD_observation_density" = 0, 
    "Calculate_effective_area" = 0,
    "Calculate_Cov_SE" = 0, 
    "Calculate_Synchrony" = 0, 
    "Calculate_Coherence" = 0, 
    "Calculate_proportion" = 0)
  Options[1] <- calcs[1]
  Options[2] <- calcs[2]
  Options[3] <- calcs[3]
  Options[4] <- calcs[4]
  Options[5] <- calcs[5]
  Options[6] <- calcs[6]
  # Options[7] <- calcs[7]
  Options[8] <- calcs[8]
  Options[9] <- calcs[9]
  if (grepl("_v4", Version)) Options <- c("Calculate_Range" = 0)

  RhoConfig <- c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)
  if (!is.null(rho)) {
    if (length(rho) == 1) rho <- rep(rho, 4)
      RhoConfig[1] <- rho[1]
      RhoConfig[2] <- rho[2]
      RhoConfig[3] <- rho[3]
      RhoConfig[4] <- rho[4]
  }

  depthdatahere <- switch(as.character(depth),
    no = NULL,
    squared = datalist$X_xtp,
    linear = array(data = datalist$X_xtp[, , 1],
      dim = c(dim(datalist$X_xtp)[1:2], 1)))

  if (pass) {
    if (!"Pass" %in% colnames(datalist$data)) {
      stop("Pass is not in the data frame passed to TMB",
        " but the settings say to use Pass.")
    } else {
      qdatahere <- as.matrix(datalist$data[, "Pass", drop = FALSE])
    }
  } else {qdatahere <- NULL}

  # Make TMB data list
  if (!"sci" %in% tolower(colnames(datalist$data))) {
    datalist$data <- cbind(datalist$data, "Sci" = 1)
  }

  # Specify FieldConfig for spatial and spatiotemporal variation
  
  # Check for missing years b/c one may want to inform them with AR1
  nfactors <- length(unique(datalist$data$Sci))
  if (is.null(field[1])) {
    ####
    if (comp) FieldConfig <- rep("IID", 4)
  } else {
    FieldConfig <- if (length(field) == 1) rep(field, 4) else field
  }
  if (!exists("FieldConfig")) {
    FieldConfig <- rep(nfactors, 4)
  }
  names(FieldConfig) <- c("Omega1", "Epsilon1", "Omega2", "Epsilon2")
  if (!is.factor(datalist$data$Sci)) {
    datalist$data$Sci <- as.factor(datalist$data$Sci)
  }

  TmbData <- suppressWarnings(VAST::Data_Fn(
    "Version" = Version,
    # An array
    "X_xtp" = depthdatahere,
    # 1=Presence-absence; 2=Density given presence;
    # Epsilon=Spatio-temporal; Omega=Spatial
    "FieldConfig" = FieldConfig,
    # Vessel-year effects for
    # Default is c("eta1" = 0, "eta2" = 0)
    "OverdispersionConfig" = overdispersion,
    # Structure for beta or epsilon over time:
    # 0=None (default); 1=WhiteNoise; 2=RandomWalk; 3=Constant
    "RhoConfig" = RhoConfig,
    "ObsModel" = obsmodel,
    "c_i" = as.numeric(datalist$data[, "Sci"]) - 1,
    "b_i" = datalist$data[, "Catch_KG"],
    "a_i" = datalist$data[, "AreaSwept_km2"],
    "v_i" = as.numeric(datalist$data[, "Vessel"]) - 1,
    "s_i" = datalist$data[, "knot_i"] - 1,
    "t_i" = datalist$data[, "Year"],
    "a_xl" = datalist$Spatial_List$a_xl,
    # Catchability variable with rows equal to the number of observations and
    # columns equal to the number of variables you want to include
    # resulting in a design matrix with 0 for the reference case.
    # The default is NULL
    "Q_ik" = qdatahere,
    "MeshList" = datalist$Spatial_List$MeshList,
    "GridList" = datalist$Spatial_List$GridList,
    "Method" = datalist$Spatial_List$Method,
    "Options" = Options))

  # Make TMB object
  TmbList = VAST::Build_TMB_Fn("TmbData" = TmbData, "RunDir" = rundir,
    "Version" = Version,
    "RhoConfig" = RhoConfig,
    "loc_x" = datalist$Spatial_List$loc_x,
    "Method" = datalist$Spatial_List$Method)
  # TmbList$Parameters$L1_z <- 0
   # TmbList$Parameters$logSigmaM[1] <- log(0.25)
  # TmbList$Map$L1_z <- factor(NA)
  # TmbList$Map$logSigmaM <- factor(rep(NA, 3))
  # TmbList = VAST::Build_TMB_Fn("TmbData" = TmbData, "RunDir" = rundir,
  #   "Version" = Version,
  #   "Parameters" = TmbList$Parameters,
  #   "Map" = TmbList$Map,
  #   "RhoConfig" = c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0),
  #   "loc_x" = datalist$Spatial_List$loc_x)

  # Run model
  dobiason <- ifelse(length(unique(datalist$data[, "Sci"])) > 1,
    "Index_xcyl", "Index_cyl")
  dobias <- TRUE
  Obj <- TmbList[["Obj"]]
  Opt <- tryCatch(TMBhelper::Optimize(
    obj = Obj,
    lower = TmbList[["Lower"]], upper = TmbList[["Upper"]],
    getsd = TRUE, savedir = dirname(savefile), newtonsteps = 1,
    bias.correct = dobias,
    bias.correct.control = list(sd = FALSE, split = NULL,
      nsplit = 1, vars_to_correct = dobiason)
    ), error = function(e) e)
  Report <- Obj$report()
  error <- NULL
  if ("simpleError" %in% class(Opt)) {
    error <- Opt$message
  }

  # Plot index
  if (!grepl("likely not converged", Opt[[1]]["Convergence_check"])) {
    ParHat <- Obj$env$parList()
    AIC <- Opt$AIC
    sdreport <- Opt[["SD"]]

    if (!is.null(Opt[["SD"]])) {
      Index <- tryCatch(FishStatsUtils::plot_biomass_index(
        DirName = dirname(savefile), TmbData = TmbData,
        Sdreport = Opt[["SD"]],
        Year_Set = seq(min(datalist$data[, "Year"]), max(datalist$data[, "Year"])),
        strata_names = strata[, 1], use_biascorr = dobias),
        error = function(e) e)
      sdinfo <- TMB::sdreport(Obj)
      tables <- summary_nwfsc(obj = Obj, sdreport = sdinfo,
        savedir = dirname(savefile))
    } else {
      Index <- NULL
      tables <- NULL
    }
  } else {
    Index <- NULL
    ParHat <- NULL
    AIC <- NULL
    sdreport <- NULL
    tables <- NULL
  }

  save(AIC, Index, Obj, Opt, ParHat, Report, sdreport, TmbData, TmbList,
    tables, error,
    file = savefile)
}
