#' Run Diagnostic Plots for VASTWestCoast Output
#' 
#' @param dir The directory where the output from 
#' \code{\link{VAST_condition}} is saved. Can be a relative
#' or an absolute path.
#' 
#' @export
#' @author Kelli Faye Johnson
#' @import FishStatsUtils
#' @importFrom pander pandoc.table
#' @importFrom VAST Summarize_Covariance
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
  setupfile <- dir(dir, pattern = "setup.RData", full.names = TRUE)
  if (length(savedfile) == 0) stop("The file Save.RData doesn't exist")
  if (length(datafile) == 0) stop("The file DatabaseSave.RData doesn't exist")
  if (length(setupfile) == 0) stop("The file setup.RData doesn't exist")
  base::load(savedfile)
  base::load(datafile)
  base::load(setupfile)

  if (!is.null(Opt$message)) {
    warning("The warning message from the optimization",
      " routine indicates the model \nmight not be converged. Check the following",
  	  " message:\n", Opt$message)
    return(NULL)
  }
  if (length(Opt[["opt"]][["Convergence_check"]]) > 0 | is.null(Opt[["SD"]])) {
    warning("The model in, ", dir, " appears to have found a solution but didn't produce a hessian")
    return(NULL)
  }
  if (is.null(info$region)) info$region <- "california_current"

  # Check convergence
  cat(file = file.path(dir, "convergence_gradient.txt"), 
    pander::pandoc.table.return(
      Opt$diagnostics[,
        c("Param","Lower","MLE","Upper","final_gradient")]))
  #browser()
  # Check encounter probability
  FishStatsUtils::plot_encounter_diagnostic(
    Report = Report,
    Data_Geostat = Database,
    DirName = dir)

  Q <- FishStatsUtils::plot_quantile_diagnostic(
    TmbData = TmbData,
    Report = Report,
    FileName_PP = "Posterior_Predictive",
    FileName_Phist = "Posterior_Predictive-Histogram",
    FileName_QQ = "Q-Q_plot",
    FileName_Qhist = "Q-Q_hist",
    DateFile = dir)
  MapDetails_List <- FishStatsUtils::make_map_info(
    spatial_list = info$Spatial_List,
    "Region" = info$region,
    "NN_Extrap" = info$Spatial_List$NN_Extrap,
    "Extrapolation_List" = info$Extrapolation_List,
    fine_scale = FALSE)
  years <- Database$Year
  Year_Set <- seq(min(years), max(years))
  Years2Include <- which(Year_Set %in% sort(unique(years)))

  plot_biomass_index(TmbData, sdreport, 
    Year_Set = Year_Set, Years2Include = Years2Include, 
    DirName = dir,
    PlotName = "timeseries",
    strata_names = if(nrow(settings$strata) == 1) {""} else {settings$strata[,1]})
  FishStatsUtils::plot_residuals(
    Lat_i = Database$Lat,
    Lon_i = Database$Lon,
    TmbData = TmbData,
    Report = Report,
    Q = Q, savedir = dir, FileName = paste0(dir, .Platform$path.sep),
    MappingDetails = MapDetails_List[["MappingDetails"]],
    PlotDF = MapDetails_List[["PlotDF"]],
    MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
    Xlim = MapDetails_List[["Xlim"]],
    Ylim = MapDetails_List[["Ylim"]],
    Year_Set = Year_Set, Years2Include = Years2Include,
    Rotate = MapDetails_List[["Rotate"]],
    Cex = MapDetails_List[["Cex"]], Legend = MapDetails_List[["Legend"]],
    zone = MapDetails_List[["Zone"]],
    mar = c(0, 0, 2, 0), oma = c(3.5, 3.5 ,0, 0), cex = 0.8)

  # Plot anisotropy
  FishStatsUtils::plot_anisotropy(
    FileName = file.path(dir, "Aniso.png"),
    TmbData = TmbData,
    Report = Report)

  # Spatial and spatiotemporal covariance
  ncats <- length(unique(Database[, grep("Sci", colnames(Database))[1]]))
  if (ncats > 1){
    VAST::Summarize_Covariance(
      Report = Report,
      ParHat = ParHat,
      Data = TmbData,
      SD = Opt$SD,
      plot_cor = FALSE,
      category_names = levels(Database$Sci),
      plotdir = dir, plotTF = TmbData$FieldConfig,
      mgp = c(2, 0.5, 0), tck = -0.02, oma = c(0, 5, 2, 2))
  }

  # Density surface
  Dens_xt <- FishStatsUtils::plot_maps(
    plot_set = c(3),
    MappingDetails = MapDetails_List[["MappingDetails"]],
    PlotDF = MapDetails_List[["PlotDF"]],
    MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
    Xlim = MapDetails_List[["Xlim"]], Ylim = MapDetails_List[["Ylim"]],
    Rotate = MapDetails_List[["Rotate"]],
    Cex = MapDetails_List[["Cex"]],
    Legend = MapDetails_List[["Legend"]],
    zone = MapDetails_List[["Zone"]],
    Report = Report,
    Sdreport = Opt$SD,
    FileName = paste0(dir, .Platform$file.sep),
    Year_Set = Year_Set, Years2Include = Years2Include,
    mar = c(0, 0, 2, 0), oma = c(3.5, 3.5, 0, 0),
    cex = 1.8, plot_legend_fig = FALSE)
  Dens_DF <- cbind(
    "Density" = as.vector(Dens_xt),
    "Year" = Year_Set[col(Dens_xt)],
    "E_km" = info$Spatial_List$MeshList$loc_x[row(Dens_xt), "E_km"],
    "N_km" = info$Spatial_List$MeshList$loc_x[row(Dens_xt), "N_km"])
  cat(file = file.path(dir, "densityperknot.txt"),
    pander::pandoc.table.return(Dens_DF, digits = 3))

  FishStatsUtils::plot_data(
    Extrapolation_List = info$Extrapolation_List,
    Spatial_List = info$Spatial_List, 
    Data_Geostat = Database, PlotDir = paste0(dir, .Platform$file.sep))

  # Range shifts
  if (!is.null(Opt[["SD"]])) {
    check <- FALSE
    if ("Calculate_effective_area" %in% names(TmbData$Options)) {
      check <- ifelse(TmbData$Options["Calculate_effective_area"] == 1,
        TRUE, check)
    }
    if ("Options" %in% names(TmbData$Options)) {
      check <- ifelse(TmbData$Options$Options["Calculate_effective_area"] == 1,
        TRUE, check)
    }
    if (check) {
      FishStatsUtils::plot_range_index(
        Report = Report,
        TmbData = TmbData,
        Sdreport = Opt[["SD"]],
        Znames = colnames(Report$Z_xm),
        PlotDir = paste0(dir, .Platform$file.sep),
        Year_Set = Year_Set)
    }
  }
  invisible()
}
