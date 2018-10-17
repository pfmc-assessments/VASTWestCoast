#' Run Diagnostic Plots for VASTWestCoast Output
#' 
#' @param dir The directory where the output from 
#' \code{\link{VAST_condition}} is saved. Can be a relative
#' or an absolute path.
#' 
#' @export
#' @author Kelli Faye Johnson
#' 
#' @return todo: Make a list of the tables and figures that are 
#' saved to the disk from VAST_diagnostics
#' 
VAST_diagnostics <- function(dir = getwd()) {
  # Load the saved file
  savedfile <- dir(dir, pattern = "^Save.RData", full.names = TRUE)
  datafile <- dir(dir, pattern = "DatabaseSave.RData", full.names = TRUE)
  setupfile <- dir(dir, pattern = "setup.RData", full.names = TRUE)
  base::load(savedfile)
  base::load(datafile)
  base::load(setupfile)

  region <- info$region
  if(is.null(region)) {
    region <- switch(
      strsplit(settings$Species, "_")[[1]][1],
      EBSBTS = "eastern_bering_sea",
      WCGBTS = "California_current",
      NULL)
  }

  # Check convergence
  sink(file.path(dir, "convergence_gradient.txt"))
  pander::pandoc.table(
    Opt$diagnostics[,
      c("Param","Lower","MLE","Upper","final_gradient")])
  sink()
  # Check encounter probability
  encprob <- FishStatsUtils::plot_encounter_diagnostic(
    Report = Report,
    Data_Geostat = Database,
    DirName = dir)

  # Check positive catch rates
  # Won't work for Poisson-link
  Q <- FishStatsUtils::plot_quantile_diagnostic(
    TmbData = TmbData,
    Report = Report,
    FileName_PP = "Posterior_Predictive",
    FileName_Phist = "Posterior_Predictive-Histogram",
    FileName_QQ = "Q-Q_plot",
    FileName_Qhist = "Q-Q_hist",
    DateFile = dir)
  MapDetails_List <- FishStatsUtils::make_map_info(
    "Region" = region,
    "NN_Extrap" = info$Spatial_List$NN_Extrap,
    "Extrapolation_List" = info$Extrapolation_List)
  years <- Database$Year
  Year_Set <- seq(min(years), max(years))
  Years2Include <- which(Year_Set %in% sort(unique(years)))
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
    mar = c(0, 0, 2, 0), oma = c(3.5, 3.5 ,0, 0), cex = 1.8)

  # Plot anisotropy
  FishStatsUtils::plot_anisotropy(
    FileName = file.path(dir, "Aniso.png"),
    TmbData = TmbData,
    Report = Report)

  # Spatial and spatiotemporal covariance
  ncats <- length(unique(Database[, grep("Sci", colnames(Database))[1]]))
  if (ncats > 1){
    Cov_List <- VAST::Summarize_Covariance(
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
    FileName = paste0(dir, .Platform$path.sep),
    Year_Set = Year_Set, Years2Include = Years2Include,
    mar = c(0, 0, 2, 0), oma = c(3.5, 3.5, 0, 0),
    cex = 1.8, plot_legend_fig = FALSE)
  Dens_DF <- cbind(
    "Density" = as.vector(Dens_xt),
    "Year" = Year_Set[col(Dens_xt)],
    "E_km" = info$Spatial_List$MeshList$loc_x[row(Dens_xt), "E_km"],
    "N_km" = info$Spatial_List$MeshList$loc_x[row(Dens_xt), "N_km"])
  sink(file = file.path(dir, "densityperknot.txt"))
    pander::pandoc.table(Dens_DF, digits = 3)
  sink()

  FishStatsUtils::plot_data(
    Extrapolation_List = info$Extrapolation_List,
    Spatial_List = info$Spatial_List, 
    Data_Geostat = Database, PlotDir = dir)

  # Range shifts
  if (!is.null(Opt[["SD"]])) {
  if (TmbData$Options["Calculate_effective_area"] == 1) {
    FishStatsUtils::plot_range_index(
      Report = Report,
      TmbData = TmbData,
      Sdreport = Opt[["SD"]],
      Znames = colnames(Report$Z_xm),
      PlotDir = paste0(dir, .Platform$file.sep),
      Year_Set = Year_Set)
  }}
  invisible()
}
