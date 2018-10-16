devtools::install_github("James-Thorson/VAST@development")
devtools::install_github("James-Thorson/FishStatsUtils")
devtools::install_github("nwfsc-assess/VASTWestCoast")
library(VASTWestCoast)
library(TMB)
# This example only works if you load your own data into
# an object called VAST_fields
load("VAST_SkateData.RData")
specieslist <- list(
    "Raja rhina" = "Raja rhina",
    "Raja binoculata" = "Raja binoculata",
    "Other skate" = unique(VAST_fields$SCIENTIFIC_NAME[grep("^(?=.*skate)(?!.+nose.+)(?!big.+)",
      VAST_fields$COMMON_NAME, ignore.case = TRUE, perl = TRUE)]))
VAST_fields <- subset(VAST_fields, 
  SCIENTIFIC_NAME %in% unlist(specieslist[1:2]) &
  DIS_MT > 0.15 & SET_YEAR < 2007)
VAST_fields <- WCGOP_clean(VAST_fields,
  species = specieslist[1:2],
  gear = "TRAWL")
dim(VAST_fields); table(VAST_fields$Year)

Sim_Settings <- list(
  "Species" = "WCGOP_skate",
  "ObsModelcondition" = c(2, 0),
  "nknots" = 50,
  "strata" = data.frame("STRATA" = "All_areas"),
  "depth" = c("no", "linear", "squared")[1],
  "version" = "VAST_v5_2_0",
  "Passcondition" = FALSE)

downloaddir <- getwd()

test <- VAST_condition(
  conditiondir = downloaddir,
  settings = Sim_Settings, spp = Sim_Settings$Species,
  datadir = downloaddir,
  overdisperion = NULL,
  data = VAST_fields)
# Currently broken, but I will fix
# VAST_diagnostics(downloaddir)


# The below code will be removed.
FishStatsUtils::plot_data(
  Extrapolation_List = Extrapolation_List,
  Spatial_List = Spatial_List,
  Data_Geostat = Data_Geostat,
  PlotDir = savedir)
MapDetails_List <- FishStatsUtils::make_map_info(
  "Region" = Region, "NN_Extrap" = Spatial_List$PolygonList$NN_Extrap,
  "Extrapolation_List" = Extrapolation_List)
# Decide which years to plot
Year_Set <- seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include <- which(Year_Set %in% sort(unique(Data_Geostat[,'Year'])))

Index <- FishStatsUtils::plot_biomass_index(
  TmbData = TmbData, Sdreport = Report,
  Year_Set = Year_Set, Years2Include = Years2Include,
  DirName = savedir, PlotName = "Index", interval_width = 1,
  strata_names = NULL, category_names = NULL, use_biascorr = FALSE,
  plot_legend = TRUE,
  total_area_km2 = NULL, plot_log = FALSE, width = 4, height = 4,
  treat_missing_as_zero = FALSE, create_covariance_table = FALSE)
Dens_xt <- FishStatsUtils::plot_maps(plot_set = c(3),
  MappingDetails = MapDetails_List[["MappingDetails"]],
  Report = Reporta, Sdreport = Opt$SD,
  PlotDF = MapDetails_List[["PlotDF"]],
  MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
  Xlim = MapDetails_List[["Xlim"]], Ylim = MapDetails_List[["Ylim"]],
  FileName = savedir,
  Year_Set = Year_Set, Years2Include = Years2Include,
  Rotate = MapDetails_List[["Rotate"]],
  Cex = MapDetails_List[["Cex"]],
  Legend = MapDetails_List[["Legend"]],
  zone = MapDetails_List[["Zone"]],
  mar = c(0,0,2,0), oma = c(3.5,3.5,0,0),
  cex = 1.8, plot_legend_fig = FALSE)
VAST::calculate_proportion(TmbData, Index,
  Year_Set = NULL, Years2Include = NULL, strata_names = NULL,
  category_names = NULL, plot_legend = TRUE,
  DirName = savedir,
  PlotName = "Proportion.png",
  interval_width = 1, width = 6, height = 6)
