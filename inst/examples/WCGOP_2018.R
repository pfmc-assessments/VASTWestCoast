devtools::install_github("James-Thorson/VAST")
devtools::install_github("James-Thorson/FishStatsUtils")
devtools::install_github("pfmc-assessments/VASTWestCoast")
library(VASTWestCoast)
# This example only works if you load your own data into
# an object called VAST_fields
load("VAST_SkateData.RData")
specieslist <- list(
    "Raja rhina" = "Raja rhina",
    "Other skate" = unique(VAST_fields$SCIENTIFIC_NAME[grep("^(?=.*skate)(?!.+nose.+)",
      VAST_fields$COMMON_NAME, ignore.case = TRUE, perl = TRUE)]))
VAST_fields <- subset(VAST_fields, SET_YEAR >= 2009)
# Need a more biological-thinking method to subset the data
VAST_fields <- subset(VAST_fields, 
  SCIENTIFIC_NAME %in% unlist(specieslist) &
  DIS_MT > 0.15)
VAST_fields <- WCGOP_clean(VAST_fields,
  species = specieslist,
  gear = "TRAWL")
dim(VAST_fields); table(VAST_fields$Year)

Sim_Settings <- list(
  "Species" = "WCGOP_skate",
  "ObsModelcondition" = c(2, 0),
  "nknots" = 50,
  "strata" = data.frame("STRATA" = "All_areas"),
  "depth" = c("no", "linear", "squared")[1],
  "Passcondition" = FALSE)

downloaddir <- getwd()

test <- VAST_condition(
  conditiondir = downloaddir,
  settings = Sim_Settings, spp = Sim_Settings$Species,
  datadir = downloaddir,
  overdispersion = NULL,
  data = VAST_fields)

VAST_diagnostics(downloaddir)

load(file.path(downloaddir, "Save.RData"))

calculate_proportion(TmbData, Index, 
  Year_Set=NULL, Years2Include=NULL, 
  strata_names=NULL, category_names=NULL, 
  plot_legend=TRUE,
  DirName = paste0(downloaddir,"/"), 
  PlotName="Proportion.png", interval_width=1, width=6, height=6)
