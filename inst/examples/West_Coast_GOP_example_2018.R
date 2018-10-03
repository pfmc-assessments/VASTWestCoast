# head of file

head(VAST_fields)
colnames(VAST_fields)

#' @param data A data frame of WCOP data.
#' @param species A vector of character entries that specify the 
#' species, using scientific names, that you want to keep.
#' @param gear A vector of character entries that specify the gear
#' types you want to keep.
#' 
cleanWCGOP <- function(data, species, gear) {
  # Notes
  # access to start and end locations, which could be helpful for
  # giving the amount of area covered.  
  # "HAUL_DURATION"  (Up - Set) * 24
  # todo: think about how this unit of measurement would be different
  # than if one assumed that the correction is in terms of space rather than time
  # Also could get names of the vessels to estimate the random effect of vessel
  gear <- tolower(gear)
  data$GEAR_TYPE <- tolower(data$GEAR_TYPE)
  if ()
  data <- data[data$GEAR_TYPE %in% gear, ]
  new <- NULL
  if (!is.list(species)) {
  	names(species) <- species
  	species <- list(species)
  }
  # todo: make a long data frame with an additional column if the CATCH_KG
  # is from retrained or discarded so that you can combine the two columns
  # of spp and this additional column to have category be retained vs. discard.
  for (ii in seq_along(species)) {
		data[data$SCIENTIFIC_NAME %in% species[[ii]], "SCIENTIFIC_NAME"] <- 
		  names(species)[ii]
		xx <- data[!data$SCIENTIFIC_NAME %in% species[[ii]], ]
    xx$SCIENTIFIC_NAME <- names(species)[ii]
    xx[, c("DIS_MT", "RET_MT")] <- 0
		new <- rbind(new, xx)
    rm(xx)
	}
  data <- rbind(data, new)
  data$Catch_KG <- (data$DIS_MT + data$RET_MT)  * 1000
  data <- data[data$SCIENTIFIC_NAME %in% names(species), ]
  data <- data[order(data$Catch_KG, decreasing = TRUE), ]
  data <- data[!duplicated(with(data, paste(SET_YEAR, HAUL_ID, SCIENTIFIC_NAME))), ]
  # todo: go off both scientific and SCIENTFIC
  data <- data.frame(
    "Catch_KG" = data$Catch_KG,
    "Retained_KG" = data$RET_MT * 1000,
    "Year" = data$SET_YEAR,
    "Vessel" = 1,
    "AreaSwept_km2" = data$HAUL_DURATION,
    "Lat" = data$AVG_LAT,
    "Lon" = data$AVG_LONG,
    "Species" = data$SCIENTIFIC_NAME)
  data <- data[data$AreaSwept_km2 != 0, ]
  invisible(data)
}


# Want to look at different proportions of skate  per state.
# each state has their own record of total skate landings, and we will then be 
# able to see the different answers per year by state or all together and then 
# groundtruth based on the state-specific metrics

Options <- c("SD_site_density"=1, "SD_site_logdensity"=1, 
	"Calculate_Range"=1, "Calculate_evenness"=1, 
	"Calculate_effective_area"=1, "Calculate_Cov_SE"=1, 
	'Calculate_Synchrony'=1, 'Calculate_Coherence'=0, 
	'normalize_GMRF_in_CPP'=TRUE)
RhoConfig <- c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) 
OverdispersionConfig <- c("Eta1"=0, "Eta2"=0)
ObsModel <- c(2,0)

OverdispersionConfig <- c("Eta1"=0, "Eta2"=0)
strata.limits <- data.frame(
  'STRATA' = c("Coastwide","CA","OR","WA"),
  'north_border' = c(49.0, 42.0, 46.0, 49.0),
  'south_border' = c(32.0, 32.0, 42.0, 46.0),
  'shallow_border' = c(55, 55, 55, 55),
  'deep_border' = c(1280, 1280, 1280, 1280)
)
Region <- "California_current"
Extrapolation_List <- make_extrapolation_info( Region=Region, strata.limits=strata.limits )
Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 ) 
rockfishdata <- Data_Geostat

Data_Geostat <- cleanWCGOP(VAST_fields,
	species = list(
		"Raja rhina" = "Raja rhina", 
		"Raja binoculata" = "Raja binoculata",
		"Other skate" = unique(VAST_fields$SCIENTIFIC_NAME[grep("^(?=.*skate)(?!.+nose.+)(?!big.+)", 
			VAST_fields$COMMON_NAME, ignore.case = TRUE, perl = TRUE)])),
	gear = "TRAWL")
nunique <- length(unique(Data_Geostat$Species))



Spatial_List = make_spatial_info( grid_size_km=grid_size_km, n_x=n_x, 
	Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], 
	Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], 
	nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], 
	DirPath=DateFile, Save_Results=FALSE)

# Add knots to Data_Geostat
Data_Geostat <- cbind(Data_Geostat, "knot_i"=Spatial_List$knot_i)

FieldConfig <- c("Omega1"=nunique, "Epsilon1"=nunique, "Omega2"=nunique, "Epsilon2"=nunique) 
TmbData = Data_Fn("Version"=Version, 
	"FieldConfig"=FieldConfig, "OverdispersionConfig"=OverdispersionConfig, "RhoConfig"=RhoConfig,
	"ObsModel"=ObsModel, 
	# "c_i"=rep(0,nrow(Data_Geostat)), 
	"c_i"=as.numeric(Data_Geostat[,'Species'])-1,
	"b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], 
	"v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, 
	"s_i"=Data_Geostat[,'knot_i']-1, 
	"t_i"=Data_Geostat[,'Year'], 
	"a_xl"=Spatial_List$a_xl, 
	"MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, 
	"Method"=Spatial_List$Method, 
  # todo: change the options
	"Options"=Options )
TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x, "Method"=Method)
Obj = TmbList[["Obj"]]
Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=DateFile, bias.correct=TRUE, newtonsteps=1, bias.correct.control=list(sd=FALSE, split=NULL, nsplit=1, vars_to_correct="Index_cyl") )
Report = Obj$report()
Save = list("Opt"=Opt, "Report"=Report, "ParHat"=Obj$env$parList(Opt$par), "TmbData"=TmbData)
save(Save, file=paste0(DateFile,"Save.RData"))


plot_data(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=DateFile )
MapDetails_List = make_map_info( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
# Decide which years to plot                                                   
Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))

Index <- plot_biomass_index(
	TmbData = TmbData, Sdreport = Report, 
	Year_Set=Year_Set, Years2Include=Years2Include, 
	DirName=DateFile, PlotName="Index", interval_width=1,
  strata_names=NULL, category_names=NULL, use_biascorr=FALSE, plot_legend=TRUE, 
  total_area_km2=NULL, plot_log=FALSE, width=4, height=4,
  treat_missing_as_zero=FALSE, create_covariance_table=FALSE)
Dens_xt = plot_maps(plot_set=c(3), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)
calculate_proportion(TmbData, Index, 
	Year_Set=NULL, Years2Include=NULL, strata_names=NULL, 
	category_names=NULL, plot_legend=TRUE,
  DirName=DateFile, 
  PlotName="Proportion.png", 
  interval_width=1, width=6, height=6)