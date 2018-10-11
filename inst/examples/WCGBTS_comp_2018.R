
#######################################
#
# NOTES
# 1. REML
#   * Much faster using REML than ML
#   * REML causes positive bias for categories with high variance (i.e., low density) if used with bias-correction
#   * Sequenced TRUE then FALSE is unbiased plus faster
#
# 2. Calculating proportions internally with bias-correction is non-separable and very memory intensive
#   * Can avoid bias-corrected propportions using an approximation to total variance and proportions
#
# 3. Common Problems with convergence
#   * Loadings values go to zero, and then cause the nelder-mead to struggle --  FIX:  turn off loadings at zero
#   * SigmaM goes to crazy values (-Inf or Inf) -- Fix:  Fix low at -Inf, or for high fix all to be constant
#     * High SigmaM causes the bias-correction to cause positive bias for that category
#     * Also MAY improve inner-optimizer (i.e., ustep stays closer to one)
#     * Occurs mainly because some categories have very low sample sizes
#   * L_omega1_z and L_epsilon1_z also go to crazy values +/- 100
#     * Again, occurs because some categories have very low sample sizes
#     * Potential fix:  Define Npool, and fix omega/epsilon/SigmaM to be identical for all categories with Nencounter < Npool
#
# 3. Lognormal-Poisson *may* result in bias because mean of distribution doesn't equal average log-intensity
#
# 4. Lingcod example
#   * Poisson for Lingcod doesn't seem to converge even turning off annual intercepts, Omega, and Epsilon for component 1
#   * Aniso doesn't work with n_x=50 because the ellipse gets linear along the coastline
#   * The default starting values for the negative-binomial are terrible, and may have caused early convergence problems
#   * Many epsilon2 values go to zero, which messes up the optimizer
#
# 5. Pollock example
#   * Bias-correction causes memory issues on laptop with n_x=200, unless I use `nsplit` options
#
#######################################

# Install packages (including development version of TMB to allow selection of bias-correction)
devtools::install_github("james-thorson/utilities")
devtools::install_github("james-thorson/VAST")
remove.packages("TMB")
devtools::install_github("kaskr/adcomp/TMB")

RootFile = "C:/Users/James.Thorson/Desktop/UW Hideaway/Collaborations/2017 -- spatiotemporal comp expansion/"
  DataFile = paste0( RootFile,"Data/")
  TmbDir = system.file("executables",package="VAST")

library(TMB)               # Can instead load library(TMBdebug)
library(VAST)

source( "C:/Users/James.Thorson/Desktop/Project_git/VAST/R/calculate_proportion.R" )
#source( "C:/Users/James.Thorson/Desktop/Project_git/VAST/R/Data_Fn.R" )
#source( "C:/Users/James.Thorson/Desktop/Project_git/VAST/R/Param_Fn.R" )
#source( "C:/Users/James.Thorson/Desktop/Project_git/VAST/R/Make_Map.R" )
#source( "C:/Users/James.Thorson/Desktop/Project_git/VAST/R/Build_TMB_Fn.R" )
#TmbDir = "C:/Users/James.Thorson/Desktop/Project_git/VAST/inst/executables"

#Date = Sys.Date()
Date = "2018-04-26"
#Date = "2018-05-02"
Suffix = "Sim--REML=TthenF_Range=250_Prop=1"
#Suffix = "LingExpand_FbyFcm--REML=F_LN_Field=1111_Rho=0000_nx=100_Npool=100_strata=ORWA"
#Suffix = "LingExpand_Mby2cm--REML=F_LN_Field=1111_Rho=0000_nx=100_Npool=100_strata=ORWA"
  DateFile = paste0(RootFile,Date,'_',Suffix,'/')
# DateFile = paste0(RootFile,"2017-07-29_LingExpand_Fby6cm--REML=F_LNconst_Field=0011_Rho=0000_nx=200/")
# DateFile = paste0(RootFile,"2017-07-29_LingExpand_Mby6cm--REML=F_LNconst_Field=0011_Rho=0000_nx=200/")
  dir.create(DateFile)

if( file.exists(paste0(DateFile,"Record.RData")) ){
  load( file=file.path(DateFile,"Record.RData"))
  attach(Record)
  print("Loaded previous Record")
}else{
  Version = "VAST_v2_8_0"
  DataSet = c("Simulated", "Pollock", "Lingcod", "SummerFlounder", "Scallop")[1]
  # Scallop -- see C:\Users\James.Thorson\Desktop\UW Hideaway\NWFSC\2016-09 -- VAST HabCam demo
  Method = c("Grid", "Mesh", "Spherical_mesh")[2]
  grid_size_km = 25
  n_x = 50 # Number of stations
  Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )
  ObsModel_Sim = c(2,1)

  if(DataSet %in% c("Simulated","SummerFlounder")) c_set = NULL
  if(DataSet=="Pollock") c_set = c(0,20,30,40,50,Inf)
  #if(DataSet=="Lingcod") c_set = c(0,seq(10,130,by=6),Inf)
  if(DataSet=="Lingcod") c_set = c(0,seq(10,130,by=2),Inf)
  if(DataSet=="Scallop") c_set = seq(from=45, to=135, by=10)
  Sex = c("Male", "Female", "Combined")[2]

  Data_type = c("Numbers", "Weight", "Corrected_weight")[3]   # Only matters for pollock
  Use_First_Stage_Expansion = TRUE                             # Only matters for lingcod

  BiasCorr = TRUE
  Options =  c("SD_site_density"=0, "SD_site_logdensity"=0, "Calculate_Range"=0, "Calculate_evenness"=0, "Calculate_effective_area"=0, "Calculate_Cov_SE"=0, 'Calculate_Synchrony'=0, 'Calculate_Coherence'=0, 'Calculate_proportion'=0)

  # Simulation settings
  if(DataSet %in% c("Simulated","Pollock")){
    Survey_to_use = "EBS"
    Species = "Gadus chalcogrammus"
    FieldConfig = c("Omega1"="IID", "Epsilon1"="IID", "Omega2"="IID", "Epsilon2"="IID")
    ObsModel_Est = c(2,1)
    OverdispersionConfig = c("Delta1"=0, "Delta2"=0)
    Use_REML = "TRUE_then_FALSE"  # Much faster if TRUE, BUT CAUSES BIAS FOR PROPORTIONS WHEN USING BIAS-CORRECTION (DUE TO INFLATING LOW PROPORTIONS MORE THAN HIGH PROPORTIONS)
    Aniso = TRUE
    RhoConfig = c("Beta1"=0, "Epsilon1"=0, "Beta2"=0, "Epsilon2"=0)
    strata.limits = data.frame('STRATA'="All_areas")
    Npool = 100     # if a category has <Npool encounters, then pool it with others also having few
  }
  if(DataSet %in% c("Lingcod") & Use_First_Stage_Expansion==FALSE){
    Survey_to_use = "WCGBTS"
    Species = "Ophiodon elongatus"
    FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"="IID", "Epsilon2"=0)
    ObsModel_Est = c(5,0)
    OverdispersionConfig = c("Delta1"=0, "Delta2"=0)
    Use_REML = FALSE  # Much faster if TRUE, BUT CAUSES BIAS FOR PROPORTIONS WHEN USING BIAS-CORRECTION (DUE TO INFLATING LOW PROPORTIONS MORE THAN HIGH PROPORTIONS)
    Aniso = FALSE
    RhoConfig = c("Beta1"=3, "Epsilon1"=0, "Beta2"=0, "Epsilon2"=0)
    #strata.limits = data.frame('STRATA'="All_areas")
    strata.limits <- data.frame( 'STRATA' = "ORWA", 'north_border' = 49.0, 'south_border' = 42.0 )
    Npool = FALSE      # if a category has <Npool encounters, then pool it with others also having few
  }
  if(DataSet %in% c("Lingcod") & Use_First_Stage_Expansion==TRUE){
    Survey_to_use = "WCGBTS"
    Species = "Ophiodon elongatus"
    FieldConfig = c("Omega1"="IID", "Epsilon1"="IID", "Omega2"="IID", "Epsilon2"="IID")
    ObsModel_Est = c(2,0)
    OverdispersionConfig = c("Delta1"=0, "Delta2"=0)
    Use_REML = FALSE  # Much faster if TRUE, BUT CAUSES BIAS FOR PROPORTIONS WHEN USING BIAS-CORRECTION (DUE TO INFLATING LOW PROPORTIONS MORE THAN HIGH PROPORTIONS)
    Aniso = FALSE
    RhoConfig = c("Beta1"=0, "Epsilon1"=0, "Beta2"=0, "Epsilon2"=0)
    strata.limits = data.frame('STRATA'="All_areas")
    Npool = 100     # if a category has <Npool encounters, then pool it with others also having few
  }
  if(DataSet %in% c("SummerFlounder")){
    Survey_to_use = "NWA"
    Species = "Ophiodon elongatus"
    FieldConfig = c("Omega1"=0, "Epsilon1"=0, "Omega2"="IID", "Epsilon2"=0)
    ObsModel_Est = c(5,0)
    OverdispersionConfig = c("Delta1"=0, "Delta2"=0)
    Use_REML = FALSE  # Much faster if TRUE, BUT CAUSES BIAS FOR PROPORTIONS WHEN USING BIAS-CORRECTION (DUE TO INFLATING LOW PROPORTIONS MORE THAN HIGH PROPORTIONS)
    Aniso = FALSE
    RhoConfig = c("Beta1"=3, "Epsilon1"=0, "Beta2"=0, "Epsilon2"=0)
    Data_Orig = read.csv( paste0(DataFile,"flukeWithZeroes_forJim_fourCmBins.csv") )
    strata.limits = list( 'Flounder'=unique(Data_Orig$STRATUM) )
    Npool = Inf     # if a category has <Npool encounters, then pool it with others also having few
  }
  if(DataSet %in% c("Scallop")){
    Survey_to_use = "HabCam"
    Species = "not_needed"
    FieldConfig = c("Omega1"="IID", "Epsilon1"="IID", "Omega2"="IID", "Epsilon2"="IID")
    ObsModel_Est = c(1,0)
    OverdispersionConfig = c("Delta1"=0, "Delta2"=0)
    Use_REML = FALSE  # Much faster if TRUE, BUT CAUSES BIAS FOR PROPORTIONS WHEN USING BIAS-CORRECTION (DUE TO INFLATING LOW PROPORTIONS MORE THAN HIGH PROPORTIONS)
    Aniso = FALSE
    RhoConfig = c("Beta1"=0, "Epsilon1"=0, "Beta2"=0, "Epsilon2"=0)
    strata.limits = data.frame('STRATA'="All_areas")
    Npool = FALSE     # if a category has <Npool encounters, then pool it with others also having few
  }
  Nrep = 100
  Nyears = 25
  Proportion_of_samples = 0.25
  if(Use_First_Stage_Expansion==TRUE & ObsModel_Est[1]==5) stop("Problem with settings")

  # Biomass-dynamic simulation
  Sim_Settings = NULL
  Sim_Settings[["AgeBased"]] = list("beta1_mean"=log(1), "beta2_mean"=log(1), "beta1_slope"=0, "beta2_slope"=0, "beta1_sd"=0.5, "beta2_sd"=0, "SigmaO1"=0.5, "SigmaO2"=0.1, "SigmaE1"=0.2, "SigmaE2"=0.1, "SigmaV1"=0, "SigmaV2"=0, "SigmaVY1"=0, "SigmaVY2"=0, "Range1"=250, "Range2"=250, "SigmaM"=1, "ObsModel"=ObsModel_Sim, "Nages"=10, "M"=0.5, "K"=0.3, "Linf"=1, "W_alpha"=1, "W_beta"=3, "Selex_A50_mean"=3, "Selex_A50_sd"=0, "Selex_Aslope"=1)

  # Save record
  Nrep = ifelse(DataSet=="Simulated", Nrep, 1)
  Record = ThorsonUtilities::bundlelist( c("Sim_Settings","Version","DataSet","Method","grid_size_km","n_x","FieldConfig","strata.limits","c_set","Sex","Data_type","Use_First_Stage_Expansion","OverdispersionConfig","BiasCorr","Use_REML","Aniso","RhoConfig","Npool","Options","ObsModel_Est","Kmeans_Config","Nrep","Nyears","Proportion_of_samples","Survey_to_use","Species") )
  save( Record, file=file.path(DateFile,"Record.RData"))
  capture.output( Record, file=paste0(DateFile,"Record.txt"))
}

#######################
# Load existing data
#######################

# Extract data
if( file.exists(paste0(DateFile,"Data_Orig.RData")) ){
  load( file=paste0(DateFile,"Data_Orig.RData"))
  load( file=paste0(DateFile,"Extrapolation_List.RData"))
  print("Loaded previous Data_Orig")
}else{
  # Setting for adding zeros
  ZeroMethod = "Fast"

  # Load shelf-slope
  if( Survey_to_use == "EBS" ){
    # Download data
    Database = FishData::download_catch_rates( survey="Eastern_Bering_Sea", species_set=Species, error_tol=0.01, localdir=DataFile )
    Data_Orig = ThorsonUtilities::rename_columns( Database[,c('Lat','Long','Year','Wt','Sci')], newname=c('Lat','Lon','Year','Catch_KG','Species') )
    Data_Orig = cbind( Data_Orig, 'AreaSwept_km2'=1, "Vessel"="none" )   # WCGBTS and all AFSC surveys are in KG per Hectare
    # Make extrapolation grid
    Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region="Eastern_Bering_Sea", strata.limits=strata.limits )
  }
  if( Survey_to_use == "WCGBTS" ){
    # Download data
    Database = FishData::download_catch_rates( survey="WCGBTS", species_set=Species, error_tol=0.01, localdir=DataFile )
    Data_Orig = ThorsonUtilities::rename_columns( Database[,c('Lat','Long','Year','Wt','Sci')], newname=c('Lat','Lon','Year','Catch_KG','Species') )
    Data_Orig = cbind( Data_Orig, 'AreaSwept_km2'=1, "Vessel"="none" )   # WCGBTS and all AFSC surveys are in KG per Hectare
    # Make extrapolation grid
    Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region="California_current", strata.limits=strata.limits )
  }
  if( Survey_to_use == "NWA" ){
    # Make extrapolation grid
    Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region="northwest_atlantic", strata.limits=strata.limits )
  }
  if( Survey_to_use == "HabCam" ){
    # Make extrapolation grid
    Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region="HabCam", strata.limits=strata.limits )
  }
  save( Extrapolation_List, file=paste0(DateFile,"Extrapolation_List.RData"))

  if( exists("Data_Orig") ){
    # Remove years
    Include = max(min(Data_Orig$Year),max(Data_Orig$Year)-Nyears+1):max(Data_Orig$Year)
    Data_Orig = Data_Orig[ which(Data_Orig$Year %in% Include), ]
    # Exclude some data if Proportion_of_samples<1
    Data_New = NULL
    for(tI in 1:length(unique(Data_Orig[,'Year']))){
      Year = unique(Data_Orig[,'Year'])[tI]
      Data_Tmp = Data_Orig[ which(Data_Orig$Year==Year), ]
      Data_Tmp = Data_Tmp[ sample(1:nrow(Data_Tmp),size=ceiling(nrow(Data_Tmp)*Proportion_of_samples),replace=FALSE), ]
      Data_New = rbind( Data_New, Data_Tmp )
    }
    cbind( "Original"=table(Data_Orig$Year), "New"=table(Data_New$Year) )
    Data_Orig = Data_New
    # Save Data_Orig
    save( Data_Orig, file=paste0(DateFile,"Data_Orig.RData"))
  }
}

######################
# Loop through replicates
######################
MakePlots = FALSE

rI = mI = 1
for( rI in 1:Nrep ){
#for( rI in c(31:33) ){
#for( rI in c(1:27,51:61) ){
#for( rI in 1:50 ){
for( mI in 1:length(Sim_Settings) ){

  if( DataSet=="Simulated" ){
    Config = names(Sim_Settings)[mI]
    ConfigFile = paste0(DateFile,Config,"/")
    RunFile = paste0(ConfigFile,"r=",rI,"/")
    dir.create(RunFile,recursive=TRUE)
  }else{
    RunFile = ConfigFile = DateFile
  }

  # Simulate data
  if( DataSet=="Simulated" ){
    if( file.exists(paste0(RunFile,"SimList.RData")) ){
      load( file=paste0(RunFile,"SimList.RData"))
      message( "Loading simulated data for replicate ", rI )
    }else{
      # Generate data
      SimList = SpatialDeltaGLMM::Geostat_Sim( Sim_Settings=Sim_Settings[[mI]], Data_Geostat=Data_Orig, Extrapolation_List=Extrapolation_List, MakePlot=FALSE )
      save( SimList, file=paste0(RunFile,"SimList.RData"))
      tapply( SimList$Data_Geostat$Catch_KG, INDEX=SimList$Data_Geostat$Age, FUN=function(vec){c(mean(vec),mean(vec>0))})
      tapply( SimList$Data_Geostat[,'Catch_KG'], INDEX=list(SimList$Data_Geostat[,'Year'],SimList$Data_Geostat[,'Age']), FUN=function(vec){mean(vec>0)} )
      tapply( SimList$Data_Geostat[,'Catch_KG'], INDEX=list(SimList$Data_Geostat[,'Year'],SimList$Data_Geostat[,'Age']), FUN=mean )
      t(SimList$B_at)
    }
    # Extract and process simulated data
    Data_Geostat = na.omit( SimList[["Data_Geostat"]] )
  }
  if( DataSet=="Pollock" ){
    # Read or simulate trawl data
    Data_Orig = read.csv( paste0(DataFile,"pollock_pred_wt_by_length.csv") )
    Data_Augmented = data.frame( "Catch_KG"=NA, "Year"=Data_Orig[,'year'], "Length"=Data_Orig[,'length'], "AreaSwept_km2"=0.01, "Lat"=Data_Orig[,'latitude'], "Lon"=Data_Orig[,'longitude'])
    if( Data_type=="Numbers" ) Data_Augmented[,'Catch_KG'] = Data_Orig[,'noha']
    if( Data_type=="Weight" ) Data_Augmented[,'Catch_KG'] = Data_Orig[,'pred_kgha']
    if( Data_type=="Corrected_weight" ) Data_Augmented[,'Catch_KG'] = Data_Orig[,'pred_kgha'] * Data_Orig[,'mult']
    Year_Set = sort(unique(Data_Augmented[,'Year']))

    # Aggregate to length bins
    Data_Augmented = data.frame(Data_Augmented, "Bin_number"=unclass(cut(Data_Augmented[,'Length'], c_set, include.lowest=TRUE)))

    # Add tow ID
    Data_Augmented = data.frame(Data_Augmented, "TowID"=factor(paste(Data_Orig[,'year'],Data_Orig[,'station'],sep="_")) )
    Data_Augmented = data.frame(Data_Augmented, "TowID_bin"=factor(paste(as.numeric(Data_Augmented[,'TowID']),as.numeric(Data_Augmented[,'Bin_number']),sep="_")) )

    # Sum across elements in the same bin
    Unique = levels(Data_Augmented[,'TowID_bin'])
    Data_Geostat = Data_Augmented[ match(Unique,Data_Augmented[,'TowID_bin']), ]
    Data_Geostat[,'Catch_KG'] = tapply( Data_Augmented[,'Catch_KG'], INDEX=Data_Augmented[,'TowID_bin'], FUN=sum )

    # Sanity checks -- catch by bin
    all( tapply(Data_Augmented$Catch_KG,INDEX=Data_Augmented$Bin_number,FUN=sum) == tapply(Data_Geostat$Catch_KG,INDEX=Data_Geostat$Bin_number,FUN=sum) )
    # Sanity checks -- catch by year
    all( tapply(Data_Augmented$Catch_KG,INDEX=Data_Augmented$Year,FUN=sum)-tapply(Data_Geostat$Catch_KG,INDEX=Data_Geostat$Year,FUN=sum) < 1 )

    # rename
    Data_Geostat = ThorsonUtilities::rename_columns( Data_Geostat[,c('Lat','Lon','Year','Catch_KG','AreaSwept_km2','Bin_number')], newname=c('Lat','Lon','Year','Catch_KG','AreaSwept_km2','Age') )
    Data_Geostat = data.frame(Data_Geostat, "Vessel"="missing")
  }
  if( DataSet=="Lingcod" ){
    # Read data
    Data1 = read.csv( paste0(DataFile,"FisheryIndices2017_Lingcod_Final_Rev4--HaulCatchWgt&Effort.csv"), skip=8 )[,1:20]
    Data2 = read.csv( paste0(DataFile,"FisheryIndices2017_Lingcod_Final_Rev4--Lengths.csv"), skip=8 )[,1:17]
    # Calculate 1st-stage expansion factor
    Data1 = cbind( Data1, "Number_in_tow"=Data1[,'HAUL_WT_KG']/Data1[,'AVG_WT_KG'] )
    Data1[,'Number_in_tow'] = ifelse( Data1[,'HAUL_WT_KG']>0 & is.na(Data1[,'AVG_WT_KG']), mean(Data1[,'Number_in_tow'],na.rm=TRUE), Data1[,'Number_in_tow'] )
    # Eliminate southern samples
    Data1 = Data1[ which(Data1[,'BEST_LAT_DD']>42), ]
    Data2 = Data2[ which(Data2[,'HAUL_LATITUDE_DD']>42), ]
    # Reduce to single sex
    if(Sex=="Male") Data2 = Data2[ which(Data2[,'SEX']=="m"), ]
    if(Sex=="Female") Data2 = Data2[ which(Data2[,'SEX']=="f"), ]
    # Assign lengths to bins
    Data2 = data.frame(Data2, "Bin_number"=unclass(cut(Data2[,'LENGTH_CM'], c_set, include.lowest=TRUE)) )
    # Calculate number at length
    Data3 = tapply( rep(1,nrow(Data2)), INDEX=list(factor(Data2$HAUL_IDENTIFIER,levels=Data1$HAUL_IDENTIFIER),factor(Data2$Bin_number,levels=1:(length(c_set)-1))), FUN=length )
    Data3 = ifelse( is.na(Data3), 0, Data3 )
    colnames(Data3) = paste0("Bin_",c_set[-length(c_set)],"-",c_set[-1])
    # Rename Data1
    Data4 = data.frame( "Year"=as.numeric(substr(Data1$PROJECT_CYCLE,7,10)), Data1[,c("BEST_LAT_DD","BEST_LON_DD","HAUL_IDENTIFIER","Number_in_tow")], "Vessel"=Data1$VESSEL, "AreaSwept_km2"=Data1[,"AREA_SWEPT_HA"]/100 )
    Data4 = cbind( Data4, "Number_sampled"=rowSums(Data3) )
    Data4 = cbind( Data4, "Expansion_factor"=Data4[,'Number_in_tow']/Data4[,'Number_sampled'] )
    Data4[,'Expansion_factor'] = ifelse( Data4[,'Expansion_factor']>50, 50, Data4[,'Expansion_factor'])
    Data4[,'Expansion_factor'] = ifelse( Data4[,'Expansion_factor']<1, 1, Data4[,'Expansion_factor'])
    # Expand numbers-at-length
    if( Use_First_Stage_Expansion==TRUE ){
      Data3 = Data3 * outer(Data4[,'Expansion_factor'],rep(1,ncol(Data3)))
      Data3 = ifelse( is.na(Data3), 0, Data3 )
    }
    # Transform to long-form
    Data5 = NULL
    for(i in 1:(length(c_set)-1)){
      Data5 = rbind(Data5,Data4)
      #Data5 = data.frame( Data5, "Bin_number"=rep(i,nrow(Data3)) )
    }
    Data5 = data.frame( Data5, "Bin_number"=rep(1:(length(c_set)-1),each=nrow(Data3)), "Number"=as.vector(Data3) )
    # Rename
    Data_Geostat = ThorsonUtilities::rename_columns( Data5[,c('BEST_LAT_DD','BEST_LON_DD','Year','Number','AreaSwept_km2','Bin_number')], newname=c('Lat','Lon','Year','Catch_KG','AreaSwept_km2','Age') )
    # Add vessel for each unique observation
    Data_Geostat = cbind( Data_Geostat, "Vessel"=1:nrow(Data_Geostat))
    #### Logical checks
    # Check total number
    if( Use_First_Stage_Expansion==FALSE & sum(Data5$Number)!=nrow(Data2) ) stop("Problem with data processing for Lingcod")
    # Check year-bin combos with or without data
    Mat1 = tapply( Data2[,'SEX'], INDEX=list(Data2[,'Bin_number'],Data2[,'PROJECT_CYCLE']), FUN=function(vec){sum(vec==switch(Sex,"Male"="m","Female"="f"))})
    Mat1 = ifelse( is.na(Mat1), 0, Mat1 )
    Mat2 = tapply( Data_Geostat[,'Catch_KG'], INDEX=list(Data_Geostat[,'Age'],Data_Geostat[,'Year']), FUN=function(vec){sum(vec>0)})
    Mat2 = Mat2[ which(rowSums(Mat2)>0), ]
    if( any(ifelse(Mat1>0,1,0)!=ifelse(Mat2>0,1,0)) ) stop("Problem with data processing for lingcod")
  }
  if( DataSet=="SummerFlounder" ){
    # Read or simulate trawl data
    Data_Orig = read.csv( paste0(DataFile,"flukeWithZeroes_forJim_fourCmBins.csv") )

    # rename
    Data_Geostat = ThorsonUtilities::rename_columns( Data_Orig[,c('DECDEG_BEGLAT','DECDEG_BEGLON','EST_YEAR','NUMLEN','AREA','BIN')], newname=c('Lat','Lon','Year','Catch_KG','AreaSwept_km2','Age') )
    #Data_Geostat[,'AreaSwept_km2'] = Data_Geostat[,'AreaSwept_km2'] / 10000 # Convert from HA to KM2
    Data_Geostat = data.frame(Data_Geostat, "Vessel"="missing")
  }
  if( DataSet=="scallop"){ # HabCam_example[,'Density'] is in grams per square-meter
    Data_Orig = read.csv( paste0(DataFile,"Habcam_MAB_BySize_2012_2016_Weight_V3.csv") )

    # Make long-form DF
    Data_Geostat = NULL
    for( cI in 1:length(c_set)){
      Data_Geostat = rbind(Data_Geostat, cbind(ThorsonUtilities::rename_columns(Data_Orig[,c("Year","Latitude","Longitude")],newname=c('Year','Lat','Lon')), "Age"=c_set[cI], "AreaSwept_km2"=1e-6, "Catch_KG"=Data_Orig[,paste0("Weight_",c_set[cI],"_g_m2")]/1000) )
    }
    if(any(tapply(Data_Geostat[,'Catch_KG'],INDEX=Data_Geostat[,'Age'],FUN=sum)*1000-colSums(Data_Orig[,paste0("Weight_",c_set,"_g_m2")])>1)) stop("Check")
    if(any(tapply(Data_Geostat[,'Catch_KG'],INDEX=Data_Geostat[,'Year'],FUN=sum)*1000-tapply(Data_Orig[,'Total_weight_g_m2'],INDEX=Data_Orig[,'Year'],FUN=sum)>1)) stop("Check")
    Data_Geostat = cbind( Data_Geostat, "Vessel"="missing" )
  }

  # Remove ages after the first with zero encounters
  #EncProb_ta = tapply( Data_Geostat[,'Catch_KG'], INDEX=list(Data_Geostat[,'Year'],Data_Geostat[,'Age']), FUN=function(vec){mean(vec>0)} )
  #FirstAge = which( apply(EncProb_ta, MARGIN=2, FUN=prod)>0 )[1]
  #Ages2Include = which(cumprod(apply(EncProb_ta[,FirstAge:ncol(EncProb_ta)], MARGIN=2, FUN=prod))>0) + (FirstAge-1)
  #Data_Geostat = Data_Geostat[ which(Data_Geostat$Age %in% Ages2Include), ]

  # Remove ages with no encounters in any year
  EncProb_ta = tapply( Data_Geostat[,'Catch_KG'], INDEX=list(Data_Geostat[,'Year'],Data_Geostat[,'Age']), FUN=function(vec){mean(vec>0)} )
  Ages2Include = as.numeric(colnames(EncProb_ta))[which( colSums(EncProb_ta)>0 )]
  Data_Geostat = Data_Geostat[ which(Data_Geostat$Age %in% Ages2Include), ]

  # Compile spatial information
  Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=RunFile, Save_Results=TRUE )
  # Add knots to Data_Geostat
  Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )
  print( rbind(Sim_Settings[[1]][c('Range1','Range2')], range(dist(Spatial_List$loc_x))) )

  # Derived
  if( exists("Spatial_Smoother") & !exists("RhoConfig") ){
    RhoConfig = c("Beta1"=0, "Epsilon1"=0, "Beta2"=0, "Epsilon2"=0)
  }

  # Make data object
  TmbData = Data_Fn("Version"=Version, "CheckForErrors"=FALSE, "Aniso"=Aniso, "FieldConfig"=FieldConfig, "OverdispersionConfig"=OverdispersionConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel_Est, "c_i"=as.numeric(factor(Data_Geostat[,'Age'],levels=sort(unique(Data_Geostat[,'Age']))))-1, "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'], "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, "s_i"=Data_Geostat[,'knot_i']-1, "t_i"=Data_Geostat[,'Year'], "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options )

  # Switch missing year-category combinations to NAs
  EncProb_ta = tapply( TmbData$b_i, INDEX=list(TmbData$t_i,TmbData$c_i), FUN=function(vec){mean(vec>0)} )
  EncNum_ta = tapply( TmbData$b_i, INDEX=list(TmbData$t_i,TmbData$c_i), FUN=function(vec){sum(vec>0)} )
  Num_ta = tapply( TmbData$b_i, INDEX=list(TmbData$t_i,TmbData$c_i), FUN=sum )
  TmbData$b_i = ifelse( EncProb_ta[1+cbind(TmbData$t_i,TmbData$c_i)]==0, NA, TmbData$b_i )
  if( sum(TmbData$b_i,na.rm=TRUE)!=sum(Data_Geostat[,'Catch_KG']) ) stop("Check switch to NAs")

  # Run model
  if( file.exists(paste0(RunFile,"Save.RData")) ){
    load( paste0(RunFile,"Save.RData") )
    if( "Save" %in% search()) detach(Save)
    attach(Save)
  }else{
    # Make TMB object
    # TmbDir = "C:/Users/James.Thorson/Desktop/Project_git/VAST/inst/executables/"
    # dyn.unload( paste0(DateFile,"/",TMB::dynlib(Version)) ) # random=Random,
    TmbList = Build_TMB_Fn( "TmbData"=TmbData, "RunDir"=DateFile, "Use_REML"=ifelse(Use_REML=="TRUE_then_FALSE",TRUE,Use_REML), "Version"=Version, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x, "Method"=Method, "TmbDir"=TmbDir)
    Obj = TmbList[["Obj"]]  # "TmbDir"=TmbDir, "Parameters"=Save$ParHat,

    # Use NegBin-2 (i.e., turning off first variance-parameter for each category)
    if( ObsModel_Est[1]==5 ){
      Map = TmbList$Map
      Map[["logSigmaM"]] = factor( cbind(NA,1:TmbData$n_c,NA))
      ParHat = Obj$env$parList()
      ParHat[["logSigmaM"]][,1] = -20
      TmbList = Build_TMB_Fn( "Parameters"=ParHat, "Map"=Map, "TmbData"=TmbData, "RunDir"=DateFile, "Use_REML"=ifelse(Use_REML=="TRUE_then_FALSE",TRUE,Use_REML), "Version"=Version, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x, "Method"=Method, "TmbDir"=TmbDir)
      Obj = TmbList[["Obj"]]
    }

    # Make all SigmaM constant (for models with many bins and few data in some bins)
    if( exists("ConstantSigmaM") && ConstantSigmaM==TRUE){
      Map = TmbList$Map
      if(ObsModel_Est[1]==2) Map[["logSigmaM"]] = factor( cbind(rep(1,TmbData$n_c),NA,NA))
      if(ObsModel_Est[1]==5) Map[["logSigmaM"]] = factor( cbind(NA,rep(1,TmbData$n_c),NA))
      ParHat = Obj$env$parList()
      TmbList = Build_TMB_Fn( "Parameters"=ParHat, "Map"=Map, "TmbData"=TmbData, "RunDir"=DateFile, "Use_REML"=ifelse(Use_REML=="TRUE_then_FALSE",TRUE,Use_REML), "Version"=Version, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x, "Method"=Method, "TmbDir"=TmbDir)
      Obj = TmbList[["Obj"]]
    }

    # Make all category-specific variances (SigmaM, Omega, Epsilon) constant for models with EncNum_a < Npool
    EncNum_a = colSums( EncNum_ta )
    if( exists("Npool") && any(EncNum_a<Npool) ){
      pool = function(poolTF){
        Return = 1:length(poolTF)
        Return = ifelse( poolTF==TRUE, length(poolTF)+1, Return )
        return(Return)
      }
      # Change SigmaM / L_omega1_z / L_omega2_z / L_epsilon1_z / L_epsilon2_z
      Map = TmbList$Map
      if(ObsModel_Est[1]==2){
        Map[["logSigmaM"]] = cbind(1:TmbData$n_c,NA,NA)
        Map[["logSigmaM"]][which(EncNum_a<Npool),1] = TmbData$n_c+1
      }
      if(ObsModel_Est[1]==5){
        Map[["logSigmaM"]] = cbind(NA,1:TmbData$n_c,NA)
        Map[["logSigmaM"]][which(EncNum_a<Npool),2] = TmbData$n_c+1
      }
      Map[["logSigmaM"]] = factor( Map[["logSigmaM"]] )
      # Change Omegas
      Map[["L_omega1_z"]] = factor(pool(EncNum_a<Npool))
      Map[["L_omega2_z"]] = factor(pool(EncNum_a<Npool))
      # Change Epsilons
      Map[["L_epsilon1_z"]] = factor(pool(EncNum_a<Npool))
      Map[["L_epsilon2_z"]] = factor(pool(EncNum_a<Npool))
      # Reload object
      ParHat = Obj$env$parList()
      TmbList = Build_TMB_Fn( "Parameters"=ParHat, "Map"=Map, "TmbData"=TmbData, "RunDir"=DateFile, "Use_REML"=ifelse(Use_REML=="TRUE_then_FALSE",TRUE,Use_REML), "Version"=Version, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x, "Method"=Method, "TmbDir"=TmbDir)
      Obj = TmbList[["Obj"]]
    }

    # Change starting values for NegBin2 (existing starting values suck)
    if( ObsModel_Est[1]==5 ){
      Obj$par[] = 1
      (fn2 = Obj$fn(Obj$par))
      Obj$gr(Obj$par)
    }

    # Optimize model                        # lower=TmbList[["Lower"]], upper=TmbList[["Upper"]],
    Opt = try( TMBhelper::Optimize(obj=Obj, newtonsteps=0, getsd=FALSE, savedir=RunFile, bias.correct=BiasCorr) )
    file.copy( from=paste0(RunFile,"parameter_estimates.txt"), to=paste0(RunFile,"parameter_estimates_V1.txt"), overwrite=TRUE )
    ParHat = Obj$env$parList()
    Save = list( "Opt"=Opt, "ParHat"=ParHat )
    save(Save, file=paste0(RunFile,"Save_V1.RData") )

    # If necessary, fix some variances at zero
    MaxGrad = 0.0001
    if( TRUE | any(abs(Opt$diagnostics$final_gradient)>=MaxGrad) ){
      Repeat = TRUE
      LoopNum = 0
    }
    while( Repeat==TRUE ){
      # Increment LoopNum
      LoopNum = LoopNum + 1
      # Extract previous MLE
      ParHat = Obj$env$parList()
      Map = TmbList$Map
      # Function to fix zero-values
      fixval = function(array,map,threshold=0.001,f=abs){
        FixTF = f(as.vector(array))<threshold
        if(!is.null(map)){
          FixTF = FixTF | is.na(map)
          Return = factor( ifelse(FixTF,NA,map) )
        }else{
          Return = factor( ifelse(FixTF,NA,1:length(as.vector(array))) )
        }
        return(Return)
      }
      is_equal = function(char,val) !is.na(as.numeric(char)) && char==val
      if(FieldConfig[1]=="IID"){
        Map[["L_omega1_z"]] = fixval( ParHat[["L_omega1_z"]], map=Map[["L_omega1_z"]], threshold=0.01 )
        ParHat[["L_omega1_z"]] = ifelse( is.na(Map[["L_omega1_z"]]), 0, ParHat[["L_omega1_z"]] )
      }
      if(FieldConfig[2]=="IID"){
        Map[["L_epsilon1_z"]] = fixval( ParHat[["L_epsilon1_z"]], map=Map[["L_epsilon1_z"]], threshold=0.01 )
        ParHat[["L_epsilon1_z"]] = ifelse( is.na(Map[["L_epsilon1_z"]]), 0, ParHat[["L_epsilon1_z"]] )
      }
      if(FieldConfig[3]=="IID"){
        Map[["L_omega2_z"]] = fixval( ParHat[["L_omega2_z"]], map=Map[["L_omega2_z"]], threshold=0.01 )
        ParHat[["L_omega2_z"]] = ifelse( is.na(Map[["L_omega2_z"]]), 0, ParHat[["L_omega2_z"]] )
      }
      if(FieldConfig[4]=="IID"){
        Map[["L_epsilon2_z"]] = fixval( ParHat[["L_epsilon2_z"]], map=Map[["L_epsilon2_z"]], threshold=0.01 )
        ParHat[["L_epsilon2_z"]] = ifelse( is.na(Map[["L_epsilon2_z"]]), 0, ParHat[["L_epsilon2_z"]] )
      }
      if(exists("ConstantSigmaM") && ConstantSigmaM==FALSE && ObsModel_Est[1]==5){
        Map[["logSigmaM"]] = fixval( ParHat[["logSigmaM"]], map=Map[["logSigmaM"]], threshold=-4.6, f=function(a){a} )
      }
      # Turn off hyperparameters
      if( all(is.na(Map[["L_omega1_z"]])) & all(is.na(Map[["L_epsilon1_z"]])) ){
        Map[["logkappa1"]] = factor( NA )
      }
      if( all(is.na(Map[["L_omega2_z"]])) & all(is.na(Map[["L_epsilon2_z"]])) ){
        Map[["logkappa2"]] = factor( NA )
      }
      # Make TMB object
      TmbList = Build_TMB_Fn( "TmbData"=TmbData, "Parameters"=ParHat, "Map"=Map, "RunDir"=DateFile, "Use_REML"=ifelse(Use_REML=="TRUE_then_FALSE",TRUE,Use_REML), "Version"=Version, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x, "Method"=Method, "TmbDir"=TmbDir)
      Obj = TmbList[["Obj"]]  # "TmbDir"=TmbDir, "Parameters"=ParHat,
      # Optimize model                        # lower=TmbList[["Lower"]], upper=TmbList[["Upper"]],
      Opt = try( TMBhelper::Optimize(obj=Obj, newtonsteps=ifelse(LoopNum<=2,0,1), getsd=FALSE, savedir=RunFile, bias.correct=BiasCorr) )
      file.copy( from=paste0(RunFile,"parameter_estimates.txt"), to=paste0(RunFile,"parameter_estimates_V2",letters[LoopNum],".txt"), overwrite=TRUE )
      ParHat = Obj$env$parList()
      Save = list( "Opt"=Opt, "ParHat"=ParHat )
      save(Save, file=paste0(RunFile,"Save_V2",letters[LoopNum],".RData") )
      # Exit conditions for loop
      if( all(abs(Opt$diagnostics$final_gradient)<MaxGrad) | LoopNum>4 ) Repeat = FALSE
    }

    # Change to REML=FALSE
    if( Use_REML=="TRUE_then_FALSE" ){
      # Extract previous MLE
      ParHat = Obj$env$parList()
      Map = TmbList$Map
      # Make TMB object
      TmbList = Build_TMB_Fn( "TmbData"=TmbData, "Parameters"=ParHat, "Map"=Map, "RunDir"=DateFile, "Use_REML"=FALSE, "Version"=Version, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x, "Method"=Method, "TmbDir"=TmbDir)
      Obj = TmbList[["Obj"]]
      # Optimize model
      Opt = try( TMBhelper::Optimize(obj=Obj, startpar=Obj$env$last.par.best[-Obj$env$random], loopnum=1, newtonsteps=0, getsd=FALSE, savedir=RunFile, bias.correct=BiasCorr) )
      file.copy( from=paste0(RunFile,"parameter_estimates.txt"), to=paste0(RunFile,"parameter_estimates_V3.txt") )
    }

    # Get hessian
    Hess = optimHess( par=Obj$env$last.par.best[-Obj$env$random], fn=Obj$fn, gr=Obj$gr )
    #Eigen = eigen(Hess)
    #data.frame( names(Obj$par), Obj$env$last.par.best[-Obj$env$random], Eigen$vectors[,273:275] )

    # get standard errors and bias-corrected predictions
    # Opt = TMBhelper::Optimize(obj=Obj, newtonsteps=1, getsd=FALSE, savedir=RunFile, bias.correct=BiasCorr)
    if( "ADreportIndex" %in% names(Obj$env) ){
      Which = as.vector(unlist( Obj$env$ADreportIndex()[ "Index_cyl" ] ))
      #Which = split( Which, cut(seq_along(Which), 10) )
      #Which = Which[sapply(Which,FUN=length)>0]
      Opt[["SD"]] = sdreport( obj=Obj, par.fixed=Opt$par, hessian.fixed=Hess, bias.correct=BiasCorr, bias.correct.control=list(sd=FALSE, split=Which, nsplit=NULL) )
    }else{
      Opt[["SD"]] = sdreport( obj=Obj, par.fixed=Opt$par, hessian.fixed=Hess, bias.correct=BiasCorr, bias.correct.control=list(sd=FALSE, split=NULL, nsplit=200) )
    }
    capture.output(Opt, file = file.path(RunFile, "parameter_estimates_Vfinal.txt"))
    file.remove( paste0(RunFile,"parameter_estimates.txt") )

    # Look at covariance
    if(FALSE){
      Which = which( names(Opt$SD$value)=="Index_cyl" )
      Cov = Opt$SD$cov[Which,Which]
    }

    # Decide which years to plot
    Report = Obj$report()
    ParHat = Obj$env$parList()

    # Save progress (in case loop is broken)
    Save = list( "Opt"=Opt, "Report"=Report, "ParHat"=ParHat, "Map"=Map, "Ages2Include"=Ages2Include )
    if(exists("Hess")) Save[["Hess"]] = Hess
    if(exists("SimList")) Save[["True_index"]] = SimList$B_t/1000
    save(Save, file=paste0(RunFile,"Save.RData") )
  }

  # Calculate design-based expansion
  if( file.exists(paste0(RunFile,"Design.RData")) ){
    load( paste0(RunFile,"Design.RData") )
    if( "Design" %in% search()) detach(Design)
    attach(Design)
  }else{
    # Index and its variance
    Index_ct = tapply( TmbData$b_i/TmbData$a_i, INDEX=list(TmbData$c_i,TmbData$t_i[,1]), FUN=mean ) * sum(Extrapolation_List$Area_km2_x)
    var_Index_ct = tapply( TmbData$b_i/TmbData$a_i, INDEX=list(TmbData$c_i,TmbData$t_i[,1]), FUN=function(vec){var(vec)/length(vec)} ) * sum(Extrapolation_List$Area_km2_x)^2
    N_ct = tapply( TmbData$b_i/TmbData$a_i, INDEX=list(TmbData$c_i,TmbData$t_i[,1]), FUN=length )
    EncProp_ct = tapply( TmbData$b_i/TmbData$a_i, INDEX=list(TmbData$c_i,TmbData$t_i[,1]), FUN=function(vec){mean(vec>0)} )

    # Fill in missing
    Num_ct = tapply( TmbData$b_i, INDEX=list(factor(TmbData$c_i,levels=1:TmbData$n_c-1),factor(TmbData$t_i[,1],levels=1:TmbData$n_t-1)), FUN=function(vec){sum(!is.na(vec))} )
    Index_ct = ifelse(Num_ct==0, 0, Index_ct)
    #dimnames(Index_ct) = list( paste0("Bin_",sort(unique(Data_Geostat[,'Age']))), paste0("Year_",unique(Data_Geostat[,'Year'])) )
    if(!is.null(c_set)) dimnames(Index_ct) = list( paste0("Bin_",c_set[Ages2Include],"-",c_set[Ages2Include+1]), paste0("Year_",unique(Data_Geostat[,'Year'])) )

    # Sum across categories
    Index_t = colSums( Index_ct )
    var_Index_t = apply(var_Index_ct,MARGIN=2,FUN=sum,na.rm=TRUE)
    # sqrt(var_Index_ct) / Index_ct

    # Design-based proportion
    Prop_ct = Index_ct / outer(rep(1,TmbData$n_c),Index_t)

    # Approximate variance for proportions, and effective sample size
    Neff_ct = var_Prop_ct = array(NA,dim=dim(Index_ct))
    for( cI in 1:dim(var_Prop_ct)[1]){
    for( tI in 1:dim(var_Prop_ct)[2]){
      # Original version
      #var_Prop_ct[cI,tI] = Index_ct[cI,tI]^2/Index_t[tI]^2 * (var_Index_ct[cI,tI]/Index_ct[cI,tI]^2  + var_Index_t[tI]/Index_t[tI]^2 )
      # Slightly extended version
      var_Prop_ct[cI,tI] = Index_ct[cI,tI]^2/Index_t[tI]^2 * (var_Index_ct[cI,tI]/Index_ct[cI,tI]^2 - 2*var_Index_ct[cI,tI]/(Index_ct[cI,tI]*Index_t[tI]) + var_Index_t[tI]/Index_t[tI]^2 )
      Neff_ct[cI,tI] = Prop_ct[cI,tI] * (1-Prop_ct[cI,tI]) / var_Prop_ct[cI,tI]
    }}
    Neff_t = apply(Neff_ct, MARGIN=2, FUN=median, na.rm=TRUE)

    # Save stuff
    Design = list( "Index_ct"=Index_ct, "Index_t"=Index_t, "var_Index_ct"=var_Index_ct, "var_Index_t"=var_Index_t, "Prop_ct"=Prop_ct, "var_Prop_ct"=var_Prop_ct, "Neff_ct"=Neff_ct, "Neff_t"=Neff_t )
    save( Design, file=paste0(RunFile,"Design.RData"))

    # Write to file
    Table = cbind( "Year"=unique(Data_Geostat$Year), "Season"=7, "Gender"=switch(Sex,"Male"=2,"Female"=1), "Partition"=0, "nSamps"=Design$Neff_t, Prop_tc=t(Design$Prop_ct) )
    write.csv( Table, file=paste0(RunFile,"Design-SS3.csv"), row.names=FALSE)
    table(Data_Geostat$Age)
  }

  # Decide which years to plot
  Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
  Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))

  # Calculate and save index   #
  # PlotName="Index.png"; interval_width=1; strata_names=NULL; category_names=NULL; use_biascorr=FALSE; plot_legend=TRUE; total_area_km2=NULL; plot_log=FALSE; width=4; height=4; treat_missing_as_zero=TRUE
  # DirName=RunFile; TmbData=TmbData; Sdreport=Opt[["SD"]]; Year_Set=Year_Set; Years2Include=Years2Include; strata_names=strata.limits[,1]; use_biascorr=TRUE
  Index = SpatialDeltaGLMM::PlotIndex_Fn( DirName=RunFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Years2Include=Years2Include, strata_names="", use_biascorr=TRUE )
  save( Index, file=paste0(RunFile,"Index.RData"))

  #Plot Anisotropy
  SpatialDeltaGLMM::PlotAniso_Fn( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )

  # Plot comparison
  Results = cbind( "Design"=Design$Index_t/1000, "VAST"=colSums(Index$Index_ctl[,,1,'Estimate']) )
  if(exists("SimList")) Results = cbind(Results, "True"=SimList$B_tl[,1]/1000 )
  matplot( Results, col=c("red","blue","black"), type="l", lty="solid", lwd=2 )

  # Calculate proportions
  # strata_names=NULL; category_names=NULL; plot_legend=TRUE; DirName=paste0(getwd(),"/"); PlotName="Proportion.png"; interval_width=1; width=6; height=6
  # Index=Index; TmbData=TmbData; Year_Set=Year_Set; DirName=RunFile; width=10; height=10; plot_legend=FALSE
  Prop_List = calculate_proportion( Index=Index, TmbData=TmbData, Year_Set=Year_Set, DirName=RunFile, width=10, height=10, plot_legend=FALSE )
  save( Prop_List, file=paste0(RunFile,"Prop_List.RData"))

  # Debug comparison of effective sample size
  if(FALSE){
    Prop_List$var_Prop_ctl[,,1]
    Design$var_Prop_ct
    Prop_List$var_Prop_ctl[,1,1] / Design$var_Prop_ct[,1]
    # Ccmpare variance for index
    mean( (Index$Index_ctl[,,1,"Std. Error"]*1000)^2 / Design$var_Index_ct, na.rm=TRUE )
    summary( as.vector((Index$Index_ctl[,,1,"Std. Error"]*1000)^2 / Design$var_Index_ct), na.rm=TRUE )
    # Ccmpare variance for proportion
    mean( Prop_List$var_Prop_ctl[,,1] / Design$var_Prop_ct, na.rm=TRUE )
    # Ccmpare Neff_ct
    mean( Prop_List$Neff_ctl[,,1] / Design$Neff_ct, na.rm=TRUE )
    # Ccmpare Neff_t
    mean( Prop_List$Neff_tl[,1] / Design$Neff_t, na.rm=TRUE )
    # Average encounter probability
    summary( as.vector(EncProp_ct) )
  }

  # Write to file
  Table = cbind( "Year"=unique(Data_Geostat$Year), "Season"=7, "Gender"=switch(Sex,"Male"=2,"Female"=1,"Combined"=3,"Both"=3), "Partition"=0, "nSamps"=Prop_List$Neff_tl[,1], Prop_tc=t(Prop_List$Prop_ctl[,,1]) )
  write.csv( Table, file=paste0(RunFile,"VAST-SS3.csv"), row.names=FALSE)
  table(Data_Geostat$Age)

  # Check for bugs
  MaxAge = max( Index$Table[,'Category'] )
  Bresults = t(Index$Index_ctl[,,1,'Estimate'])
  m1 = Bresults / outer(rowSums(Bresults), rep(1,MaxAge))
  m2 = t(Prop_List$Prop_ctl[,,1])
  if( any(m1!=m2) ) stop("Check `calculate_proportion`")

  # Plot true index by category
  Par = list( mar=c(2,2,1,0), mgp=c(2,0.5,0), tck=-0.02, yaxs="i", oma=c(1,1,0,0), mfrow=c(ceiling(sqrt(TmbData$n_c)),ceiling(TmbData$n_c/ceiling(sqrt(TmbData$n_c)))) )
  png( file=paste0(RunFile,"/Index-True.png"), width=8, height=8, res=200, units="in")
    par( Par )
    for( cI in 1:TmbData$n_c ){
      # Calculate y-axis limits
      interval_width = 1; plot_log = FALSE;  plot_legend=FALSE;  category_names = 1:TmbData$n_c
      if(exists("Index")) Ylim = c(0, max(max(Index$Index_ctl[cI,Years2Include,,'Estimate']%o%c(1,1) * exp(Index$log_Index_ctl[cI,Years2Include,,'Std. Error']%o%c(-interval_width,interval_width)),na.rm=TRUE),max(Design$Index_ct/1000)) )
      if(!exists("Index")) Ylim =  c(0, max(max(Report$Index_cyl),max(Design$Index_ct/1000)))
      # Plot stuff
      plot(1, type="n", xlim=range(Year_Set), ylim=ifelse(plot_legend==TRUE,1.25,1.05)*Ylim, xlab="", ylab="", main=ifelse(TmbData$n_c>1,category_names[cI],""), log=ifelse(plot_log==TRUE,"y","") )
      for(l in 1:TmbData$n_l){
        if(!exists("Index")) lines( x=Year_Set[Years2Include], y=Report$Index_cyl[cI,Years2Include,1], col="red", lwd=2 )
        if(exists("Index")) SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( y=Index$Index_ctl[cI,Years2Include,l,'Estimate'], x=Year_Set[Years2Include]+seq(-0.1,0.1,length=TmbData$n_l)[l], ybounds=(Index$Index_ctl[cI,Years2Include,l,'Estimate']%o%c(1,1))*exp(Index$log_Index_ctl[cI,Years2Include,l,'Std. Error']%o%c(-interval_width,interval_width)), type="b", col=rainbow(TmbData[['n_l']])[l], col_bounds=rainbow(TmbData[['n_l']])[l], ylim=Ylim)
        if(exists("SimList")) lines( x=Year_Set[Years2Include], y=SimList$B_at[cI,Years2Include]/1000, lwd=2 )
        lines( x=Year_Set[Years2Include], y=Design$Index_ct[cI,Years2Include]/1000, col="blue", lwd=2 )
      }
    }
    mtext( side=1:2, text=c("Year","Abundance (metric tonnes)"), outer=TRUE, line=c(0,0) )
  dev.off()

  # Plot true proportion by category
  interval_width = 1
  Nrow = ceiling(sqrt(TmbData$n_t))
  Ncol = ceiling(TmbData$n_t/Nrow)
  Par = list( mar=c(1,2,1,0), mgp=c(2,0.5,0), tck=-0.02, yaxs="i", oma=c(3,2,0,0), mfrow=c(Nrow,Ncol) )
  File = NULL
  if( DataSet=="Lingcod" ){
    File = paste0(RunFile,"Fig_",switch(Sex,"Male"="5b","Female"="5a"),"_Proportion-Comparison_")
  }
  if( DataSet=="Simulated" & rI==1 ){
    File = paste0(RunFile,"/Fig_1_Proportion-Comparison_")
  }
  if( is.null(File) ){
    File = paste0(RunFile,"/Proportion-Comparison_")
  }
  ThorsonUtilities::save_fig( file=File, suffix=c("LO","HI"), width=10, height=10, res=c(200,600), FUN=function(){
    par( Par )
    # Calculate y-axis limits
    Ymax = max(Prop_List$Prop_ctl%o%c(1,1) + sqrt(Prop_List$var_Prop_ctl)%o%c(-interval_width,interval_width),na.rm=TRUE)
    Ymax = max(Ymax,max(Design$Prop_ct%o%c(1,1) + sqrt(Design$var_Prop_ct)%o%c(-interval_width,interval_width),na.rm=TRUE))
    for( tI in 1:TmbData$n_t ){
      # Plot stuff
      plot(1, type="n", xlim=c(1,TmbData$n_c), ylim=c(0,1.05*Ymax), xlab="", ylab="", main=paste0("Year ",Year_Set[tI]), xaxt="n" )
      SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( y=Prop_List$Prop_ctl[,tI,l], x=1:TmbData$n_c-0.05, bounds_type="whiskers", ybounds=Prop_List$Prop_ctl[,tI,]%o%c(1,1) + sqrt(Prop_List$var_Prop_ctl[,tI,])%o%c(-interval_width,interval_width), type="b", col="red", col_bounds=rgb(1,0,0,1), fn=lines, lwd=1.5 )
      SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( y=Design$Prop_ct[,tI], x=1:TmbData$n_c+0.05, bounds_type="whiskers", ybounds=Design$Prop_ct[,tI]%o%c(1,1) + sqrt(Design$var_Prop_ct[,tI])%o%c(-interval_width,interval_width), type="b", col="blue", col_bounds=rgb(0,0,1,1), fn=lines, lwd=1.5 )
      legend( "top", bty="n", legend=formatC(Prop_List$Neff_tl[tI,1],format="f",digits=0), inset=-0.03, text.col="red" )                                                                                                                                          # rgb(0,0,1,0.2)
      legend( "top", bty="n", legend=formatC(Design$Neff_t[tI],format="f",digits=0), inset=0.05, text.col="blue" )
      if(DataSet=="Simulated") lines( x=1:TmbData$n_c, y=SimList$B_at[Ages2Include,tI]/sum(SimList$B_at[Ages2Include,tI]), lwd=2, lty="dotted", col="black" )
      WhichTicks = round(seq(1,TmbData$n_c,length=min(10,TmbData$n_c)))
      if(tI > TmbData$n_t-Ncol){
        if( is.null(c_set)) axis(1, at=(1:TmbData$n_c)[WhichTicks], labels=Ages2Include[WhichTicks], las=3, line=0)
        if( !is.null(c_set)) axis(1, at=(1:TmbData$n_c)[WhichTicks], labels=paste0("[",c_set[Ages2Include],"-",c_set[Ages2Include+1],")")[WhichTicks], las=3)
      }
    }
    mtext( side=1:2, text=c(switch(DataSet,"Lingcod"="Length (cm)","Age"),switch(DataSet,"Lingcod"="Proportion of abundance","Proportion of biomass")), outer=TRUE, line=c(ifelse(DataSet=="Simulated",1,0),0) )
  })

  if( DataSet=="Lingcod" ){
    # Load original
    Comps = read.csv( file=paste0(DataFile,"LingNorth2017--Original_Comps.csv") )
    Comps = Comps[,paste0(switch(Sex,"Female"="F","Male"="M"),c_set[Ages2Include])]
    Comps = Comps / rowSums(Comps,rep(1,ncol(Comps)))
    # Plot true proportion by category
    interval_width = 1
    Nrow = ceiling(sqrt(TmbData$n_t))
    Ncol = ceiling(TmbData$n_t/Nrow)
    Par = list( mar=c(1,2,1,0), mgp=c(2,0.5,0), tck=-0.02, yaxs="i", oma=c(3,2,0,0), mfrow=c(Nrow,Ncol) )
    png( file=paste0(RunFile,"/Proportion-Comparison-withoriginal.png"), width=10, height=10, res=200, units="in")
      par( Par )
      # Calculate y-axis limits
      Ymax = max(Prop_List$Prop_ctl%o%c(1,1) + sqrt(Prop_List$var_Prop_ctl)%o%c(-interval_width,interval_width),na.rm=TRUE)
      Ymax = max(Ymax,max(Design$Prop_ct%o%c(1,1) + sqrt(Design$var_Prop_ct)%o%c(-interval_width,interval_width),na.rm=TRUE))
      for( tI in 1:TmbData$n_t ){
        # Plot stuff
        plot(1, type="n", xlim=c(1,TmbData$n_c), ylim=c(0,1.05*Ymax), xlab="", ylab="", main=paste0("Year ",Year_Set[tI]), xaxt="n" )
        SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( y=Prop_List$Prop_ctl[,tI,l], x=1:TmbData$n_c-0.05, bounds_type="whiskers", ybounds=Prop_List$Prop_ctl[,tI,]%o%c(1,1) + sqrt(Prop_List$var_Prop_ctl[,tI,])%o%c(-interval_width,interval_width), type="b", col="red", col_bounds=rgb(1,0,0,1), fn=lines, lwd=1.5 )
        SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( y=Design$Prop_ct[,tI], x=1:TmbData$n_c+0.05, bounds_type="whiskers", ybounds=Design$Prop_ct[,tI]%o%c(1,1) + sqrt(Design$var_Prop_ct[,tI])%o%c(-interval_width,interval_width), type="b", col="blue", col_bounds=rgb(0,0,1,1), fn=lines, lwd=1.5 )
        lines( y=Comps[tI,], x=1:TmbData$n_c+0.05, lwd=3 )
        legend( "top", bty="n", legend=formatC(Prop_List$Neff_tl[tI,1],format="f",digits=0), inset=-0.03, text.col="red" )                                                                                                                                          # rgb(0,0,1,0.2)
        legend( "top", bty="n", legend=formatC(Design$Neff_t[tI],format="f",digits=0), inset=0.05, text.col="blue" )
        if(DataSet=="Simulated") lines( x=1:TmbData$n_c, y=SimList$B_at[Ages2Include,tI]/sum(SimList$B_at[Ages2Include,tI]), lwd=2, lty="dotted", col="black" )
        WhichTicks = round(seq(1,TmbData$n_c,length=min(10,TmbData$n_c)))
        if(tI > TmbData$n_t-Ncol) axis(1, at=(1:TmbData$n_c)[WhichTicks], labels=paste0("[",c_set[Ages2Include],"-",c_set[Ages2Include+1],")")[WhichTicks], las=3)
      }
      mtext( side=1:2, text=c(switch(DataSet,"Lingcod"="Length (cm)","Age"),"Proportion of biomass"), outer=TRUE, line=c(0,0) )
    dev.off()
  }

  # Plot true index by year
  if( DataSet=="Simulated" ){
    interval_width = 1
    Par = list( mar=c(2,2,1,0), mgp=c(2,0.5,0), tck=-0.02, yaxs="i", oma=c(2,2,0,0), mfrow=c(ceiling(sqrt(TmbData$n_t)),ceiling(TmbData$n_t/ceiling(sqrt(TmbData$n_t)))) )
    png( file=paste0(RunFile,"/Proportion-True.png"), width=10, height=10, res=200, units="in")
      par( Par )
      # Calculate y-axis limits
      Ymax = max(Prop_List$Prop_ctl%o%c(1,1) + sqrt(Prop_List$var_Prop_ctl)%o%c(-interval_width,interval_width),na.rm=TRUE)
      Ymax = max(Ymax,max(Design$Prop_ct%o%c(1,1) + sqrt(Design$var_Prop_ct)%o%c(-interval_width,interval_width)),na.rm=TRUE)
      #Ylim = c(0, max(Prop_List$Prop_ctl%o%c(1,1) + sqrt(Prop_List$var_Prop_ctl)%o%c(-interval_width,interval_width)))
      for( tI in 1:TmbData$n_t ){
        # Plot stuff
        plot(1, type="n", xlim=c(1,TmbData$n_c), ylim=c(0,1.05*Ymax), xlab="", ylab="", main=paste0("Year ",Year_Set[tI]) )
        SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( y=Prop_List$Prop_ctl[,tI,l], x=1:TmbData$n_c, bounds_type="shading", ybounds=Prop_List$Prop_ctl[,tI,]%o%c(1,1) + sqrt(Prop_List$var_Prop_ctl[,tI,])%o%c(-interval_width,interval_width), type="b", col="red", col_bounds=rgb(1,0,0,0.2), ylim=Ylim, fn=lines, lwd=2 )
        SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( y=Design$Prop_ct[,tI], x=1:TmbData$n_c, bounds_type="shading", ybounds=Design$Prop_ct[,tI]%o%c(1,1) + sqrt(Design$var_Prop_ct[,tI])%o%c(-interval_width,interval_width), type="b", col="blue", col_bounds=rgb(0,0,1,0.2), ylim=Ylim, fn=lines, lwd=2 )
        #lines( x=1:TmbData$n_c, y=Design$Design_ct[,tI], lwd=1.5, col="blue" )
        lines( x=1:TmbData$n_c, y=SimList$B_at[Ages2Include,tI]/sum(SimList$B_at[Ages2Include,tI]), lwd=1.5, col="black" )
        legend( "top", bty="n", legend=formatC(Prop_List$Neff_tl[tI,1],format="f",digits=0), inset=-0.03, text.col="red" )                                                                                                                                          # rgb(0,0,1,0.2)
        legend( "top", bty="n", legend=formatC(Design$Neff_t[tI],format="f",digits=0), inset=0.05, text.col="blue" )
      }
      mtext( side=1:2, text=c("Age","Proportion of biomass"), outer=TRUE, line=c(0,0) )
    dev.off()
  }

  if( MakePlots==TRUE ){
    # Get region-specific settings for plots
    MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( "Region"=switch(DataSet,"Lingcod"="California_current","Simulated"="California_current","Pollock"="Eastern_Bering_Sea","SummerFlounder"="Northwest_Atlantic","Scallop"="Northwest_Atlantic"), "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )

    # Plot results on map
    SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=c(3), MappingDetails=MapDetails_List[["MappingDetails"]], plot_legend_fig=FALSE, Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=RunFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=0.1, category_names=levels(Data_Geostat[,'Age']))

    # Calculate and save COG
    COG = SpatialDeltaGLMM::Plot_range_shifts(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Znames=colnames(TmbData$Z_xm), PlotDir=RunFile, Year_Set=Year_Set)

    # Plot data
    SpatialDeltaGLMM::Plot_data_and_knots(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=RunFile )

    # Diagnostics
    Enc_prob = SpatialDeltaGLMM::Check_encounter_prob( Report=Report, Data_Geostat=Data_Geostat, DirName=RunFile )
    save(Enc_prob, file=paste0(RunFile,"Enc_prob.RData"))

    # Aggregated QQ plot
    Q = SpatialDeltaGLMM::QQ_Fn( TmbData=TmbData, Report=Report, DateFile=RunFile) # SpatialDeltaGLMM::
    save(Q, file=paste0(RunFile,"Q.RData"))

    # Plot for encoutner probabilities
    Par = list( mar=c(3,3,1,1), mgp=c(2,0.5,0), tck=-0.02 )
    interval_width = 1
    png( file=paste0(RunFile,"/EncProb.png"), width=5, height=5, res=200, units="in")
      par( Par )
      plot( x=Enc_prob$Diag_z[,'midpoints_z'], y=Enc_prob$Diag_z[,'freq_z'], pch=20, cex=1.2, xlim=c(0,1), ylim=c(0,1), xlab="Predicted encounter probability", ylab="Observed encounter frequency" )
      SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( x=Enc_prob$Diag_z[,'midpoints_z'][which(!is.na(Enc_prob$Diag_z[,'mean_z']))], y=Enc_prob$Diag_z[,'mean_z'][which(!is.na(Enc_prob$Diag_z[,'mean_z']))], ybounds=(Enc_prob$Diag_z[,'mean_z']%o%c(1,1)+Enc_prob$Diag_z[,'sd_mean_z']%o%c(-interval_width,interval_width))[which(!is.na(Enc_prob$Diag_z[,'mean_z'])),], lwd=2, bounds_type="shading", col_bounds=rgb(1,0,0,0.2), col="red" )
      abline(a=0, b=1, lty="dotted", lwd=2 )
      legend( "topleft", legend=c("Observed","Predicted"), fill=c("black","red"), bty="n")
    dev.off()

    # Disaggregated QQ plot
    ThorsonUtilities::save_fig( file=paste0(RunFile,"QQ_aggregate"), width=4, height=4 )
      par( mar=c(2.5,2.5,3,0), mgp=c(1.25,0.25,0), tck=-0.02 )
      Qtemp = unlist( sapply(Q, FUN=function(qlist){qlist$Q}) )
      Order = order(Qtemp)
      plot(x=seq(0,1,length=length(Order)), y=Qtemp[Order], xlab="Uniform distribution", ylab="Empirical distribution", type="l", lwd=3, main="Quantile-Quantile distribution\n(aggregate)" )
      abline(a=0,b=1)
    dev.off()

    # True vs. estimated density maps
    #Nrow = 4
    #save_fig( file=paste0(DateFile,"Example_figure--Maps"), width=3.8, height=7, res=200, type="png", FUN=function(){
    #  par( mfrow=c(TmbData$n_c,Nrow), mar=c(0,0.5,2,0.5), mgp=c(2,0.5,0), tck=-0.02, oma=c(2.5,2,0,0) )  #
    #  PolygonList = Spatial_List$PolygonList
    #  # plot_set=1:5; MappingDetails; Report; Sdreport=NULL; Nknots=Inf; PlotDF; MapSizeRatio=c('Width(in)'=4,'Height(in)'=4); Xlim; Ylim; FileName=paste0(getwd(),"/"); Year_Set=NULL; Years2Include=NULL; Rescale=FALSE; Rotate=0; Format="png"; Res=200; zone=NA; Cex=0.01; add=FALSE; category_names=NULL; textmargin=NULL; pch=NULL; Legend=list("use"=FALSE,"x"=c(10,30),"y"=c(10,30)); Dim=NULL
    #  # plot_set=3; Format=""; Years2Include=round(seq(1,length(Save$Year_Set),length=Nrow)); add=ifelse(speciesI==1,TRUE,TRUE); Dim=par()$mfcol; Report=Save$Report; MappingDetails=MapDetails_List[["MappingDetails"]]; PlotDF=MapDetails_List[["PlotDF"]]; MapSizeRatio=MapDetails_List[["MapSizeRatio"]]; Xlim=MapDetails_List[["Xlim"]]; Ylim=MapDetails_List[["Ylim"]]; Rotate=MapDetails_List[["Rotate"]]; Cex=MapDetails_List[["Cex"]]; FileName=paste0(DateFile,"Summary_"); Year_Set=Save$Year_Set; mar=c(0,0,2,0); oma=c(3.5,3.5,0,0); Legend=list("use"=TRUE,"x"=list(c(76,86),c(70,80))[[speciesI]],"y"=list(c(48,83),c(5,40))[[speciesI]])
    #  SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=3, Format="", Years2Include=round(seq(1,length(Year_Set),length=Nrow)), add=TRUE, Dim=par()$mfcol, Report=Report, MappingDetails=MapDetails_List[["MappingDetails"]], PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], FileName=paste0(RunFile,"Summary_"), Year_Set=Year_Set, mar=c(0,0,2,0), oma=c(3.5,3.5,0,0) )  # , Legend=list("use"=TRUE,"x"=list(c(76,81),c(75,80))[[speciesI]],"y"=list(c(48,83),c(5,40))[[speciesI]])
    #  }
    #  mtext( side=1:2, text=c("Eastings","Northings"), outer=TRUE, line=c(1,0), cex=1.75)
    #})
  }
}}

######################
# Compile results
# INSTRUCTIONS:  Load previous Record and then run
######################

if( DataSet=="Simulated" ) Nyears = length(min(Data_Orig[,'Year']):max(Data_Orig[,'Year']))
if( DataSet=="Pollock" ) Nyears = 34
if( DataSet=="Lingcod" ) Nyears = 13
if( DataSet=="SummerFlounder" ) Nyears = 48

Presults = Bresults = array(NA, dim=c(length(Sim_Settings),Nrep,Nyears,Sim_Settings[[1]]$Nages,5), dimnames=list(names(Sim_Settings),NULL,NULL,paste0("Age_",1:Sim_Settings[[1]]$Nages),c("Est","True","SE","logSE","Design")) )
Results = array(NA, dim=c(length(Sim_Settings),Nrep,3), dimnames=list(names(Sim_Settings),NULL,c("ConvergedTF",'max_grad','mean_Neff')) )
Pchisq = array(NA, dim=c(length(Sim_Settings),Nrep,Nyears,5), dimnames=list(NULL,NULL,NULL,c("mean","median","geometric_mean","harmonic_mean","Design-median")) )
Pnorm_P = Pnorm_B = array(NA, dim=c(length(Sim_Settings),Nrep,Nyears,Sim_Settings[[1]]$Nages), dimnames=list(NULL,NULL,NULL,NULL) )
Neff = array(NA, dim=c(length(Sim_Settings),Nrep,Nyears,2), dimnames=list(NULL,NULL,NULL,c("VAST","Design")) )

for( rI in 1:Nrep ){
for( mI in 1:length(Sim_Settings) ){
  if( DataSet=="Simulated" ){
    Config = names(Sim_Settings)[mI]
    ConfigFile = paste0(DateFile,Config,"/")
    RunFile = paste0(ConfigFile,"r=",rI,"/")
  }else{
    RunFile = ConfigFile = DateFile
  }

  # Load old results
  if( file.exists(paste0(RunFile,"Save.RData")) ){
    load( file=paste0(RunFile,"Index.RData"))
    load( file=paste0(RunFile,"Save.RData"))
    load( file=paste0(RunFile,"Prop_List.RData"))
    load( file=paste0(RunFile,"Design.RData"))

    # Biomass results                           # which(Index$Table[,'Fleet']=="All_areas")
    if( "Ages2Include" %in% names(Save)){
      Ages2Include = Save$Ages2Include
    }else{
      Ages2Include = 1:max(Index$Table[,'Category'])
    }
    Bresults[mI,rI,,Ages2Include,'Est'] = t(Index$Index_ctl[,,1,'Estimate'])
    Bresults[mI,rI,,Ages2Include,'SE'] = t(Index$Index_ctl[,,1,'Std. Error'])
    Bresults[mI,rI,,Ages2Include,'logSE'] = t(Index$log_Index_ctl[,,1,'Std. Error'])
    Bresults[mI,rI,,Ages2Include,'Design'] = t(Design$Index_ct) / 1000

    # Proportion results
    Presults[mI,rI,,Ages2Include,'Est'] = t(Prop_List$Prop_ctl[,,1])
    Presults[mI,rI,,Ages2Include,'SE'] = t(sqrt(Prop_List$var_Prop_ctl[,,1]))
    Presults[mI,rI,,Ages2Include,'Design'] = t(Design$Prop_ct)

    # Check convergence
    Results[mI,rI,'ConvergedTF'] = any( abs(Save$Opt$diagnostics$final_gradient)>0.01 )
    Results[mI,rI,'max_grad'] = max(abs(Save$Opt$diagnostics$final_gradient))
    Results[mI,rI,'mean_Neff'] = mean( Prop_List$Neff_tl )

    # Effective sample size
    Neff[mI,rI,,'VAST'] = Prop_List$Neff_tl[,1]
    Neff[mI,rI,,'Design'] = Design$Neff_t

    # Stuff for simulation
    if( DataSet=="Simulated" ){
      load( file=paste0(RunFile,"SimList.RData"))
      Bresults[mI,rI,,,'True'] = t(SimList$B_at) / 1000
      Presults[mI,rI,,Ages2Include,'True'] = Bresults[mI,rI,,Ages2Include,'True'] / outer(rowSums(Bresults[mI,rI,,Ages2Include,'True']), rep(1,length(Ages2Include)))

      # Quantiles for proportions
      harmonic.mean = function(vec) 1/mean(vec^-1)
      geometric.mean = function(vec) prod(vec)^(1/length(vec))
      for( tI in 1:dim(Presults)[3] ){
        for( pI in 1:4 ){
          f = list( mean, median, geometric.mean, harmonic.mean )[[pI]]
          Neff_tl = apply( Prop_List$Neff_ctl, MARGIN=2:3, FUN=f )
          LRT = 2 * sum(Neff_tl[tI,1]*Presults[mI,rI,tI,Ages2Include,'True'] * log(Presults[mI,rI,tI,Ages2Include,'True']/Presults[mI,rI,tI,Ages2Include,'Est']) )
          Pchisq[mI,rI,tI,c("mean","median","geometric_mean","harmonic_mean")[pI]] = pchisq(LRT, df=length(Ages2Include)-1 )
        }
        LRT = 2 * sum(Design$Neff_t[tI]*Presults[mI,rI,tI,Ages2Include,'True'] * log(Presults[mI,rI,tI,Ages2Include,'True']/Presults[mI,rI,tI,Ages2Include,'Design']) )
        Pchisq[mI,rI,tI,"Design-median"] = pchisq(LRT, df=length(Ages2Include)-1 )
      }

      # Quantiles for index
      Pnorm_B[mI,rI,,] = plnorm( Bresults[mI,rI,,,'True'], meanlog=log(Bresults[mI,rI,,,'Est']), sdlog=Bresults[mI,rI,,,'logSE'] )

      # Quantiles for proportion
      #Pnorm_P[mI,rI,,] = plnorm( Bresults[mI,rI,,,'True'], meanlog=log(Bresults[mI,rI,,,'Est']), sdlog=Bresults[mI,rI,,,'logSE'] )
    }
  }

  if((rI %% 10) == 0) message( "Finished processing replicate ",rI )
}}

# How many replicates are available?
sum( !is.na(Bresults[mI,,1,"Age_1",'Est']))

# Plot error
mI = 1
Nrow = ceiling(sqrt(Sim_Settings[[1]]$Nages))
  Ncol = ceiling(Sim_Settings[[1]]$Nages/Nrow)
for( pI in 1:2 ){
for( p2 in 1:2 ){
  ThorsonUtilities::save_fig( file=paste0(DateFile,ifelse(pI==1&p2==1,"Fig_2_",""),c("P","B")[pI],"_",c("Error","RE")[p2],"_results_"), suffix=c("LO","HI"), width=Ncol*2.5, height=Nrow*2.5, res=c(200,600), FUN=function(){
    par( mfrow=c(Nrow,Ncol), mar=c(0.5,0.5,2,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(2,4,0,1), xaxs="i" )
    if( pI==1 ) Array = Presults
    if( pI==2 ) Array = Bresults
    if( p2==1 ) f = function(est, true) est - true
    if( p2==2 ) f = function(est, true) (est-true)/true
    MRE_vast = apply(f(Array[mI,,,,'Est'],Array[mI,,,,'True']), MARGIN=c(2,3), FUN=mean, na.rm=TRUE)
    SDRE_vast = apply(f(Array[mI,,,,'Est'],Array[mI,,,,'True']), MARGIN=c(2,3), FUN=sd, na.rm=TRUE)
    MRE_design = apply(f(Array[mI,,,,'Design'],Array[mI,,,,'True']), MARGIN=c(2,3), FUN=mean, na.rm=TRUE)
    SDRE_design = apply(f(Array[mI,,,,'Design'],Array[mI,,,,'True']), MARGIN=c(2,3), FUN=sd, na.rm=TRUE)
    #f(Array[mI,,,,'Est'],Array[mI,,,,'True'])
    for( cI in 1:Sim_Settings[[1]]$Nages ){
      if( !any(is.na(SDRE_vast[,cI])) ){
        Ybounds_vast = cbind(MRE_vast[,cI]-SDRE_vast[,cI], MRE_vast[,cI]+SDRE_vast[,cI])
        Ybounds_design = cbind(MRE_design[,cI]-SDRE_design[,cI], MRE_design[,cI]+SDRE_design[,cI])
        if( pI==1 & p2==1 ){
          Ylim = c(-0.04,0.06)
        }else{
          Ylim = c(-1,1) * max(max(abs(Ybounds_vast)),max(abs(Ybounds_design)))
        }
        SpatialDeltaGLMM::Plot_Points_and_Bounds_Fn( x=1:max(SimList$t_i), y=MRE_vast[,cI], xaxt="n", fn=plot, type="l", col="red", lwd=2, ybounds=Ybounds_vast, bounds_type="shading", col_bounds=rgb(1,0,0,0.2), ylim=Ylim*c(1,1.2), ylab="", xlab="", main=paste0("Age ",cI), yaxt="n" )
        SpatialDeltaGLMM::Plot_Points_and_Bounds_Fn( x=1:max(SimList$t_i), y=MRE_design[,cI], fn=lines, col="blue", lwd=2, ybounds=Ybounds_design, bounds_type="shading", col_bounds=rgb(0,0,1,0.2), ylim=Ylim, ylab="", xlab="", main=paste0("Age ",cI) )
        abline( h=0, lwd=2 )
        if( p2==1 & pI==1 ){
          Mult = 100
          Char = "%"
          Digits = c(2,2)
        }else{
          Mult = 1
          Char = ""
          Digits = c(4,3)
        }
        legend( "top", legend=paste0("Model-based: ",formatC(Mult*mean(MRE_vast[,cI]),format="f",digits=Digits[1]),Char," (",formatC(Mult*mean(SDRE_vast[,cI]),format="f",digits=Digits[2]),Char,")"), bty="n", text.col="red", inset=-0.03 )
        legend( "top", legend=paste0("Design: ",formatC(Mult*mean(MRE_design[,cI]),format="f",digits=Digits[1]),Char," (",formatC(Mult*mean(SDRE_design[,cI]),format="f",digits=Digits[2]),Char,")"), bty="n", text.col="blue", inset=0.03 )
        if(cI > (Sim_Settings[[1]]$Nage-Ncol)) axis(1)
        if( (cI%%par('mfrow')[2]) == 1 ){
          if( pI==1 & p2==1 ){
            At = seq(-0.04,0.06,by=0.02)
            axis(2, at=At, labels=paste0(At*100,"%") )
          }else{
            axis(2)
          }
        }
      }
    }
    if( pI==1 & p2==1 ){
      mtext( side=1:2, text=c("Year","Error for estimates of proportion-at-age"), outer=TRUE, line=c(0,2), cex=1.2 )
    }else{
      mtext( side=1:2, text=c("Year","Error"), outer=TRUE, line=c(0,0), cex=1.2 )
    }
  })
}}

ThorsonUtilities::save_fig( file=paste0(DateFile,"Prop_quantiles"), width=6, height=6, res=c(200), FUN=function(){
  par( mfrow=c(2,2), mar=c(3,3,1,1), mgp=c(1.75,0.25,0), tck=-0.02, xaxs="i", yaxs="i" )
  for( pI in 1:4 ){
    hist( Pchisq[,,,pI], freq=FALSE, xlab="Quantile", ylab="Proportion", main=dimnames(Pchisq)[[4]][pI], col="lightgrey", breaks=seq(0,1,by=0.1) )
    abline( h=1, lwd=2, lty="dashed" )
  }
})

ThorsonUtilities::save_fig( file=paste0(DateFile,"Fig_3_Prop_quantiles--median_"), width=4, height=4, res=c(200,600), suffix=c("LO","HI"), FUN=function(){
  par( mar=c(3,3,1,1), mgp=c(1.75,0.25,0), tck=-0.02, xaxs="i", yaxs="i" )
  #hist( Pchisq[,,,"median"], freq=FALSE, xlab="Quantile", ylab="Proportion", main="", col="lightgrey", breaks=seq(0,1,by=0.1) )
  ThorsonUtilities::Hist_Fn( x=list(Pchisq[,,,"median"],Pchisq[,,,"Design-median"]), freq=FALSE, xlab="Quantile", ylab="Proportion", main="", col=c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)), breaks=seq(0,1,by=0.1) )
  abline( h=1, lwd=2, lty="dashed" )
})

ThorsonUtilities::save_fig( file=paste0(DateFile,"Index_quantiles"), width=4, height=4 )
  par( mar=c(3,3,1,1), mgp=c(1.75,0.25,0), tck=-0.02, xaxs="i", yaxs="i" )
  hist( Pnorm_B, freq=FALSE, xlab="Quantile", ylab="Proportion", main="", col="lightgrey", breaks=seq(0,1,by=0.1) )
  abline( h=1, lwd=2, lty="dashed" )
dev.off()

# Effective sample size comparison
ThorsonUtilities::save_fig( file=paste0(DateFile,"Fig_4_Neff_comparison_"), suffix=c("LO","HI"), width=5, height=5, res=c(200,600), FUN=function(){
  par( mar=c(3,3,2,1), mgp=c(1.75,0.25,0), tck=-0.02, xaxs="r", yaxs="r" ) # , xaxs="i", yaxs="i"
  plot( x=Neff[,,,'Design'], y=Neff[,,,'VAST'], xlab="Design", main="Estimated sample size", ylab="Spatio-temporal model", log="xy", ylim=range(Neff,na.rm=TRUE), xlim=range(Neff,na.rm=TRUE) )
  Lm = lm( log(as.vector(Neff[,,,'VAST'])) ~ 1, offset=log(as.vector(Neff[,,,'Design'])) )
  legend( "bottomright", bty="n", legend=paste0("Change = ",formatC(100*exp(summary(Lm)$coef[1,1])-100,format="f",digits=2),"% (",formatC(100*summary(Lm)$coef[1,2],format="f",digits=2),"%)") )
  abline( a=0, b=1, lwd=1, lty="dashed" )
  #abline( a=0, b=Lm$coef[1], lwd=2, lty="dotted" )
  log_X = log(range(Neff,na.rm=TRUE) * c(0.5,2))
  log_Y = summary(Lm)$coef[1,1] + log_X
  lines( x=exp(log_X), y=exp(log_Y), lwd=2, lty="dotted")
})

Files = c(
  "2018-05-02_LingExpand_FbyFcm--REML=F_LN_Field=1111_Rho=0000_nx=100_Npool=100_strata=ORWA",
  "2018-05-02_LingExpand_Mby2cm--REML=F_LN_Field=1111_Rho=0000_nx=100_Npool=100_strata=ORWA"
)
ThorsonUtilities::save_fig( file=paste0(RootFile,Files[1],"/Fig_C1_Diagnostics_"), suffix=c("LO","HI"), width=6.5, height=6.5, res=c(200,600), FUN=function(){
  par( mfrow=c(2,2), mar=c(1,3,0.5,0.5), oma=c(2,0,2.5,1.5), mgp=c(2,0.5,0), tck=-0.02 )
  for( Sex in 1:2 ){
    #
    Dir = paste0( RootFile,Files[Sex],"/" )

    # Diagnostics
    load(file=paste0(Dir,"Enc_prob.RData"))

    # Aggregated QQ plot
    load(file=paste0(Dir,"Q.RData"))

    # Plot for encoutner probabilities
    interval_width = 1.96
    plot( x=Enc_prob$Diag_z[,'midpoints_z'], y=Enc_prob$Diag_z[,'freq_z'], pch=20, cex=1.2, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="" )
    SpatialDeltaGLMM:::Plot_Points_and_Bounds_Fn( x=Enc_prob$Diag_z[,'midpoints_z'][which(!is.na(Enc_prob$Diag_z[,'mean_z']))], y=Enc_prob$Diag_z[,'mean_z'][which(!is.na(Enc_prob$Diag_z[,'mean_z']))], ybounds=(Enc_prob$Diag_z[,'mean_z']%o%c(1,1)+Enc_prob$Diag_z[,'sd_mean_z']%o%c(-interval_width,interval_width))[which(!is.na(Enc_prob$Diag_z[,'mean_z'])),], lwd=2, bounds_type="shading", col_bounds=rgb(1,0,0,0.2), col="red" )
    mtext( side=4, text="Observed encounter frequency", line=0.5 )
    abline(a=0, b=1, lty="dotted", lwd=2 )
    if(Sex==2) mtext( side=1, "Predicted encounter probability", line=1.5 )
    if(Sex==1) mtext( side=3, text="Encounter probability and\nobserved proportion", line=0.5 )
    if(Sex==1) legend( "topleft", legend=c("Observed","Predicted"), fill=c("black","red"), bty="n")
    mtext( side=2, text=c("Female","Male")[Sex], line=1.5 )

    # Disaggregated QQ plot
    Qtemp = unlist( sapply(Q, FUN=function(qlist){qlist$Q}) )
    Order = order(Qtemp)
    plot(x=seq(0,1,length=length(Order)), y=Qtemp[Order], xlab="", ylab="", type="l", lwd=3, main="" )
    mtext( side=4, text="Empirical distribution", line=0.5 )
    if(Sex==2) mtext( side=1, "Uniform distribution", line=1.5 )
    abline(a=0,b=1)
    if(Sex==1) mtext( side=3, text="Quantile-Quantile distribution\n(aggregate)", line=0.5 )
  }
})
