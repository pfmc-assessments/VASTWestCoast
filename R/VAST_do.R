#' Shortened Code for \code{\link[FishStatsUtils:fit_model]{VAST}}
#' 
#' Major workhorse to run a VAST model using data and settings that are
#' specific to the US West Coast. Many parameters are still customizable,
#' but many parameters are also set in the background based on the sampling
#' protocols and data availability that are specific to each survey.
#' 
#' @param Database A data base entered and returned from or just returned
#' from \code{\link{get_data}()}.
#' @param settings A list of settings used to run the spatiotemporal model.
#' The full list of settings can be seen by running
#' \code{\link{get_settings}()}, and
#' any settings that are not included in the list supplied to \code{settings}
#' will be added using the default values seen \code{get_settings()}.
#' @param conditiondir A directory, either a full or relative path,
#' that will be used to save the results.
#' The directory will be created recursively if it does not already exist.
#'
VAST_do <- function(Database, settings, conditiondir) {
	
	spp <- settings[["Species"]]
	survey <- get_spp(spp)["survey"]
	overdispersion <- settings[["overdispersion"]]
  
  dir.create(conditiondir, showWarnings = FALSE, recursive = TRUE)

  info <- FishStatsUtils::make_settings(
    n_x = settings[["nknots"]],
    Region = "california_current",
    purpose = "index2",
    fine_scale = FALSE,
    strata.limits = settings[["strata"]],
    #zone = NA, #default
    FieldConfig = settings[["FieldConfig"]],
    RhoConfig = settings[["RhoConfig"]],
    OverdispersionConfig = overdispersion,
    ObsModel = settings[["ObsModelcondition"]],
    bias.correct = TRUE,
    #calculate derived quantities of interest, no harm
    #Options = ,#default is more than original VASTWestCoast
    #use_anisotropy = TRUE, #default
    Version = settings[["version"]],
    #treat_nonencounter_as_zero = TRUE, #default
    #VamConfig = c(Method = 0, Rank = 0, Timing = 0), #default
    max_cells = Inf, #default is max( 2000, n_x*10 )
    knot_method = "samples", #default is "grid"
    n_categories = length(unique(Database$Sci))
  )
  out <- FishStatsUtils::fit_model(
    settings = info,
    Lat_i = Database[, "Lat"],
    Lon_i = Database[, "Lon"],
    t_i = Database[, "Year"],
    c_iz = as.numeric(Database[, "Sci"]) - 1,
    b_i = Database[, "Catch_KG"],
    a_i = Database[, "AreaSwept_km2"],
    v_i = as.numeric(Database[, "Vessel"], as.is = FALSE) - 1,
    working_dir = conditiondir,
    # Xconfig_zcp = ,
    # X_gtp = ,
    # X_itp = ,
    Q_ik = if (settings[["Passcondition"]]) {
        as.matrix(Database[, "Pass", drop = FALSE])
      } else {
        NULL
      },
    #newtonsteps = 1, #default
    extrapolation_args = c(info["zone"], info["Region"], info["strata.limits"],
      surveyname = switch(survey,
        Triennial = "propInTriennial",
        AFSC.Slope = "propInSlope98_00",
        NWFSC.Slope = "propInSlope02",
        #default
        "propInWCGBTS")),
    spatial_args = list(randomseed = 1, nstart = 100, iter.max = 1e3),
    # optimize_args = ,
    # model_args = ,
    silent = TRUE,
    run_model = TRUE)
  maps <- FishStatsUtils::plot_results(settings = info, fit = out,
    working_dir = file.path(conditiondir, .Platform$file.sep), check_residuals = FALSE)
  summary_nwfsc(obj = out$tmb_list$Obj, parameter_estimates = out$parameter_estimates,
    savedir = conditiondir)

  save(info, out, Database, settings, conditiondir, spp, survey, maps, overdispersion,
    file = file.path(conditiondir, "Save.RData"))
  save(Database, file = file.path(conditiondir, "DatabaseSave.RData"))

  return(NULL)
}