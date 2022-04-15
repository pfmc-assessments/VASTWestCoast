#' Shortened code for [FishStatsUtils::fit_model]
#'
#' Run a VAST model using data and settings that are
#' specific to the US West Coast. Many parameters are still customizable,
#' but many parameters are also set in the background based on the sampling
#' protocols and data availability that are specific to each survey.
#'
#' @param Database A data base returned from [get_data()].
#' Users can also supply their own database as long as it has the same
#' column names as the one returned from [get_data()].
#' @param settings A list of settings used to run the
#' index-standardization model.
#' The full list of settings is available with
#' [get_settings()], and
#' any default settings that are not included in the user-supplied list
#' that is passed to to `settings`
#' will be added using the default values in the master list.
#' @template conditiondir
#' @template compiledir
#' @param region A single character value specifying the region of interest.
#' This value will be passed to
#' [FishStatsUtils::make_extrapolation_info()], where the region
#' defines the background data set that is used to create the prediction grid.
#' If the default value of `"user"` is used, then [VAST_do] calls
#' [get_inputgrid] to define a corrected version of the
#' `california_current_grid` that is used if
#' `region = "california_current"`. The default removes all grid cells
#' that fall in depths shallower than 35 m (including those on land) and
#' grid cells within the Cowcod Conservation Areas. Furthermore, it will only
#' predict to the strata that are included in the stratifications rather than
#' the entire area.
#'
#' @author Kelli Faye Johnson
#' @export
#' @return Nothing is returned from the function, but many items are saved
#' to the disk, largely by other helper functions that are called by
#' [VAST_do]. All R objects are saved in a file called `"Save.RData"`
#' within `conditiondir`.
#'
#' @examples
#' \dontrun{
#' # Use the default settings to run the model for WCGBTS - sablefish
#' settings <- get_settings()
#' surveyspp <- get_spp(settings$Species)
#' Database <- get_data(survey = surveyspp["survey"], species = surveyspp["species"])
#' check <- VAST_do(Database = Database, settings = settings,
#'   conditiondir = getwd(), compiledir = getwd())
#' }
#'
VAST_do <- function(Database, settings, conditiondir, compiledir,
  region = c("user", "california_current")) {

  region <- match.arg(region, several.ok = FALSE)
  spp <- settings[["Species"]]
  survey <- get_spp(spp)["survey"]
  spp_sci <- paste(strsplit(spp, "_")[[1]][2:3], collapse = " ")

  dir.create(conditiondir, showWarnings = FALSE, recursive = TRUE)

  local <- VAST_mesh(data = Database, survey = survey,
    savedir = conditiondir,
    numknots = settings[["nknots"]],
    range.depth = c(
      -1 * min(settings[["strata"]][,"shallow_border"]),
      -1 * max(settings[["strata"]][,"deep_border"])),
    range.lat = c(
      min(settings[["strata"]][,"south_border"]),
      max(settings[["strata"]][,"north_border"])))
  subdata <- local$mesh$data.inner
  info <- FishStatsUtils::make_settings(
    n_x = settings[["nknots"]],
    Region = region,
    purpose = "index2",
    fine_scale = settings[["fine_scale"]],
    strata.limits = settings[["strata"]],
    #zone = NA, #default
    # automatically detecting zone in `FishStatsUtils::make_extrapolation_info()`
    # DO NOT allow users to control just use default
    FieldConfig = settings[["FieldConfig"]],
    RhoConfig = settings[["RhoConfig"]],
    OverdispersionConfig = settings[["overdispersion"]],
    ObsModel = settings[["ObsModelcondition"]],
    bias.correct = TRUE,
    #calculate derived quantities of interest, no harm
    #Options = ,#default is more than original VASTWestCoast
    use_anisotropy = settings[["use_anisotropy"]],
    Version = settings[["version"]],
    #treat_nonencounter_as_zero = TRUE, #default
    #VamConfig = c(Method = 0, Rank = 0, Timing = 0), #default
    max_cells = Inf, #default is max( 2000, n_x*10 )
    knot_method = "samples", #default is "grid"
    n_categories = length(unique(subdata$Sci))
  )
  anisotropic_mesh <- NULL
  if (info["Region"] == "user") {
    anisotropic_mesh <- local[["mesh"]]
  }
  catchability_data <- NULL
  if (settings[["Passcondition"]]) {
    passrange <- range(subdata[, "Pass"])
    catchability_data <- subdata[, "Pass", drop = FALSE] - mean(passrange)
  }
  out <- tryCatch(FishStatsUtils::fit_model(
    settings = info,
    Lat_i = subdata[, "Lat"],
    Lon_i = subdata[, "Lon"],
    t_i = subdata[, "Year"],
    c_iz = as.numeric(subdata[, "Sci"]) - 1,
    b_i = subdata[, "Catch_KG"],
    a_i = subdata[, "AreaSwept_km2"],
    v_i = as.numeric(subdata[, "Vessel"], as.is = FALSE) - 1,
    working_dir = conditiondir,
    # Xconfig_zcp = ,
    # X_gtp = ,
    # X_itp = ,
    # Density covariates
    Q1_formula = ~ Pass,
    Q2_formula = ~ Pass,
    catchability_data = catchability_data,
    #newtonsteps = 1, #default
    extrapolation_args = c(info["zone"], info["Region"], info["strata.limits"],
      surveyname = convert_survey4vast(survey),
      input_grid = list(local[["inputgrid"]])),
    spatial_args = list(
      randomseed = 1,
      nstart = 100,
      iter.max = 1e3,
      anisotropic_mesh = anisotropic_mesh,
      Kmeans = NULL,
      fine_scale = info[["fine_scale"]]),
    # optimize_args = ,
    model_args = list(CompileDir = compiledir),
    silent = TRUE,
    run_model = TRUE), error = function(e) e)
  if ("simpleError" %in% class(out) |
      is.null(out$parameter_estimates$SD)) {
    save(out, file = file.path(conditiondir, "Error.RData"))
    save(list = ls(all.names = TRUE), file = file.path(conditiondir, "Save.RData"))
    return(out)
  }

  maps <- tryCatch(suppressWarnings(FishStatsUtils::plot_results(settings = info, fit = out,
    working_dir = file.path(conditiondir, .Platform$file.sep),
    check_residuals = TRUE)),
    error = function(e) e)

  index <- suppressWarnings(FishStatsUtils::plot_biomass_index(
    fit = out,
    DirName = file.path(conditiondir, .Platform$file.sep),
    TmbData = out$data_list, Sdreport = out$parameter_estimates$SD,
    use_biascorr = TRUE,
    Year_Set = out$year_labels, Years2Include = out$years_to_plot,
    strata_names = out$settings$strata.limits$STRATA
  ))
  rsessioninfo <- summary_session(
    savefile = file.path(conditiondir, "VASTWestCoast_session.txt"))
  modelinfo <- summary_nwfsc(obj = out$tmb_list$Obj,
    parameter_estimates = out$parameter_estimates,
    savedir = conditiondir)
  fileindex <- file.path(conditiondir, "Table_for_SS3.csv")
  indexdata <- data.frame(
    Year = out$year_labels[index[["Table"]][, "Time"]],
    Unit = index[["Table"]][, "Units"],
    Fleet = index[["Table"]][, "Stratum"],
    Estimate_metric_tons = index[["Table"]][, "Estimate"],
    SD_log = index[["Table"]][, "Std. Error for Estimate"],
    SD_mt = index[["Table"]][, "Std. Error for ln(Estimate)"]
  )
  utils::write.csv(x = indexdata, file = fileindex, row.names = FALSE)
  plot_ss(file.in = fileindex,
    lab.survey = survey,
    lab.spp = bquote(italic(.(spp_sci))))

  save(list = ls(all.names = TRUE), file = file.path(conditiondir, "Save.RData"))
  save(Database, file = file.path(conditiondir, "DatabaseSave.RData"))

  return(NULL)
}
