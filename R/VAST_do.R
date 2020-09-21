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
#' @template conditiondir
#' @template compiledir
#' @param region A single character value specifying the region of interest.
#' This value will be passed to
#' \code{\link[FishStatsUtils]{make_extrapolation_info}()}, where the region
#' defines the background data set that is used to create the prediction grid.
#' If the default value of \code{"user"} is used, then \code{VAST_do} calls
#' \code{\link{get_inputgrid}} to define a corrected version of the
#' \code{california_current_grid} that is used if
#' \code{region = "california_current"}. The default removes all grid cells
#' that fall in depths shallower than 35 m (including those on land) and
#' grid cells within the Cowcod Conservation Areas.
#'
VAST_do <- function(Database, settings, conditiondir, compiledir,
  region = c("user", "california_current")) {

  region <- match.arg(region, several.ok = FALSE)
  spp <- settings[["Species"]]
  survey <- get_spp(spp)["survey"]

  dir.create(conditiondir, showWarnings = FALSE, recursive = TRUE)
  localinputgrid <- get_inputgrid(survey)
  if (region == "user"){
    g <- plot.inputgrid(localinputgrid, print = FALSE)
    suppressMessages(ggplot2::ggsave(plot = g, units = "in",
      filename = file.path(conditiondir, "VASTWestCoast_inputgrid.png"),
      height = 9))
  }

  info <- FishStatsUtils::make_settings(
    n_x = settings[["nknots"]],
    Region = region,
    purpose = "index2",
    fine_scale = settings[["fine_scale"]],
    strata.limits = settings[["strata"]],
    #zone = NA, #default
    FieldConfig = settings[["FieldConfig"]],
    RhoConfig = settings[["RhoConfig"]],
    OverdispersionConfig = settings[["overdispersion"]],
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
  out <- tryCatch(FishStatsUtils::fit_model(
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
    catchabilitydata = if (settings[["Passcondition"]]) {
        as.matrix(Database[, "Pass", drop = FALSE])
      } else {
        NULL
      },
    #newtonsteps = 1, #default
    extrapolation_args = c(info["zone"], info["Region"], info["strata.limits"],
      surveyname = convert_survey4vast(survey),
      input_grid = list(localinputgrid)),
    spatial_args = list(randomseed = 1, nstart = 100, iter.max = 1e3),
    # optimize_args = ,
    model_args = list(CompileDir = compiledir),
    silent = TRUE,
    run_model = TRUE), error = function(e) e)
  if ("simpleError" %in% class(out)) {
    if (grepl("Please change model structure to avoid problems", out)) {
      return(out)
    }
  }
  maps <- suppressWarnings(FishStatsUtils::plot_results(settings = info, fit = out,
    working_dir = file.path(conditiondir, .Platform$file.sep),
    check_residuals = TRUE))
  rsessioninfo <- summary_nwfsc(obj = out$tmb_list$Obj,
    parameter_estimates = out$parameter_estimates,
    savedir = conditiondir)

  save(list = ls(all = TRUE), file = file.path(conditiondir, "Save.RData"))
  save(Database, file = file.path(conditiondir, "DatabaseSave.RData"))

  return(NULL)
}
