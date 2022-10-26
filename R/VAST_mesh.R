#' Create grid and mesh for VASTWestCoast
#'
#' Create the input grid and the mesh based on the survey and
#' format all objects as needed for VASTWestCoast.
#'
#' @param data A data frame extracted from the NWFSC data warehouse using
#' [get_data]. These points will be used as starting locations
#' for the knot locations in the final mesh. Locations do not need to be
#' positive tows, and all tow locations can be included, which is the default.
#' @template survey
#' @template savedir
#' @template numknots
#' @template range.depth
#' @template range.lat
#'
#' @author Kelli F. Johnson
#' @export
#' @return A list with two elements is returned, the first is a input grid
#' for `"extrapolation_args"` and the second is an anisotropic
#' INLA triangulation mesh for `"spatial_args"`. Each of these list
#' elements are also plotted and the plots are saved in `savedir`.
#' @seealso See [get_inputgrid], [get_mesh],
#' [plot.inputgrid], and [plot.mesh].
#'
VAST_mesh <- function(data, survey, savedir = getwd(),
  numknots,
  range.depth = c(-35, -Inf), range.lat = c(-Inf, Inf)) {

  localinputgrid <- get_inputgrid(survey,
    range.depth = range.depth, range.lat = range.lat)
  g <- plot.inputgrid(localinputgrid, savedir = savedir, lab.survey = survey)
  localmesh <- get_mesh(
    data.inner = data[
      data[, "Depth_m"] > -1 * range.depth[1] &
      data[, "Depth_m"] < -1 * range.depth[2] &
      data[, "Lat"] > range.lat[1] &
      data[, "Lat"] < range.lat[2], ],
    data.outer = localinputgrid[localinputgrid[, "Area_km2"] > 0, ],
    numknots = numknots
  )
  g <- plot.mesh(localmesh, savedir = savedir, lab.survey = survey)

  return(list(inputgrid = localinputgrid, mesh = localmesh))
}
