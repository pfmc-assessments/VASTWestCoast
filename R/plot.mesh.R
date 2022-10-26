#' Plot mesh
#'
#' Plot a mesh created from the [get_mesh] that is saved in
#' the specified directory as `VASTWestCoast_mesh`.
#'
#' @param mesh An \pkg{INLA} mesh resulting from [get_mesh()].
#' @template savedir
#' @template lab.survey
#'
#' @author Kelli F. Johnson
#' @return A figure nine inches long (roughly fitting on a single page)
#' is saved to the disk, along with a caption. The files are saved
#' using the name VASTWestCoast_mesh.xxx. The \pkg{ggplot2} is also
#' returned for future manipulation.
#' @seealso See [INLA::plot.inla.mesh()] for regular base R
#' methods for plotting meshes, which can be called using
#' `plot(meshobject)`.

plot.mesh <- function(mesh, savedir = getwd(),
  lab.survey = "survey") {

  savedir <- normalizePath(savedir)
  pngout <- file.path(savedir, "VASTWestCoast_mesh.png")
  captionout <- file.path(savedir, "VASTWestCoast_mesh.csv")
  stext <- nwfscSurvey::GetSurveyAbb.fn(lab.survey)

  idx <- cbind(
    mesh$graph$tv[1:nrow(mesh$graph$tv),c(1:3, 1), drop = FALSE],
    NA)
  data <- data.frame(x = mesh$loc[t(idx), 1], y = mesh$loc[t(idx), 2],
    group = rep(1:NROW(idx), each = 5))
  data <- data[!is.na(data[, 1]), ]
  data[, 1:2] <- suppressWarnings(data.frame(
    FishStatsUtils::project_coordinates(
      X = data[, 1], Y = data[, 2],
      projargs="+proj=longlat +ellps=WGS84",
      origargs="+proj=utm +datum=WGS84 +units=km +zone=10")))

  g <- plot_westcoast()
  g <- g + ggplot2::geom_polygon(data = data, fill = NA, color = "black",
    aes(x = .data[["x"]], y = .data[["y"]], group = .data[["group"]]))

  if ("data.inner" %in% names(mesh)) {
    locsll <- data.frame(suppressWarnings(FishStatsUtils::project_coordinates(
      X = mesh[["data.inner"]][, "X"],
      Y = mesh[["data.inner"]][, "Y"],
      projargs="+proj=longlat +ellps=WGS84",
      origargs="+proj=utm +datum=WGS84 +units=km +zone=10"
      )),
      type = ifelse(mesh[["data.inner"]][, "Catch_KG"] > 0, "success", "empty"))
    g <- g + geom_point(data = locsll,
      aes(x = .data[["X"]], .data[["Y"]],
        col = .data[["type"]])) + 
    scale_color_manual(values = c(
      grDevices::rgb(1, 1, 1, 1),
      grDevices::rgb(0, 0, 0, 0.1))) + theme(legend.position = "none")
  }
  suppressMessages(ggplot2::ggsave(plot = g,
    file = pngout,
    height = 9, units = "in"))

  caption <- plot_caption(text = paste0(
    "Spatially-explicit constrained delaunay triangulation mesh for the ",
    ifelse(stext == "", lab.survey, stext),
    " using ", mesh$n, " knots ",
    "and an outer boundary defined by the spatial footprint of the survey.",
    ifelse("data.inner" %in% names(mesh),
      paste(" Points show positive and negative tows (transparent black and",
        "empty white points, respectively) for the species across all years."),
      "")),
    figname = "mesh", survey = lab.survey, savefile = captionout)

  return(g)
}
