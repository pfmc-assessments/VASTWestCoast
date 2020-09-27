#' Plot Inputgrid
#'
#' Plot a map of the grid cells used in the prediction of VAST results.
#'
#' @param data A data frame returned from \code{\link{get_inputgrid}}
#' that includes the following columns:
#' Latitude, available as \code{"Lat"};
#' Longitude, available as \code{"Lon"};
#' Area, available as \code{"Area_km2"}; and
#' Depth in meters.
#' @template savedir
#' @template lab.survey
#'
#' @author Kelli Faye Johnson
#' @return A figure 9 inches long (roughly fitting on a single page)
#' is saved to the disk, along with a caption. The files are saved
#' using the name VASTWestCoast_mesh.[ext]. The \pkg{ggplot2} object
#' is returned for future manipulation.

plot.inputgrid <- function(data, savedir = getwd(),
  lab.survey = "survey") {

  savedir <- normalizePath(savedir)
  pngout <- file.path(savedir, "VASTWestCoast_inputgrid.png")
  captionout <- file.path(savedir, "VASTWestCoast_inputgrid.csv")

  g <- plot_westcoast() +
    ggplot2::geom_point(data = data[data[, "Area_km2"] > 0, ],
    ggplot2::aes(
      x = .data[["Lon"]],
      y = .data[["Lat"]],
      col = .data[["Depth"]])) +
    ggplot2::labs(colour = "Depth (m)") +
    ggplot2::geom_text(
      data = data.frame(Label = nwfscSurvey::GetSurveyAbb.fn(lab.survey)),
      ggplot2::aes(x = Inf, y = Inf,
        label = paste0(gsub(" ", "  \n", .data[["Label"]]), "  ")),
      hjust = 1, vjust = 6, cex = 5.0) +
    ggplot2::scale_color_gradient(trans = "reverse")

  suppressMessages(ggplot2::ggsave(plot = g,
    filename = pngout,
    height = 9, units = "in"))

  g[["caption"]] <- plot_caption(text = paste0(
    "A map of the prediction grid colored by depth (m) for the ",
    nwfscSurvey::GetSurveyAbb.fn(lab.survey), ".",
    " White space indicates areas not included in the predictions."
    ),
    figname = "inputgrid", survey = lab.survey, savefile = captionout)

  return(g)
}
