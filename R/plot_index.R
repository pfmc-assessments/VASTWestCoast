#' Comparison Plot of Estimated Indices of Relative Abundance
#'
#' Compare estimated indices and their confidence intervals on a single plot.
#' Results from \code{Table_for_SS3.csv} files are searched for and loaded as
#' objects, which are then plotted by model run and area
#' using unique colors for each run-area combination.
#'
#' @param dir A file path with folders containing VASTWestCoast runs.
#' Presumably each model run will be in its own folder, where these folder
#' names are later used to name the lines on the figure.
#' @param recursive A boolean value indicating whether or not you want to
#' recursively search for VASTWestCoast runs inside \code{dir} or just look
#' at the top level. The default is to look in all available folders.
#' @param area A vector of character values specifying the areas to plot.
#' The area is specified as the Fleet column in the csv file. The default is
#' to keep all of the defined areas and partition the results by the folder
#' it was found in and area using colors.
#' \code{NA} values in the Fleet column will be changed to \code{"unknown"}
#' and words separated using \code{"_"} will be separated using spaces, e.g.,
#' \code{"All_areas"} will become \code{"All areas"}. Only the first underscore
#' is replaced.
#' @param keepyears A vector of years you want to plot.
#' If \code{NULL}, which is the default, then all years are plotted.
#' @param limit A upper limit of the vertical axis, which is forced. The default is
#' to let the data define the upper limit, where all confidence intervals will
#' be included.
#' @param legend_position A vector of two numbers between zero and one that
#' specify the location of the legend relative to the lower-left corner.
#' The default values keeps the legend near the lower-left corner.
#' @template savefile
#'
#' @return A data frame of sourced \code{Table_for_SS3.csv} files that have
#' been subset appropriately based on the input arguments. This data frame was
#' used to generate the image that is returned to the screen and potentially
#' saved to the disk.
#'
#' @export
#' @import ggplot2
#'
plot_index <- function(dir, recursive = TRUE, area = NULL,
  keepyears = NULL, limit = NULL, legend_position = c(0.5, 0.05),
  savefile = NULL) {
  options(device = "x11")

  #get the index information that is saved to the disk
  dir <- normalizePath(dir, mustWork = TRUE)
  files <- dir(dir, pattern = "Table_for_SS3", full.names = TRUE,
    recursive = recursive)
  if (length(files) == 0) {
    stop("No Table_for_SS3 files were found in ", dir)
  }
  data <- do.call("rbind",
    mapply(function(x, y) "[<-"(x, "folder", value = y),
    lapply(files, utils::read.table, sep = ",", header = TRUE),
    basename(dirname(files)), SIMPLIFY = FALSE))

  #subset data and calculate confidence intervals
  if (is.null(keepyears)) keepyears <- unique(data[, "Year"])
  data <- data[data[, "Year"] %in% keepyears, ]
  data[, "low"] <- data[, "Estimate_metric_tons"] - 1.96 * data[, "SD_mt"]
  data[, "upp"] <- data[, "Estimate_metric_tons"] + 1.96 * data[, "SD_mt"]
  if (!is.null(area[1])) {
    data <- data[data[, "Fleet"] %in% area, ]
  }
  data[, "Area"] <- ifelse(is.na(data[, "Fleet"]), "unknown",
    as.character(data[, "Fleet"]))
  data[, "Area"] <- gsub("([a-zA-Z0-9]+)_([a-zA-Z0-9]+)", "\\1 \\2",
    data[, "Area"])

  #plot the information using ggplot
  g <- ggplot(data, aes(data[["Year"]], data[["Estimate_metric_tons"]])) +
    geom_ribbon(data = data,
      aes(ymin = data[["low"]], ymax = data[["upp"]],
        fill = interaction(as.factor(data[["folder"]]), data[["Area"]], sep = " -- ")),
      alpha = 0.2,
      show.legend = FALSE) +
    geom_line(lwd = 1.5,
      aes(col = interaction(as.factor(data[["folder"]]), data[["Area"]], sep = " -- "))) +
    theme_bw() +
    scale_colour_brewer(palette="Spectral", name = "", guide = "legend") +
    scale_fill_brewer(palette="Spectral", name = "") +
    guides(
      colour = guide_legend(
        title = "Colors for combinations of\nmodel run and areas within model run"),
      fill = FALSE) +
    theme(
      legend.key = element_rect(colour = NA, fill = NA),
      legend.justification = c(1, 0),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
      legend.position = legend_position) +
    ylab("Abundance (mt)") +
    ylim(c(NA, ifelse(is.null(limit), max(data[, "upp"]) * 1.02, limit)))
  grDevices::dev.new()
  print(g)
  if (!is.null(savefile)) {
    savefile <- normalizePath(savefile)
    ggsave(plot = g, filename = normalizePath(savefile))
  }

  invisible(data)
}
