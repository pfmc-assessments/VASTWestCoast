#' Plot indicies of Relative Abundance
#' 
#' Compare estimated indices by plotting them with their confidence
#' intervals on a single plot. 
#' 
#' @param dir A file path with folders containing VASTWestCoast runs.
#' @param recursive A boolean value indicating whether or not you want to
#' recursively search for VAST runs inside \code{dir} or just look 
#' at the top level. 
#' @param area A character value specifying the area to plot.
#' The area is specified as the fleet column in the csv file.
#' @param keepyears A vector of years you want to plot.
#' If \code{NULL}, then all years will be plotted.
#' @param limit A vertical-axis upper limit, which is forced.
#' 
#' @export
#' @import ggplot2
#' 
plot_index <- function(dir, recursive = TRUE, area = "All_areas",
	keepyears = NULL, limit = NULL) {
	options(device = "x11")
	files <- dir(dir, pattern = "Table_for_SS3", full.names = TRUE,
	  recursive = recursive)
	data <- do.call("rbind", 
		mapply(function(x, y) "[<-"(x, "folder", value = y),
		lapply(files, read.table, sep = ",", header = TRUE), 
		basename(dirname(files)), SIMPLIFY = FALSE))
	if (is.null(keepyears)) keepyears <- unique(data[, "Year"])
	data <- data[data[, "Year"] %in% keepyears, ]
	data[, "low"] <- data[, "Estimate_metric_tons"] - 1.96 * data[, "SD_mt"]
	data[, "upp"] <- data[, "Estimate_metric_tons"] + 1.96 * data[, "SD_mt"]
	g <- ggplot(data, aes(Year, Estimate_metric_tons, 
		color = as.factor(Fleet),
  fill = as.factor(folder))) +
    geom_ribbon(data = data,
      aes(ymin = low, ymax = upp), alpha = 0.2,
      show.legend = FALSE) +
    geom_line(data = data, lwd = 1.5)+
    theme_bw() +
    scale_colour_brewer(palette="Spectral", name = "") +
    scale_fill_brewer(palette="Spectral", name = "") +
    # guides(fill = FALSE) +
    theme(legend.key = element_rect(colour = NA, fill = NA),
        legend.justification = c(1, 0),
        legend.position = c(0.35, 0.5)) +
    xlab("year") + ylab("abundance (mt)") + 
    ylim(c(NA, ifelse(is.null(limit), max(data[, "upp"]) * 1.02, limit)))
  dev.new()
  print(g)
	
	invisible(data)
}
