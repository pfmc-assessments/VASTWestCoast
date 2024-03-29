#' Plot the U.S. West Coast
#'
#' Plot the U.S. West Coast. The axes are in decimal degrees
#' and tick marks are labeled as such; thus, no labels are provided for the
#' axes themselves.
#'
#' @param g A ggplot object that you would like to add a map of the US West
#' Coast to. The coordinates of the object provided need to be in decimal
#' degrees such that the units of the added coastline will align with the
#' provided data. The default is to use an empty {ggplot2} figure.
#' @param xlim A vector of two values supplying the limits to the x axis.
#' The default values focus the resulting map on the waters included in the
#' exclusive economic zone of the US. Units are in decimal degrees.
#' @param ylim A vector of two values supplying the limits to the y axis.
#' The default values focus the resulting map on the area between the Canadian-
#' U.S. and U.S.-Mexico borders. Units are in decimal degrees.
#'
#' @author Kelli F. Johnson
#' @export
#' @return A {ggplot2} object providing a map of the U.S. West Coast. The map
#' is returned visibly to provide easy printing capabilities.
#' @examples
#' \dontrun{
#' westCoast <- plot_westcoast()
#' }
#'
plot_westcoast <- function(g = ggplot2::ggplot(),
                           xlim = c(-127.15, -116.50),
                           ylim = c(31.90, 49.50)) {

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Include the outline of the US West Coast and remove axes labels
  gg <- g +
    ggplot2::geom_sf(data = world) +
    ggplot2::coord_sf(
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "")

  # Return it visibly so users don't have use print()
  return(gg)
}
