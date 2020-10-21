#' Plot the US West Coast
#'
#' Plot the US West Coast using data from
#' \code{\link[rnaturalearth]{ne_countries}}. The axes are in decimal degrees
#' and tick marks are labeled as such; thus, no labels are provided for the
#' axes themselves.
#'
#' @param g A ggplot object that you would like to add a map of the US West
#' Coast to.The coordinates of the object provided need to be in decimal
#' degrees such that the units of the added coastline will align with the
#' provided data. If a value is not supplied for this argument, then a
#' map of the US West Coast is returned, rather than adding to an object.
#' @param xlim A vector of two values supplying the limits to the x axis.
#' The default values focus the resulting map on the waters included in the
#' exclusive economic zone of the US. Units are in decimal degrees.
#' @param ylim A vector of two values supplying the limits to the y axis.
#' The default values focus the resulting map on the area between the Candian-
#' US and US-Mexico borders. Units are in decimal degrees.
#'
#' @author Kelli Faye Johnson
#' @export
#' @return A ggplot2 object providing a map of the US West Coast. The map is
#' returned visibly to provide easy printing capabilities.
#'
plot_westcoast <- function (g, xlim = c(-127.15, -116.5), ylim = c(31.9, 49.5)) 
{
    if (.Platform$OS.type == "windows") {
        world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    }
    else {
        load(paste0(find.package("VASTWestCoast"), "/data/world_hires.RData"))
        # Avoid warning: st_crs<- : replacing crs does not reproject data; use st_transform for that 
        oldOpt <- options(warn = -1)
        sf::st_crs(world) = 4326
        options(oldOpt)
    }
    if (missing(g)) {
        g <- ggplot2::ggplot(data = world)
    }
    gg <- g + ggplot2::geom_sf(data = world) + ggplot2::coord_sf(xlim = xlim, 
        ylim = ylim, expand = FALSE) + ggplot2::theme_bw() + 
        ggplot2::labs(x = "", y = "")
    return(gg)
}

