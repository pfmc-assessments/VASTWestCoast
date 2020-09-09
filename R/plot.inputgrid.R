#' Plot Inputgrid

plot.inputgrid <- function(data, print = TRUE) {
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  survey <- unique(data[, "surveyname"])
  g <- ggplot2::ggplot(data = data[data[, "Area_km2"] > 0, ]) +
    ggplot2::geom_point(ggplot2::aes(
      x = .data[["Lon"]],
      y = .data[["Lat"]],
      col = .data[["Depth"]])) +
    ggplot2::geom_sf(data = world) +
    ggplot2::coord_sf(xlim = c(-127.15, -116.50),
      ylim = c(31.90, 50.00), expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "", colour = "Depth (m)") +
    ggplot2::geom_text(data = data.frame(Label = survey), 
      ggplot2::aes(x = Inf, y = Inf,
        label = paste0(gsub(" ", "  \\n", Label), "  ")),
      hjust = 1, vjust = 6, cex = 5.0) +
    ggplot2::scale_color_gradient(trans = "reverse")
  if (print) print(g)
  return(invisible(g))
}
