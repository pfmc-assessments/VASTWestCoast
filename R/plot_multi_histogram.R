#' Plot Multiple Histograms
#'
#' Plot multiple histograms for a given column (x axis) by
#' category (colors). Both the stacked histogram and individual
#' density lines are included, where the colors coincide and are
#' labelled in the margin.
#'
#' @param df A data frame.
#' @param col.feature A character value specifying the column of \code{df}
#' that will be used for the x axis.
#' @param col.label A character value specifying the column of \code{df}
#' that will be used for the colors or indivdual lines.
#'
#' @author Kelli F. Johnson
#' @return A ggplot object.
#'
plot_multi_histogram <- function(df, col.feature, col.label) {
  plt <- ggplot2::ggplot(df,
    ggplot2::aes(
      x = .data[[col.feature]],
      fill = .data[[col.label]])) +
  ggplot2::geom_histogram(alpha = 0.5, position = "identity",
    ggplot2::aes(y = ..density..), color = "black") +
  ggplot2::geom_density(alpha = 0.5) +
  labs(x = col.feature, y = "Density") + 
  ggplot2::guides(fill = ggplot2::guide_legend(title = col.label))
  return(plt)

}
