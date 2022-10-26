#' Get Depth by Year
#'
#' Get depth of positive and empty tows by year plotted in a histogram.
#'
#' @param df A data frame from the NWFSC warehouse.
#' @param vline The x-axis location of the vertical line.
#' The default is to include a line at 366 m because of the
#' change in sampled depths in the Triennial Survey. A value of
#' \code{vline = NULL} will remove the line from the plot.
#'
#' @export
#' @author Kelli F. Johnson
#' @return A ggplot object.
#' @examples
#' get_depthbin(Database)
#' \dontrun{
#' tridata <- nwfscSurvey::PullCatch.fn(SurveyName = "Triennial")
#' tridata[, "Catch_KG"] <- tridata[, "total_catch_wt_kg"]
#' for (ii in unique(tri$Common_name)) {
#'   dd <- subset(tri, Common_name == ii)
#'   if (nrow(dd) == 0) next
#'   g <- get_depthbin(dd)
#'   g <- g + ggtitle(ii)
#'   ggsave(g, file = paste0(tempdir(), "Tri_", ii, ".png"))
#' }
#' }
#'
get_depthbin <- function(df, vline = 366) {
  df[, "year"] <- factor(df[, "Year"])
  df[, "positive"] <- ifelse(df[, "Catch_KG"] > 0,
    "positive tow", "empty tow")

  gg <- plot_multi_histogram(df = df, col.feature = "Depth_m", col.label ="year") +
    ggplot2::facet_grid(positive ~ .) +
    ggplot2::geom_vline(xintercept = vline, color = "black",
      linetype = "dashed", size = 1) +
    ggplot2::xlab("Depth (m)") + ggplot2::theme_bw()
  return(gg)
}
