#' Calculate coverage of an index
#'
#' @param data A data frame with three x columns.
#' @param plot A logical value specifying whether or not to
#' plot the data
#'
#' @importFrom stats reshape
#' @return A vector of information regarding the coverage for the data
#' that was provided in the argument \code{data}. One value will be returned
#' for each row of data.
#'
#' @import ggplot2
#' @author Kelli Faye Johnson
#' @export
#'
get_coverage <- function(data, plot = FALSE) {
  # todo: Fix the data if the input is a named vector

  om <- data[, grep("om_index", colnames(data))]
  omm <- data.frame(t(om), year = 1:NCOL(om))

  # Interval wide to long
  temp <- data[, grep("Index_cyl", colnames(data))]

  coverage <- sapply(seq_along(grep("om_index_[[:digit:]]+", colnames(data))),
    function(x) {
    subdata <- data[, eval(parse(text = sprintf(
      "c(\"om_index_%s\", \"Index_cyl_%s_low\", \"Index_cyl_%s_upp\")",
      x, x, x)))]
    apply(subdata, 1, function(xx) {
      ifelse(any(is.na(xx)), NA, findInterval(xx[1], vec = xx[2:3]))
    })
  })
  coveragea <- apply(coverage, 1,
    function(xxx) sum(ifelse(xxx == 1, 1, 0)) / length(xxx))

  if (plot) {
    ci <- stats::reshape(data = temp, v.names = c("low", "upp"),
      sep = "_", direction = "long",
      varying = lapply(c("low", "upp"), grep, colnames(temp)),
      timevar = "year")    # todo: Add the ability to pull the file name out
    # todo: vectorize this so that it would work on multiple rows.
    gg <- ggplot(ci, aes(x = year)) +
    geom_line(aes(y = low))+
    geom_line(aes(y = upp)) +
    facet_grid(depth~.)+
    geom_point(data = omm, aes(y = X1, x = year)) +
    ylab("index") + theme_bw() +
    theme(strip.background = element_blank(),
    panel.border = element_rect(colour = "black"))
    ggsave(plot = gg, filename = file.path(file, "coverage.jpeg"),
      device = "jpeg")
  }

  return(coveragea)
}
