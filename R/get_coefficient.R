#' Calculate the delta coefficient of a model
#'
#' Calculate the delta coefficient of a simulation. The coefficient
#' represents the relationship between the estimation method and the
#' operating model log-index of abundance. The coefficient should be
#' close to one, which indicates that the index is well calibrated.
#'
#' @param data A data frame with columns for each year of the index
#' generated for the operating model and estimated from the estimation
#' method.
#'
#' @importFrom stats lm
#' @return A vector of delta values, one for each row of the data frame.
#'
#' @author Kelli Faye Johnson
#' @export
#'
#' @examples
#' fake <- data.frame("om_index1" = 2, "om_index2" = 4, "om_index3" = 4,
#'   "em_index1" = 10, "em_index2" = 18, "em_index3" = 18)
#' get_coefficient(fake)
#'
get_coefficient <- function(data) {

  indexdatatouse <- data[, grepl("index", colnames(data))]
  delta <- apply(indexdatatouse, 1, function(x) {
    data <- data.frame(
      "om" = x[grep("om", names(x))],
      "em" = x[grep("em", names(x))],
      "time" = 1:(length(x)/2))
    if (any(is.na(data))) return(NA)
    coef(stats::lm(I(log(em)) ~ time + I(log(om)), data = data))[3]
  })
  return(delta)
}
