#' Calculate the log-ratio of the index data.
#'
#' @param data A data frame or vector of index data. Data should be
#' on the normal scale and will be transformed to the log scale.
#'
#' @importFrom utils tail
#' @return A vector of values log-index values.
#'
#' @examples
#' data <- data.frame("first" = rlnorm(10), "second" = rlnorm(10))
#' ratios <- get_logratio(data)
#'
#' @author Kelli Faye Johnson
#' @export
#'
get_logratio <- function(data) {
  # Create a data frame from a vector to allow for vectorization
  if (is.vector(data)) data <- as.matrix(data)

  # Calculate the log ratio
  out <- apply(data, 2, function(x) {
    log(utils::tail(x, 1)) - log(x[1])
  })
  return(out)
}
