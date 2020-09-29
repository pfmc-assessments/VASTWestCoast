#' @param range.lat A vector of two numeric values specifying the range of
#' latitude values of interest. Values must be in decimal degrees.
#' The first entry is the smaller value, i.e., southern border, and the second
#' entry is the larger value, i.e., northern border. The default value is
#' \code{c(-Inf, Inf)}, which leads to a range of the entire coast and if the
#' input argument is being used to truncate the data, the data is not changed.
#' To limit the data to California use \code{c(32.5, 42)} and
#' to limit the data to Washington and Oregon use \code{c(42, 49)}.
#' These ranges typically match up with the outer ranges of all
#' combined strata you wish to predict to, but
#' this does not have to be the case.
