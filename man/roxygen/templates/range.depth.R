#' @param range.depth A vector of two numeric values specifying the range of
#' depth values of interest. Values must be in meters and negative values
#' represent those below sea level and positive values are assumed to be
#' above sea level. The first entry is the shallow value and the second
#' entry is the deeper cutoff. The default value is
#' \code{c(-35, -Inf)}, which truncates the data to depths deeper than 35 m
#' with no limit on how deep they can be.
