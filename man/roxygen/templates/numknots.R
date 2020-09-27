#' @param numknots The desired number of knots to have in the resulting mesh.
#' This will be approximate because the distance between points used for knots
#' will be used to solve to the ideal number of knots using a while loop.
#' Thus, expect the resulting number of knots to be within plus or minus five
#' percent of this input value. See \code{\link{get_mesh}()} for specifics.
#' The desired value is typically stored in the settings list; see,
#' \code{\link{get_settings}()} and is known as \code{"n_x"} in
#' \code{\link[FishStatsUtils]{make_settings}()}.
