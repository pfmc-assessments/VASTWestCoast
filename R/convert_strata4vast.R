#' Convert Area-Based Strata to VAST Strata
#'
#' Convert strata defined using \code{\link[nwfscSurvey]{getStrata.fn}()}
#' to strata that will work with VAST. Results will be more consistent using
#' these defined strata rather than All_Areas, which predicts to the entire
#' extrapolation grid.
#'
#' @param strata A data frame generated using
#' \code{\link[nwfscSurvey]{getStrata.fn}()}.
#' @param overridedepth A logical value specifying if \code{mindepth} and
#' \code{maxdepth} should be used instead of the depths that are available
#' within \code{strata}. These depths will always replace NA values, but
#' if \code{TRUE} all depths will be replaced with the argument values.
#' The default is to not replace them, i.e., \code{overridedepth = FALSE}.
#' @param mindepth The depth in meters that you want to use as a boundary for
#' the shallow edge of all strata. The default value is 55 m.
#' All \code{NA} values will be replaced with \code{mindepth}.
#' @param maxdepth The maximum depth that you want to use as a boundary for
#' the deep edge of all strata. The default value is 1280 m.
#' All \code{NA} values will be replaced with \code{maxdepth}.
#'
#' @export
#' @author Kelli Faye Johnson
#' @return A data frame with one row per strata.
#' Strata can overlap and need not be unique.
#' \itemize{
#' \item STRATA (the name of the strata, only used for plotting later),
#' \item north_border (northern latitude of the strata),
#' \item south_border (southern latitude of the strata),
#' \item shallow_border (shallow depth (m) of the strata), and
#' \item deep_border (deep depth (m) of the strata).
#' }
#' @examples
#' convert_strata4vast(nwfscSurvey::getStrata.fn("sablefish"))
#'
convert_strata4vast <- function(strata,
  overridedepth = FALSE, mindepth = 55, maxdepth = 1280) {

  sp <- do.call("rbind", strsplit(strata[, "name"], "_"))
  strata[, "dname"] <- sp[, 1]
  strata[, "STRATA"] <- sp[, 2]

  new <- stats::reshape(strata, direction = "wide",
    timevar = "dname", sep = "_",
    idvar = grep("STRATA|dd", colnames(strata), value = TRUE),
    drop = grep("^name", colnames(strata), value = TRUE))
  colnames(new) <- gsub("Depth_m\\.1_(s[a-z]+)", "\\1_border", colnames(new))
  colnames(new) <- gsub("Depth_m\\.2_(d[a-z]+)", "\\1_border", colnames(new))
  colnames(new) <- gsub("Latitude_dd\\.1", "south_border", colnames(new))
  colnames(new) <- gsub("Latitude_dd\\.2", "north_border", colnames(new))

  keep <- c(
    "STRATA",
    "north_border", "south_border",
    "shallow_border", "deep_border")
  missing <- keep[!keep %in% colnames(new)]
  if (length(missing) > 0) {
    new[, missing] <- NA
  }
  alter <- new[order(new[, "STRATA"]), keep]
  # Check depth columns and override them if TRUE
  if (overridedepth) {
    alter[, "deep_border"] <- maxdepth
    alter[, "shallow_border"] <- mindepth
  } else {
    alter[is.na(alter[, "deep_border"]), "deep_border"] <- maxdepth
    alter[is.na(alter[, "shallow_border"]), "shallow_border"] <- mindepth
  }

  # Don't do a summary strata if only one row or coast is present
  if (nrow(alter) == 1 | "coast" %in% alter[, "STRATA"]) {
    return(alter)
  }
  sumrow <- data.frame(
    STRATA = paste(alter[, "STRATA"], collapse = "_"),
    north_border = max(alter[, "north_border"]),
    south_border = min(alter[, "south_border"]),
    shallow_border = min(alter[, "shallow_border"]),
    deep_border = max(alter[, "deep_border"])
  )
  return(rbind(sumrow, alter))
}
