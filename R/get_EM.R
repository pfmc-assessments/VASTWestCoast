#' Get information on parameters from an estimation folder
#'
#' @param file An R file with information saved from \code{\link{VAST_EM}}.
#'
#' @return A data frame with parameter values. Parameter names will be
#' prefixed with "em".
#'
#' @author Kelli Faye Johnson
#' @export
#'
get_EM <- function(file) {
  # todo: add number of knots
  if (!file.exists(file)) return(NULL)
  load(file)
  if (is.na(Report$jnll)) return(NULL)
  em <- list()
  if (is.null(sdreport)) return(NULL)
  temp <- sdreport$par.fixed[!
    grepl("^beta", names(sdreport$par.fixed))]
  names(temp) <- paste0("em_", names(temp))
  em <- as.list(temp); rm(temp)
  names(em) <- gsub("L_omega([1-9]{1})_z", "sigmao\\1", names(em))
  names(em) <- gsub("L_epsilon([1-9]{1})_z", "sigmae\\1", names(em))
  names(em) <- gsub("gamma([1-9]{1})_ctp", "depth\\1_km", names(em))
  names(em)[duplicated(names(em))] <- paste0(
    names(em)[duplicated(names(em))], "2")
  em[grep("sigma", names(em))] <-
    abs(as.numeric(em[grep("sigma", names(em))]))

  depthnames <- c(
    "em_depth1_km", "em_depth1_km2",
    "em_depth2_km", "em_depth2_km2")
  em[depthnames[!depthnames %in% names(em)]] <- 0

 if (!"em_ln_H_input" %in% names(em)) {
   em$em_ln_H_input <- 0
   em$em_ln_H_input2 <- 0
 }
 if (length(Report$Range_raw1) == 1) {
    em$em_range1 <- Report$Range_raw1
    em$em_range2 <- Report$Range_raw2
  } else {
    em$em_range1 <- NA
    em$em_range2 <- NA
  }

  em$em_type <- ifelse(TmbData$ObsModel_ez[2] == 0,
    "Conventional delta", "Poisson-process link")
  em$nknots <- TmbData$n_x
  if ("unbiased" %in% names(sdreport)) {
    SD <- TMB::summary.sdreport(sdreport)
    index <- c(SD[grep("^Index_cyl", rownames(SD)), 3])
    ci <- data.frame(
     "par" = names(index),
     "index" = index,
     "sd" = c(SD[grep("^Index_cyl", rownames(SD)), 2]))
   ci$low <- ci$index - 1.96 * ci$sd
   ci$upp <- ci$index + 1.96 * ci$sd
   names(index) <- paste0("em_index_", seq_along(index))
  } else {
    # todo: look at these values because they are not the same
    # as what is generated above.
    index <- sdreport$value[
      grep("^Index", names(sdreport$value))]
    ci <- data.frame(
      "par" = names(sdreport$value),
      "index" = sdreport$value,
      "sd" = sdreport$sd)
    ci$low <- ci$index - 1.96 * ci$sd
    ci$upp <- ci$index + 1.96 * ci$sd
    ci <- ci[grep("^Index", ci$par), ]
    names(index) <- paste0("em_index_", seq_along(index))
  }
  logratio <- get_logratio(data = index)
  ci <- setNames(c(ci$low, ci$upp),
    paste(ci$par, 1:NROW(ci), c(rep("low", NROW(ci)), rep("upp", NROW(ci))),
    sep = "_"))
  em <- c(em, ci)
  lnindex <- log(index)
  linear <- lm(lnindex ~ 1 + seq_along(index))$coef
  names(linear) <- NULL
  em$em_linear <- linear[2]
  em$AIC <- AIC
  em$em_logratio <- logratio
  em$em_depth <- ifelse(all(em[grep("em_depth[[:digit:]]", names(em))] == 0),
    "FALSE", "TRUE")
  em$gradient <- ifelse(is.null(sdreport), NA, max(abs(sdreport$gradient.fixed)))
  em$hessian <- ifelse(is.null(sdreport), NA, sdreport$pdHess)
  em$rep <- gsub(
    paste0(".+OM\\", .Platform$file.sep, "([0-9]+).+"),
    "\\1", file)
  em$OM <- gsub(
    paste0(".+\\", .Platform$file.sep, "([0-9]+)OM.+"),
    "\\1", file)
  em$EM <- gsub(
    paste0(".+\\", .Platform$file.sep, "EM([0-9]+).+"),
    "\\1", file)
  return(data.frame(c(em, index)))
}

#' Create full path to estimation model
#'
#' @param simnum The simulation number
#' @param omnum The number for the operating model
#' @param repnum The replicate number
#' @param emnum The estimation method number
#' @param basedir A file path of your base directory
#'
#' @return A character value specifying the full or relative
#' path to the directory.
#'
#' @author Kelli Faye Johnson
#' @export
#'
#' @examples
#' get_EMpath(simnum = 1, omnum = 1, repnum = 1, emnum = 1)
get_EMpath <- function(simnum, omnum, repnum, emnum,
  basedir = getwd(),
  simname = "_VAST_simulation", check = FALSE, make = FALSE) {

  nums <- sapply(c(simnum, omnum, repnum, emnum),
    formatC, flag = "0", width = 2)

  path <- file.path(basedir, paste0(nums[1], simname),
    paste0(nums[2], "OM"), nums[3], paste0("EM", nums[4]))
  if (check) {
    if(!dir.exists(path)) warning(path, "\n does not exist")
  }
  if (make) dir.create(path, showWarnings = FALSE, recursive = TRUE)
  return(path)
}
