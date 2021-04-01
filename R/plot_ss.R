#' Plot Index From SS3 Table
#'
#' Plot a single time series of estimated index values for the
#' US West Coast using data in a \code{"Table_for_SS3.csv"} file.
#'
#' @param file.in A file path pointing to the \code{"Table_for_SS3.csv"} file
#' that has the results you wish to plot. The path can be relative or absolute.
#' This file is generated while running \code{\link{VAST_condition}()}.
#' @template savefile
#' @template lab.survey
#' @param lab.spp A character string that will be pasted after the survey name
#' for the main title for the figure. Typically, this is the species name.
#' Scientific names can be passed using
#' \code{lab.spp = expression(italic("xxx"))} or
#' \code{lab.spp = bquote(italic(.(xxx)))} if you want to evaluate the
#' object \code{xxx}.
#' @param do.smooth A logical value specifying whether or not a smoother should
#' be added for the first strata, which is typically the strata that includes
#' all areas.
#' @author Chantel Wetzel
#' @export
#' @return A 10 by 7 inch png is saved to the disk showing a time series of the
#' estimated index of abundance from a single model run. If there are more than
#' one strata, all strata are plotted, but the summary strata is always
#' plotted in black and will be the only line with confidence intervals.
#' Estimates are in metric tonnes, as labelled.
#' Years with estimates of zero are excluded from the figure.
#' A caption with a Latex label is also written to a csv file, where the first
#' column is the label and the second column is the caption.

plot_ss <- function(file.in = "Table_for_SS3.csv", savefile,
  lab.survey = "survey ", lab.spp = "",
  do.smooth = TRUE) {

  #### Get data
  file.in <- normalizePath(file.in)
  dat <- utils::read.csv(file.in, header = TRUE)
  dat <- dat[dat[, "Estimate_metric_tons"] != 0, ]
  if (lab.survey == "WCGBTS") lab.survey <- "NWFSC.Combo"
  stext <- nwfscSurvey::GetSurveyAbb.fn(lab.survey, na.return = lab.survey)
  strat <- unique(dat$Fleet)
  years <- unique(dat$Year)

  #### Make savefile name and directory
  if (missing(savefile)) {
    savefile <- file.path(dirname(file.in),
      paste0("VASTWestCoast_Index_", max(dat$Year), ".png"))
  }
  savefile <- normalizePath(savefile, mustWork = FALSE)
  dir.create(dirname(savefile), recursive = TRUE, showWarnings = FALSE)

  colors <- c("black",  "blue", "darkorchid1", "red")
  pch.vec <- c(21, 22, 23, 24)
  cex.vec <- c(1.6, 1.4, 1.4, 1.4)
  cex.lab  <- 2.1
  cex.axis <- 1.6
  colors <- c("black",  "blue", "darkorchid1", "red")
  if (length(strat) > 4) {
    stop("plot_ss is not designed to work with more than 4 strata.",
      "\nPlease contact the maintainer(s) to request additional",
      "\nfunctionality here or limit the number of strata in the csv.")
  }
  if ("north" %in% tolower(strat) && length(strat) == 3) {
    colors <- c("black", "blue", "red")
  }

  grDevices::png(filename = savefile,
    width = 10, height = 7, units = "in", res = 300, pointsize = 12)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  plot(0, type = "n",
    xlim = range(years),
    ylim = c(0, 1.5 * max(dat$Estimate_metric_tons)),
    xlab = "", ylab = "", yaxs = "i",
    main = "", cex.axis = cex.axis)
  graphics::mtext(side = 1 , "Year", cex = cex.lab, line = 3)
  graphics::mtext(side = 2, "Index (mt)", cex = cex.lab, line = 2.5)
  graphics::mtext(side = 3, text = lab.spp,
    font = 2, cex = cex.lab, line = 0.25)
  graphics::mtext(side = 3, text = stext,
    font = 2, cex = cex.lab, line = -1.75)

  for (b in seq_along(strat)) {
    ind <- dat$Fleet == strat[b]
    est <- dat[ind, "Estimate_metric_tons"]
    sd  <- dat[ind, "SD_log"]
    hi <- stats::qlnorm(0.975, meanlog = log(est), sdlog = sd)
    lo <- stats::qlnorm(0.025, meanlog = log(est), sdlog = sd)
    if (b == 1) {
      graphics::arrows(x0 = years, y0 = lo, x1 = years, y1 = hi, angle = 90, code = 3,
        length = 0.01, col = "grey")
      if (do.smooth == TRUE) {
        pred <- stats::loess(Estimate_metric_tons ~ Year, dat[ind,])
        graphics::lines(years, stats::predict(pred), lty = 2, col = "snow4")
      }
    }
    graphics::points(years, est, pch = pch.vec[b], bg = colors[b], cex = cex.vec[b])
    graphics::lines(years, est, col = colors[b], cex = 1)
  }

  goodstratname <- convert_strata4plot(strat)
  if (all(strat != 1)) {
    graphics::legend("topleft", legend = goodstratname, pt.bg = colors[1:b],
      pch = pch.vec[1:b], bty = "n", cex = cex.lab)
  }

  caption <- plot_caption(text = paste0(
      "Estimated index of relative abundance (mt) for ", stext, " data ",
      "(black points), with 5 and 95% confidence intervals (gray vertical lines)",
      ifelse(do.smooth, " and a loess smoother (dashed gray line).", "."),
      ifelse(length(strat) == 1, "",
        paste0(" Region-specific estimates are included for ",
          knitr::combine_words(goodstratname), " (colors) stratifications."))),
    figname = "index", survey = lab.survey,
    savefile = gsub(".png", ".csv", savefile))

}
