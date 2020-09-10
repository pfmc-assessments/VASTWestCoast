#' Plot Index Time Series
#'
#' Plot a single time series of estimated index values for the
#' US West Coast.
#'
#' @param file.in A file path pointing to the \code{"Table_for_SS3.csv"} file
#' that has the results you wish to plot. The path can be relative or absolute.
#' This file is generated from \code{\link{VAST_condition}}.
#' @param file.out A file path for plotting the results and saving them to the
#' disk. This can be a relative or absolute path. If no extension is added, then
#' \code{.png} will be added to the file name because the resulting figure is
#' a png. If missing, a default file name will be generated.
#' @param name A character string that will be pasted after the survey name to
#' for the main title for the figure.
#' Typically, this is the name of the species, which is also the folder
#' in which the results exist, though not always.
#' @author Chantel Wetzel
#' @export
#' @return A 10 by 7 inch png is saved to the disk showing a time series of the
#' estimated index of abundance from a single model run. If there are more than
#' one strata, than all strata are plotted, but the summary strata is always
#' plotted in black and will be the only line with confidence intervals.
#' The survey name and text provided in the name argument are pasted together
#' to form the main title of the plot. Estimates are in metric tonnes and the
#' ylab reflects this. Years with estimates of zero are excluded from the figure.
#' A caption with a Latex label is also written to a csv file, where the first
#' column is the label and the second column is the caption.

plot_ss <- function(file.in = "Table_for_SS3.csv", file.out,
  name = "", do_smooth = TRUE) {

  file.in <- normalizePath(file.in)
  dat <- utils::read.csv(file.in, header = TRUE)
  dat <- dat[dat[, "Estimate_metric_tons"] != 0, ]

  if (missing(file.out)) {
    file.out <- file.path(dirname(file.in),
      paste0("VASTWestCoast_Index_", max(dat$Year), ".png"))
  }
  file.out <- normalizePath(file.out, mustWork = FALSE)
  if (!grepl("\\.png$", file.out)){
    file.out <- paste0(file.out, ".png")
  }
  dir.create(dirname(file.out), recursive = TRUE, showWarnings = FALSE)
  nwfscSurveyname <- basename(dirname(file.out))
  if (nwfscSurveyname == "WCGBTS") nwfscSurveyname <- "NWFSC.Combo"
  surveyname <- GetSurveyAbb.fn(nwfscSurveyname)
  surveyname <- ifelse(is.na(surveyname), "", surveyname)

  colors <- c("black",  "blue", "darkorchid1", "red")
  pch.vec <- c(21, 22, 23, 24)
  cex.vec <- c(1.6, 1.4, 1.4, 1.4)
  cex.lab  <- 2.1
  cex.axis <- 1.6

  colors <- c("black",  "blue", "darkorchid1", "red")
  strat <- unique(dat$Fleet)
  years <- unique(dat$Year)
  if (length(strat) > 4) {
    stop("plot_ss is not designed to work with more than 4 strata.",
      "\nPlease contact the maintainer(s) to request additional",
      "\nfunctionality here or limit the number of strata in the csv.")
  }
  if ("north" %in% strat) {
    colors = c("black", "red", "blue")
  }

  grDevices::png(filename = file.out,
    width = 10, height = 7, units = "in", res = 300, pointsize = 12)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  plot(0, type = "n",
    xlim = range(years),
    ylim = c(0, 1.5 * max(dat$Estimate_metric_tons)),
    xlab = "", ylab = "", yaxs = "i",
    main = "", cex.axis = cex.axis)
  graphics::mtext(side = 1 , "Year", cex = cex.lab, line = 3)
  graphics::mtext(side = 2, "Index (mt)", cex = cex.lab, line = 2.5)
  graphics::mtext(side = 3, paste(surveyname, name),
    font = 2, cex = cex.lab, line = 1)

  for (b in seq_along(strat)) {
    ind <- dat$Fleet == strat[b]
    est <- dat[ind, "Estimate_metric_tons"]
    sd  <- dat[ind, "SD_log"]
    hi <- stats::qlnorm(0.975, meanlog = log(est), sdlog = sd)
    lo <- stats::qlnorm(0.025, meanlog = log(est), sdlog = sd)
    if (b == 1) {
      graphics::arrows(x0 = years, y0 = lo, x1 = years, y1 = hi, angle = 90, code = 3,
        length = 0.01, col = "grey")
      if (do_smooth == TRUE) {
        pred <- stats::loess(Estimate_metric_tons ~ Year, dat[ind,])
        graphics::lines(years, stats::predict(pred), lty = 2, col = "snow4")
      }
    }
    graphics::points(years, est, pch = pch.vec[b], bg = colors[b], cex = cex.vec[b])
    graphics::lines(years, est, col = colors[b], cex = 1)
  }
  if (all(strat != 1)) {
    goodstratname <- gsub("^([a-z]{2})$", "\\U\\1", strat, perl = TRUE)
    goodstratname <- gsub("All_areas", "all areas", goodstratname)
    goodstratname <- gsub("coast", "coast wide", goodstratname)
    graphics::legend("topleft", legend = goodstratname, pt.bg = colors[1:b],
      pch = pch.vec[1:b], bty= "n", cex = cex.lab)
  }

  caption <- data.frame(
    "label" = paste0("fig_index_", nwfscSurveyname),
    "caption" = paste0(
      "Estimated index of relative abundance (mt) for ",
      surveyname,
      " (black points), with 5 and 95% confidence intervals (grey vertical lines)",
      ifelse(do_smooth, " and a loess smoother (dashed grey line).", "."),
      ifelse(length(strat) > 1, 
        paste0("Region-specific estimates are included for ",
          knitr::combine_words(strat), " (colors)."),
        "")
    )
  )
  utils::write.table(x = caption,
    file = gsub(".png", ".csv", file.out), sep = ",", row.names = FALSE)

}
