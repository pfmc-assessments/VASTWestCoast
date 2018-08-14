
re_all <- function(data, bind = TRUE, type = c("re", "ae")) {
# todo: move all these functions into individual files and document them.
  type <- match.arg(type)
  re <- function(text = "index_1", data) {
    omd <- as.numeric(data[, paste0("om_", text)])
    emd <- as.numeric(data[, paste0("em_", text)])

    return((emd - omd) / omd)
  }
  if (type == "ae") {
    re <- function(text = "index_1", data) {
      omd <- as.numeric(data[, paste0("om_", text)])
      emd <- as.numeric(data[, paste0("em_", text)])
      return(abs(emd - omd))
    }
  }
  names <- gsub("om_", "", grep("om_", colnames(data), value = TRUE))
  names <- names[!(names %in% c("type"))]
  names <- names[!(names %in% c("logratio"))]
  names <- names[!grepl("depth$|linear|logratio", names)]
  res <- mapply(re, names, MoreArgs = list(data = data))
  colnames(res) <- paste0(type, "_", colnames(res))
  res <- data.frame(res,
    "re_logratiodiff" = data$em_logratio - data$om_logratio)

  indexnames <- grep("index", names, value = TRUE)
  om <- data[, grepl("om_index_", colnames(data))]
  em <- data[, grepl("em_index_", colnames(data))]
  om <- apply(om, 2, as.numeric)
  em <- apply(em, 2, as.numeric)
  om <- t(apply(om, 1, function(x) log(x) - mean(log(x))))
  em <- t(apply(em, 1, function(x) log(x) - mean(log(x))))
  logre <- em - om
  colnames(logre) <- gsub("em", "lncentdiff", colnames(logre))

  res <- cbind(res, logre)
  res$rmse_index <- apply(logre, 1, function(x) sqrt(mean(x^2)))

  if (bind) {
    return(cbind(data, res))
  } else {return(res)}

}

ggplotre <- function(data, x, y, print = TRUE, gradient = FALSE,
  facetx = c("emname", "em_depth"), facety = "omname",
  labels = NULL, type = "box", scales = c("fixed", "free"),
  dir = NULL, lim = list(x = NULL, y = NULL)) {

  VAST_simulation_depth_theme <- theme_bw() + theme(
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1))

  scales <- match.arg(scales)
  facetx <- facetx[!facetx %in% x]

  data$em_depth <- paste("depth in EM =", data$em_depth)

  data$em_type <- gsub("process ", "", data$em_type)
  data$om_type <- gsub("process ", "", data$om_type)
  data$em_type <- gsub("Conventional delta", "Delta-model", data$em_type)
  data$om_type <- gsub("Conventional delta", "Delta-model", data$om_type)
  data$omname <- paste("OM =", data$om_type)
  data$emname <- paste("EM =", data$em_type)

  if (x != "em_depth") data[, x] <- as.numeric(data[, x])
  data[, y] <- as.numeric(data[, y])

  gg <- ggplot(data)
  if (gradient) gg <- ggplot(data,
    aes(col = as.numeric(gradient)))

  if (type == "points"){
    gg <- gg +
      geom_point(aes_string(x = x, y = y),
      alpha = 0.5, size = 2.5) +
      scale_shape_manual(name = "", values = c(15, 19), guide = FALSE) +
      scale_color_gradient(name = "gradient")
    if (grepl("^re", y)) {
      gg <- gg + geom_vline(xintercept = 0, lty = 2, col = "red")
    }
  }
  if (type == "box") {
    gg <- gg +
      geom_boxplot(aes_string(x = x, y = y))
  }
  if (facetx[1] == "" & facety[1] == "") {
    facet <- "."
  } else {
    gg <- gg + facet_grid(as.formula(paste(paste(facety, collapse = "+"), "~",
      paste(facetx, collapse = "+"))), scales = scales)
  }

  gg <- gg + VAST_simulation_depth_theme
  if (grepl("^re", y)) {
    gg <- gg + geom_hline(yintercept = 0, lty = 2, col = "red")
  }


  if(!is.null(labels[1])) gg <- gg +
    xlab(labels[1]) + ylab(labels[2])
  if (!is.null(lim$x)) gg <- gg + xlim(lim$x)
  if (!is.null(lim$y)) gg <- gg + ylim(lim$y)

  if (print) print(gg)
  if (!is.null(dir)) {
    jpeg(
      filename = file.path(dir,
        paste0("VAST_simulation_depth_", x, "VS", y, ".jpeg")),
      res = 600, units = "in", width = 8, height = 8)
    print(gg)
    dev.off()
  }
  invisible(gg)
}

getrdata <- function(file){
  ne <- new.env()
  load(file, env = ne)

  out <- data.frame(
    "par" = names(
      get("parameter_estimates", env = ne)$SD[c("par.fixed")][[1]]),
    "val" = get("parameter_estimates", env = ne)$SD[c("par.fixed")],
    "se" = sqrt(diag(
      get("parameter_estimates", env = ne)$SD[c("cov.fixed")][[1]])))
  colnames(out)[2] <- "val"
  report <- data.frame(
    "par" = names(get("parameter_estimates", env = ne)$SD$value),
    "val" = get("parameter_estimates", env = ne)$SD$value,
    "se" = sqrt(diag(get("parameter_estimates", env = ne)$SD$cov)))
out$par <- make.unique(as.character(out$par))
report$par <- make.unique(as.character(report$par))

  # ne2 <- new.env()
  # load(file.path(dirname(file), "Save.RData"), env = ne2)
  # depth <- data.frame(
  #   "par" = c("gamma1_ctp", "gamma2_ctp"),
  #   "val" = sapply(get("Save",
  #     env = ne2)$ParHat[c("gamma1_ctp", "gamma2_ctp")], mean),
  #   "se" = NA)
  # report <- rbind(report, depth)

  out <- rbind(out, report)
  out$om_name <- basename(dirname(file))

  out[grep("L_", out$par), "val"] <- abs(out[grep("L_", out$par), "val"])
  return(out)
}

#' Calculate the median absolute error over categories and add to a plot
#'
#' @param data The data used in the \code{\link{ggplot2}} plot
#' @param y A character vector of length 2 that specifies the
#' columns to summarize
#' @param x A character value providing the right side of the
#' formula used for aggregation (e.g., \code{"depth + year"})
#' @param gg The \code{\link{ggplot2}} you want to add the MAE to
#' @param nsmall An integer value providing the number of digits you
#' want after the decimal for the MAE
#'
addMAE <- function(data, y, x, gg = NULL, nsmall = 3) {
  if (grepl("em_depth", x)) {
    data$em_depth <- paste("depth in EM =", data$em_depth)
  }
  agg1 <- aggregate(as.formula(paste(y[1], "~", x)),
    data = data,
    function(xx) format(median(xx, na.rm = TRUE), nsmall = nsmall,
      digits = 1))
  agg2 <- aggregate(as.formula(paste(y[2], "~", x)),
    data = data,
    function(xx) format(median(xx, na.rm = TRUE), nsmall = nsmall,
      digits = 1))
  aggs <- merge(agg1, agg2)
  if (is.null(gg)) return(aggs)

  gg <- gg + geom_text(data = aggs, x = Inf, y = -Inf,
  vjust = -1.5, hjust = 1.5, aes_string(label = y[1])) +
  geom_text(data = aggs, x = -Inf, y = Inf,
    vjust = 2.5, hjust = -0.5, aes_string(label = y[2]))
  return(gg)
}

scaleFUN <- function(x) sprintf("%.0f", x)

getcompare <- function(file) {
  load(file)
  aic <- AIC
  temp <- gsub("VAST_simulation_depth_", "",
    basename(dirname(dirname(file))))
  temp <- strsplit(temp, "_")[[1]]
  nx <- temp[4]
  species <- paste(temp[2:3], collapse = " ")
  area <- ifelse(grepl("EBSBTS", temp[1]), "EBS", "WC")
  type <- ifelse(grepl("nodepth", file), "FALSE", "TRUE")
  depth <- EmSave$Sdreport$par.fixed[
    grep("gamma", names(EmSave$Sdreport$par.fixed))]
  if (length(depth) == 0) depth <- rep(0, 4)
  if (length(depth) == 2) depth <- c(depth[1], 0, depth[2], 0)
  returnme <- c(area, species, type, nx,  unlist(aic), unlist(depth))
return(returnme)
}

