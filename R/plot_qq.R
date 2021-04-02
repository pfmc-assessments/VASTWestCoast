#' Plot quantiles versus theoretical quantiles
#'
#' Plot quantiles versus theoretical quantiles as was originally done in [VAST]
#' prior to their use of DHARMa residual diagnostics.
#' Functionality is limited to lognormal and gamma distributions for a single
#' species or group.
#' The 1:1 line is automatically added to the figure to aid in performing the
#' fat-pen test to determine if your distributional assumption fits your data.
#'
#' @param outdir The directory where you would like to save the resulting figure,
#' which will be named `VASTWestCoast_QQ.png`.
#' @param model The list of model specifications and results, typically available
#' in the `Save.RData` file and produced by [VAST_do].
#'
#' @seealso Users will not typically interact with this function, but will instead
#' benefit from it being called in [VAST_diagnostics].
#' @author Kelli Faye Johnson
#' @export

plot_qq <- function(outdir, model) {
  stopifnot("data_list" %in% names(model))
  stopifnot(length(model[["data_list"]][["n_c"]]) == 1)

  n_e <- model[["data_list"]][["n_c"]]
  e_i <- model[["data_list"]][["n_e"]]
  sigmaM <- (rep(1, n_e) %o% model[["Report"]][["SigmaM"]])[1, 1, 1]
  Which <- which(model[["data_list"]][["b_i"]] > 0)

  if (model[["data_list"]][["ObsModel_ez"]][1] == 1) {
    Qvals <- na.omit(mapply(plnorm,
          q = model[["data_list"]][["b_i"]][Which],
          meanlog = log(model[["data_list"]][["a_i"]][Which] * exp(model[["Report"]][["P2_iz"]][Which])) -
                    (sigmaM^2) / 2,
          MoreArgs = list(sdlog = sigmaM)))
  }
  if (model[["data_list"]][["ObsModel_ez"]][1] == 2) {
    Qvals <- na.omit(mapply(pgamma,
          q = model[["data_list"]][["b_i"]][Which],
          scale = (sigmaM^2) *
            (model[["data_list"]][["a_i"]][Which] *
            exp(model[["Report"]][["P2_iz"]][Which])
            ),
          MoreArgs = list(shape = 1 / (sigmaM^2))))
  }

  grDevices::png(file.path(outdir, "VASTWestCoast_QQ.png"), height = 18, width = 18, units = "cm",
    res = 300)
  on.exit(grDevices::dev.off(), add = TRUE)
  Qorder <- order(Qvals)
  plot(
    x = seq(0, 1, length = length(Qorder)),
    y = Qvals[Qorder], type = "p",
    xlab = "Uniform", ylab = "Empirical", pch = 1,
    )
  graphics::abline(0, 1)

  return(invisible())
}