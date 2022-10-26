#' Show list of greek letters and their meaning
#'
#' Show a list of greek letters and their meaning within the VAST modeling
#' framework because it is often hard to remember what part of the model
#' each letter represents.
#'
#' @param type The model type that you want to get the symbols for.
#' See the function call for the available types where the first one
#' listed is the default using [match.arg].
#' @param plot A logical value specifying if a plot should be generated.
#' The default is to NOT plot the results.
#'
#' @author Kelli F. Johnson
#' @export
#' @return A data frame with two columns, the greek symbol in words and
#' the description of what it means in the modeling framework.
#'
show_greek <- function(type = c("VAST"), plot = FALSE) {
  type <- match.arg(type, several.ok = FALSE)

  if (type == "VAST") {
    out <- data.frame(
      greek = c(
        "omega",
        "epsilon",
        "eta",
        "gamma",
        "lambda"
      ),
      description = c(
        "random spatial variation",
        "random spatio-temporal variation",
        "random tow- and/or vessel-level catchability variation",
        "density covariates",
        "catchability covariates"
      )
    )
  }

  if (plot) {
    yy <- seq(.6, 1.3, length.out = NROW(out))
    plot(
      x = seq(NROW(out)),
      y = yy,
      type = "n",
      xaxt = "n", yaxt = "n",
      xlab = "", ylab = ""
    )
    mtext(side = 1, line = 0.2, "VAST symbols and their definitions")
   text(
     x = 1.1,
     y = seq(.6, 1.3, length.out = NROW(out)),
     labels = mapply(eval, parse(text = paste0("expression(", out[["greek"]], ")"))),
     cex = 2
   )
   text(
     x = 1.3,
     y = seq(.6, 1.3, length.out = NROW(out)),
     labels = paste0("(", out[["greek"]], ") = ", out[["description"]]),
     cex = 1, pos = 4
   )
  }

  return(out)
}