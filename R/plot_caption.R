#' Make and Save a Figure Caption
#'
#' Take input text and generate a standardized csv file
#' with a caption for a given figure.
#'
#' @param text A text string of any length pasted together into a single string.
#' @template survey
#' @param figname A character value that will be pasted together along with the
#' survey name to generate the label used in the Latex file or markdown file.
#' For example entries of \code{figname = "mesh"} and \code{survey = "Triennial"}
#' would lead to a label of \code{"fig_mesh_Triennial"}.
#' @template savefile
#' @return The resulting data frame with the label and the caption as column
#' entries with a single row for the given figure. The data frame is also
#' saved to the disk using the file name in \code{savefile}.
#'
plot_caption <- function(text, survey, figname, savefile) {

  caption <- data.frame(
    label = paste("fig", figname, survey, sep = "_"),
    caption = text
  )
  utils::write.table(
    x = caption,
    file = savefile,
    sep = ",",
    row.names = FALSE)

  return(caption)
}
