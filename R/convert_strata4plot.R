#' Convert Strata Names for Plot Text
#'
#' Convert the strata names in the column \code{STRATA} to names that
#' are formatted for a figure or for text. Things such as converting
#' underscores to spaces and two-letter lower-case words to two-letter
#' upper-case words are performed for you.
#'
#' @param text A vector of text strings that will be formatted.
#'
#' @author Kelli Faye Johnson
#' @return A vector of text strings the same length as \code{text} that
#' have been formatted to use in a plot or in text.
#'
convert_strata4plot <- function(text) {
  # Check if the text is just numeric values
  if (any(text != 1)) {
    check <- suppressWarnings(as.numeric(text))
    if (!any(is.na(check))) return(text)
  }

  goodstratname <- text
  # Change two letter words to upper case assuming they are state abb.
  goodstratname <- gsub("^([a-z]{2})$", "\\U\\1", goodstratname, perl = TRUE)
  goodstratname <- gsub("^([a-z]{2})_", "\\U\\1 - ", goodstratname, perl = TRUE)
  goodstratname <- gsub("- ([a-z]{2})_", "- \\U\\1 - ", goodstratname, perl = TRUE)
  goodstratname <- gsub("(- [a-z]{2})$", "\\U\\1", goodstratname, perl = TRUE)
  goodstratname <- gsub("All_areas", "all areas", goodstratname)
  goodstratname <- gsub("^coast$", "coast wide", goodstratname)
  goodstratname <- gsub("north_south", "coast wide", goodstratname)
  # Separate remaining words with space instead of under score
  goodstratname <- gsub("_", " ", goodstratname)

  return(goodstratname)
}
