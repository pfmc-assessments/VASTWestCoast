#' Get the Species and Survey Name
#'
#' Separate a single character string into three words to 
#' determine the survey and scientific name for the species
#' of interest.
#' 
#' @param input A character value that contains the survey of
#' interest and the scientific name of the species. The three
#' words will be split using the \code{split} argument.
#' @param split A character value specifying how to break up
#' the \code{input} argument. The default is to use \code{"_"}.
#' 
#' @return A vector with two entries, survey specifies the survey
#' name and species provides a normalized scientific name.
#' 
#' @author Kelli Faye Johnson
#' @importFrom nwfscSurvey createMatrix
#' @importFrom taxize gnr_resolve
#' @export
#' 
#' @examples
#' get_spp(input = "Triennial_Anoplopoma_fimbria")
#' 
get_spp <- function(input, split = "_") {
  if (length(input) > 1) stop("get_spp only accomadates 1 value.")
  splits <- strsplit(x = input, split = split)

  # Survey name
  availablesurveys <- c("EBSBTS", "WCGBTS", "WCGOP",
    nwfscSurvey::createMatrix()[, 1])
  survey <- toupper(sapply(splits, "[[", 1))
  finalsurvey <- switch(survey,
  	TRIENNIAL = "Triennial",
  	WCGBT = "WCGBTS",
  	NWFSC.COMBO = "WCGBTS",
  	survey)
  if (!finalsurvey %in% availablesurveys) {
    stop("The survey (specified as ", finalsurvey, 
    	") must be one of the following:\n",
      paste(availablesurveys, collapse = "\n"))
  }

  # Species name
  species <- gsub(split, " ",
  	gsub(paste0("^[A-Za-z]+", split), "", input))
  scitable <- taxize::gnr_resolve(names = species)
  scis <- scitable[
    scitable[, "score"] == max(scitable[, "score"]), 
    "submitted_name", drop = TRUE]
  finalspp <- unique(scis[!grepl("\\(", scis)])
  if (length(finalspp) > 1) stop("More than one species name was found",
  	" for ", species, ":\n", paste(finalspp, collapse = ","))
  
  return(c("survey" = finalsurvey, "species" = finalspp))
}
