#' Check Rtools Is Installed Properly
#'
#' Check that Rtools is properly installed on your machine such that
#' packages and code within packages can be compiled.
#'
#' @details
#' Warning - this function was built by Windows users and may not
#' be sufficient for other operating systems.
#'
#' A new version of Rtools is required for R releases > 4.0.0
#' https://cran.r-project.org/bin/windows/Rtools/
#'
#' @author Chantel R. Wetzel, modified by Kelli Faye Johnson
#' @export
#' @returns Various summaries are printed to the screen to help you troubleshoot
#' your installation of Rtools. If you make it to the end of this function
#' successfully, then a value of 0 will be returned and you should be
#' able to then run \code{\link{check_TMB}()} to see if TMB is properly
#' installed for your system.
#' @seealso \code{\link{check_TMB}()} to see if TMB is properly installed
#' and has access to rtools.
#' @examples
#' # Hope for a 0
#' check_rtools()
check_rtools <- function() {
  if (version$major < 4) {
    stop("VASTWestCoast does not support R versions less than 4.0.")
  }
  if (version$major >= 4 & !grepl("rtools40", Sys.getenv("PATH"))) {
    message("Rtools is not available on your machine for R version 4.x .\n",
      "Please install rtools40.")
    switch(.Platform$OS.type,
      windows = utils::browseURL("https://cran.r-project.org/bin/windows/Rtools"),
      utils::browseURL("https://cran.r-project.org/bin")
    )
    return(NULL)
  }

  if (!pkgbuild::find_rtools()) {
    stop("rtools40 is not properly installed. Please contact IT.\n",
      "It should be in a folder on your c drive, e.g., 'C:\\rtools40'")
  }

  # Rtools40 has multiple bin folders, and it is unclear to KFJ which folder
  # needs to be in the PATH so IT is instructed to put all three of the following
  pathsplit <- strsplit(Sys.getenv("PATH"), ";")[[1]]
  if (!any(grepl("rtools40$", pathsplit))) {
    warning("c:\\\\rtools40 was not found in your path, see path below:")
    utils::capture.output(pathsplit, type = "message")
  }
  if (!any(grepl("rtools40.+usr.+bin$", pathsplit))) {
    warning("c:\\\\rtools40\\\\usr\\\\bin was not found in your path,",
      " see path below:")
    utils::capture.output(pathsplit, type = "message")
  }
  if (!any(grepl("rtools40.+mingw64.+bin$", pathsplit))) {
    warning("c:\\\\rtools40\\\\mingw64\\\\bin was not found in your path,",
      " see path below:")
    utils::capture.output(pathsplit, type = "message")
  }

  # Double check that you have access to a make file
  if (Sys.which("make") == "") {
    stop("The make file was not found, and it should be here\n",
      "rtools40\\usr\\bin\\make.exe\n",
      "\nTry the following code to see if you get non VAST stuff to compile\n",
      "install.packages('jsonlite', type = 'source')")
  }

  return(0)
}