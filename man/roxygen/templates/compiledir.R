#' @param compiledir A file path, either relative or full, to a directory where
#' the VAST cpp file will be compiled leading to three files saved to the disk.
#' Thus, users should have both read and write capabilities within this
#' directory. A single file path can be used for multiple models allowing for
#' the model to be only compiled once rather than multiple times.
#' This file path is tranfered to the \code{CompileDir} within
#' \code{\link[VAST]{make_model}()}.
