
.onLoad <- function(libname, pkgname) {
  test <- function(pkg) {
    thetest <- tryCatch(find.package(pkg), 
      error = function(e) e)
    ifelse("error" %in% class(thetest), TRUE, FALSE)
  }
  if (getOption("repos")["CRAN"] == "@CRAN@") {
    options(repos = c("CRAN" = "http://cran.us.r-project.org"))
  }
  if (test("INLA")) {
    install.packages("INLA", 
      repos = c(getOption("repos"), 
        INLA = "https://inla.r-inla-download.org/R/stable"),
      dep = TRUE)
  }
}
