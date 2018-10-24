
.onAttach <- function(libname, pkgname) {
  test <- function(pkg) {
    thetest <- tryCatch(find.package(pkg), 
      error = function(e) e)
    ifelse("error" %in% class(thetest), TRUE, FALSE)
  }
  if (test("devtools")){
    # packageStartupMessage("Installing package: ")
    utils::install.packages("devtools", 
      repos = getOption("repos"), dependencies = TRUE)
  }
  if (test("JRWToolBox")){
   devtools::install_github("John-R-Wallace/JRWToolBox")
  }
  if (test("VAST")){
   devtools::install_github("James-Thorson/VAST")
  }
}
