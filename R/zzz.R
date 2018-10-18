
.onAttach <- function(libname, pkgname) {
  if (!"devtools" %in% utils::installed.packages()[, 1]){
    # packageStartupMessage("Installing package: ")
    utils::install.packages("devtools", 
      repos = getOption("repos"), dependencies = TRUE)
  }
  if (!"JRWToolBox" %in% utils::installed.packages()[, 1]){
   devtools::install_github("John-R-Wallace/JRWToolBox")
  }
  if (!"VAST" %in% utils::installed.packages()[, 1]){
   devtools::install_github("James-Thorson/VAST")
  }
}
