## ENVE_script.R

# Package checker function

Package_install <- function(package)
{
  if (as.character(package) %in% library()$results[,"Package"])
  {
  library(package)
  }
  else
  {
    install.packages(as.character(package), repos="http://cran.itam.mx/")
  }
}
