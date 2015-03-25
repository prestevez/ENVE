## Set up

# Install and load packages from the internet

# install.packages("survey")
# install.packages("ggplot2")
# install.packages("xtable")
# install.packages("foreign")


# Load packages into library

library(survey)
library(foreign)
# library(xtable)
# library(ggplot2)

# Install and load packages from local directory

# dirPackages <- "Packages"
# 
# setwd(dirPackages)

# install.packages("survey", lib=dirPackages)
## install.packages("ggplot2", lib=dirPackages) # Should not be needed, it is in base install
# install.packages("xtable", lib=dirPackages)
# install.packages("foreign", lib=dirPackages)

# Load packages into library

# library(survey, lib.loc=dirPackages)
## library(foreign) # foreign package should not be needed, it is in base install
# library(xtable, lib.loc=dirPackages)
# library(ggplot2, lib.loc=dirPackages)
      
# setwd(MainWD)

# Set data paths

# Stores Raw Data
dirRawData <- "RawData"

# Directory for the R data
dirRdata <- "Rdata"
# dir.create(dirRdata) # If it already exists, comment it out

# Output directories, for graphical output mainly
dirRoutput <- "Routput"
# dir.create(dirRoutput) # If it already exists, comment it out

