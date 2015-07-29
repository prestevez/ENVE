## 00 Master script

# Load the necessary packages
library(foreign)
library(ggplot2)
library(Cairo)
library(xtable)
library(texreg)
library(lmtest)
library(MASS)
library(lme4)
library(glmmADMB)


## Create directories where results and log will be written.
#if (dir.exists("Output/") == FALSE)
#{
#  dir.create("Output/")
#}

# Create directories to save results outputs
dir_name <- paste("Output/",as.integer(Sys.time()), "_results/", sep="")

dir.create(dir_name)

# Create logfile object

logfile_name <- paste(as.integer(Sys.time()), "_log.txt", sep="")

logfile <- file(paste(dir_name, logfile_name, sep=""))

# Initiate sink to store console output to log file

sink(logfile, append=TRUE, type=c("output","message"))

# Run the scripts

sessionInfo()

ll <- parse(file = "ENVE_script.R")

source("ENVE_script.R", echo=TRUE, max.deparse.length=10000)


## Run with trycatch

#ll <- parse(file = "ENVE_script.R")

#for (i in seq_along(ll))
#{
#  tryCatch(eval(ll[[i]]), error = function(e) message("Error!", as.character(e)))
#}

# End sink

sink()

## End of ENVE_script_00.R
