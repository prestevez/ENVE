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
#library(glmmADMB)
library(evaluate)


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

# Set seed for reproducibility

set.seed(42)

sessionInfo()

#source("ENVE_script.R", echo=TRUE, max.deparse.length=10000)

results <- evaluate(file("ENVE_script.R"), new_device=FALSE)

replay(results)

# End sink

sink()

## End of ENVE_script_00.R
