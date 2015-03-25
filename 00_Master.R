## 00 Master script

# Register initial working directory

setwd("/Users/prestevez/R Workspace/ENVE")
MainWD <- getwd()


# Create object of file where Log will be written 

logfile <- file("log.txt")

# Instruct R to save console output to logfile

sink(logfile, append=TRUE, type=c("output","message"))

# Run the necessary scripts

source("01_Setup.R", echo=TRUE, max.deparse.length=10000)

source("02_LoadingData.R", echo=TRUE, max.deparse.length=10000)

# source("03_XX.R", echo=TRUE, max.deparse.length=10000)
# 
# source("04_XX.R", echo=TRUE, max.deparse.length=10000)

# End sink

sink()

