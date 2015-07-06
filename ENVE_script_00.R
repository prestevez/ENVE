## 00 Master script

# Create directories where results and log will be written.
if (dir.exists("Output/") == FALSE)
{
  dir.create("Output/")
}

# Create directories to save results outputs
dir_name <- paste("Output/",Sys.time(), "_results/", sep="")

dir.create(dir_name)

# Create logfile object

logfile_name <- paste(Sys.time(), "log.txt", sep="_")

logfile <- file(paste("Output/", logfile_name, sep=""))

# Initiate sink to store console output to log file

sink(logfile, append=TRUE, type=c("output", "messsage"))

# Run the scripts

source("ENVE_script", echo=TRUE, max.deparse.length=10000)

# End sink

sink(type=c("output", "messsage"))

## End of ENVE_script_00.R
