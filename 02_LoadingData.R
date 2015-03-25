## Loading files

setwd(dirRawData)

# ## Read ENVE 2012 Main Questionnaire data
# enve12 <- read.dbf("FILENAME.dbf", as.is=TRUE)
#
# ## Read ENVE 2012 Victimisation module
# vic12 <- read.dbf("FILENAME.dbf", as.is=TRUE)
# 
# ## Read ENVE 2014 Main Questionnaire data
# enve14 <- read.dbf("FILENAME.dbf", as.is=TRUE)
# 
# ## Read ENVE 2012 Victimisation module
# vic14 <- read.dbf("FILENAME.dbf", as.is=TRUE)

## Save objects for use latter
setwd(dirRdata)
save(list=ls(all=TRUE), file="ENVE1214.Rdata")

setwd(MainWD)

### END of 02_LoadingData.R