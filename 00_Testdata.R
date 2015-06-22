# Load packages

library(foreign)

setwd("~/R Workspace/ENVE")
MainWD <- "~/R Workspace/ENVE"

# Set data paths

# Stores Raw Data
dirRawData <- "~/R Workspace/ENVE/RawData"

# Stores Test Data
TestData <- "~/R Workspace/ENVE/ENVIPE"

# Directory for the R data
dirRdata <- "~/R Workspace/ENVE/Rdata"
# dir.create(dirRdata) # If it already exists, comment it out

# Output directories, for graphical output mainly
dirRoutput <- "~/R Workspace/ENVE/Routput"
# dir.create(dirRoutput) # If it already exists, comment it out

## 02_Loading files

setwd(dirRawData)

# ## Read ENVE 2012 Main Questionnaire data, Check if import as factor looses decimals
# enve12 <- read.dbf("FILENAME.dbf")
#
# ## Read ENVE 2012 Victimisation module
# vic12 <- read.dbf("FILENAME.dbf")
# 
# ## Read ENVE 2014 Main Questionnaire data
# enve14 <- read.dbf("FILENAME.dbf")
# 
# ## Read ENVE 2012 Victimisation module
# vic14 <- read.dbf("FILENAME.dbf")

## Read Catálogo entidades

cat_entidades <- read.csv("cat_entidades.csv", head=TRUE)

## Load Test data

setwd(TestData)

envipe <- read.dbf("TPer_Vic2.dbf")

envipe.hogar <- read.dbf("THogar.dbf")

envipe.dem <- read.dbf("TSDem.dbf")

envipe.viv <- read.dbf("TVivienda.dbf")


## Save objects for use latter
setwd(dirRdata)
save(list=ls(all=TRUE), file="TestData.Rdata")

setwd(MainWD)

