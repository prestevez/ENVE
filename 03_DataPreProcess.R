## 03_Pre-process the data. 
## Mostly change factors to numeric.

setwd(dirRdata)
load("ENVE1214.Rdata")

### Set factors as numeric

# Create list of variables per data frame that need to be transformed


### Set NAs

# Should be same list as previous

# mydata$v1[mydata$v1==99] <- NA, apply as needed?


### Set factor labels
# Create list of common factor relationships, levels labels pair

# Alevels <- c(1,2,3,9) # includes "no aplica"
# Alabels <- c("Sí", "No", "No aplica", "No sabe/no responde")

# Create list of variables to apply such factors

## Set logical variables, 9 is.na

### Set ordered labels

#### Create new indicator variables as needed

# Create list of common ordered relationships, levels labels pair


## Set variable labels Hmisc package











save(list=ls(all=TRUE), file="ENVE1214.Rdata")

setwd(MainWD)

### END of 03_DataPreProcess.R