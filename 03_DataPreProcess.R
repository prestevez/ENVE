## 03_Pre-process the data. 
## Mostly change factors to numeric.

setwd(dirRdata)
load("ENVE1214.Rdata")















save(list=ls(all=TRUE), file="ENVE1214.Rdata")

setwd(MainWD)

### END of 03_DataPreProcess.R