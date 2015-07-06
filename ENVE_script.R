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

# Load foreign package and load data

Package_install(foreign)

enve_all <- read.dbf("XXXXXX")
cat_entidades <- read.csv("cat_entidades.csv", head=TRUE)
homicidios <- read.csv("homicidios_tasas_2013_b.csv", header=TRUE)
homicidios <- merge(homicidios, cat_entidades, by="CVE_ENT")

# Prepare data for analysis

# Selecting oly the relevant variables

enve_test <- data.frame(extortions=integer(), bribes=integer(), CVE_ENT=integer(),
                          size=factor(), sector=factor(), susbector=factor(),
                          years=integer())

enve_test$extortions <- as.integer(as.character(enve_all$P26_10))

enve_test$bribes <- as.integer(as.character(enve_all$P33))

enve_test$CVE_ENT <- as.integer(enve_all$cve_ent)

enve_test$size <- enve_all$id_estrato

enve_test$sector <- enve_all$Sector_final

enve_test$subsector <- enve_all$P1_1B

enve_test$years <- 2013 - as.numeric(as.character(enve_all$P3))
# Still considering whether I should add one for 2013 businesses

enve_test <- merge(enve_test, homicidios, by="CVE_ENT", all.x=TRUE)
