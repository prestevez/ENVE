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

# Selecting only the relevant variables

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

enve_test$extortions[is.na(enve_test$extortions)] <- 0
enve_test$bribes[is.na(enve_test$bribes)] <- 0

# EDA

# Distribution of extortion victimisations

ext_dist <- data.frame(table(enve_test$extortions))

colnames(ext_dist) <- c("Events", "Prevalence")

ext_dist$Events<- as.integer(as.character(ext_dist$Events))

ext_dist$Incidence <- ext_dist$Events * ext_dist$Prevalence

ext_dist$preval_per <- prop.table(ext_dist$Prevalence)*100

ext_dist$victim_per[2:length(ext_dist$Events)] <- prop.table(ext_dist[2:length(ext_dist$Events),2])*100

ext_dist$incid_per <- prop.table(ext_dist$Incidence)*100

ext_dist

save(ext_dist, file=paste(dir_name, "ext_dist.Rdata", sep=""))

# Testing for Poisson distribution

# analysing the mean variance relationship

mean_ext <- mean(enve_test$extortions)
var_ext <- var(enve_test$extortions)

mean_ext
var_ext

var_mean_ratio <- var_ext/mean_ext

var_mean_ratio

save(mean_ext, var_mean_ratio, var_ext, file=paste(dir_name, "var_mean.Rdata", sep=""))

# Create DF to fit obs v expected for counts

obsexp <- data.frame(Events=0:ext_dist[length(ext_dist$Events),1])

obsexp <- merge(obsexp, ext_dist, by="Events", all.x=TRUE)

obsexp <- obsexp[,c("Events", "Prevalence")]

colnames(obsexp) <- c("Events", "Obs")

obsexp$Obs[is.na(obsexp$Obs)] <- 0

obsexp

# Generate Poisson expected frequencies

obsexp$exp_po <- dpois(0:(length(obsexp$Events)-1), lambda=mean_ext) * length(enve_test$extortions)

## Function to print table with obs and exp and chi-sq test

obs_exp_test <- function(dataframe, exp, par)
  {
  cs<-factor(0:(length(dataframe[,1])-1))
  index <- max(which(exp >= 4))
  levels(cs)[index:length(dataframe[,1]] <- paste(as.character(index-1), "+", sep="")
  ef<-as.vector(tapply(exp,cs,sum))
  of<-as.vector(tapply(dataframe[,2],cs,sum))
  ofef_table <- data.frame(Events=0:(index-1), Obs=of, Exp=ef)
  chisq_t <- sum((of-ef)^2/ef)
  df <- length(of)-par-1
  pval <- 1-pchisq(chisq_t, df)
  return(list(Table=ofef_table, Chisq=chisq_t, DF=df, PValue=pval))
  }

## Convenience log function

clog <- function(x) log(x + 1)

## Same as above but on log scale

obs_exp_test_log <- function(dataframe, exp, par)
  {
  cs<-factor(0:(length(dataframe[,1])-1))
  index <- max(which(exp >= 4))
  levels(cs)[index:length(dataframe[,1])] <- paste(as.character(index-1), "+", sep="")
  ef<-as.vector(tapply(exp,cs,sum))
  of<-as.vector(tapply(dataframe[,2],cs,sum))
  ofef_table <- data.frame(Events=0:(index-1), log.Obs=clog(of), log.Exp=clog(ef))
  chisq_t <- sum((clog(of)-clog(ef))^2/clog(ef))
  df <- length(of)-par-1
  pval <- 1-pchisq(chisq_t, df)
  return(list(Table=ofef_table, Chisq=chisq_t, DF=df, PValue=pval))
  }

# Poisson test

po_chisq <- obs_exp_test(obsexp, obsexp$exp_po, 1)

po_chisq

po_chisq_log <- obs_exp_test_log(obsexp, obsexp$exp_po, 1)

po_chisq_log

save(po_chisq_log, po_chisq file=paste(dir_name, "po_chisq.Rdata", sep=""))

# Testing for Negative Binomial distribution

# loading MASS package

Package_install(MASS)

# Obtaining the parameters for negbin

nb_estimates <- fitdistr(enve_test$extortions)

nb_estimates

# Generating the neg bin expected frequencies

obsexp$exp_nb <- dnbinom(0:(length(obsexp$Events)-1), size=nb_estimates$estimate[1], mu=nb_estimates$estimate[2]) *
                          length(b_test$extortions)

# NB Tests

nb_chisq <- obs_exp_test(obsexp, obsexp$exp_nb, 2)

nb_chisq

nb_chisq_log <- obs_exp_test_log(obsexp, obsexp$exp_nb, 2)

nb_chisq_log

save(nb_estimates, nb_chisq_log, nb_chisq, file=paste(dir_name, "nb_chisq.Rdata", sep=""))

save(obsexp, file=paste(dir_name, "obsexp.Rdata"))
