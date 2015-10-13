## ENVE_script.R

# Load glmmADMB
library(glmmADMB)

# Load data

enve_all <- read.dbf("TR_ENVE_CUES_2014.dbf")
cat_entidades <- read.csv("cat_entidades.csv", head=TRUE)
homicidios <- read.csv("homicidios_tasas_2013_b.csv", header=TRUE)
homicidios <- merge(homicidios, cat_entidades, by="CVE_ENT", all.x=TRUE)
scode <- read.csv("secode.csv", head=TRUE)
scode$Code <- scode$Code*10000


# Prepare data for analysis

# Selecting only the relevant variables

enve_test <- data.frame(extortions=as.integer(as.character(enve_all$P26_10)))

enve_test$bribes <- as.integer(as.character(enve_all$P33))

enve_test$CVE_ENT <- as.integer(as.character(enve_all$CVE_ENT))

enve_test$size <- enve_all$ID_ESTRATO
levels(enve_test$size) <- c("Large", "Medium", "Small", "Micro")

enve_test$sector <- enve_all$SECTOR_FIN

enve_test$tempsub <- as.integer(as.character(enve_all$P1_1B))
enve_test$subsector <- cut(enve_test$tempsub, scode$Code, right=FALSE)
levels(enve_test$subsector) <- scode$Sector

enve_test$years <- 2013 - as.numeric(as.character(enve_all$P3))

## Recode years into categorical (as a new variable)

enve_test <- merge(enve_test, homicidios, by="CVE_ENT", all.x=TRUE)

enve_test$extortions[is.na(enve_test$extortions)] <- 0
enve_test$bribes[is.na(enve_test$bribes)] <- 0

head(enve_test, 50)

tail(enve_test, 50)

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

### Package_install(xtable)

xext_dist <- xtable(ext_dist, digits=c(0,0,0,0,3,3,3), caption="The distribution of extortion victimisations",
                      label="T_dist")

print(xext_dist, include.rownames=FALSE)

save(ext_dist, xext_dist, file=paste(dir_name, "ext_dist.Rdata", sep=""))

# Testing for Poisson distribution

# analysing the mean variance relationship

mean_ext <- mean(enve_test$extortions)
var_ext <- var(enve_test$extortions)

mean_ext
var_ext

var_mean_ratio <- var_ext/mean_ext

var_mean_ratio

vmr_df <- data.frame(Mean=mean_ext, Variance=var_ext, Ratio=var_mean_ratio)

vmr_df

xvmr_df <- xtable(vmr_df, digits=4, caption="If the variance-mean ratio is larger than one, there is over-disperison",
                    label="T_vmr")

print(xvmr_df, include.rownames=FALSE)

save(mean_ext, var_mean_ratio, var_ext, vmr_df, xvmr_df, file=paste(dir_name, "var_mean.Rdata", sep=""))

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
  levels(cs)[index:length(dataframe[,1])] <- paste(as.character(index-1), "+", sep="")
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

xpo_chisq <- xtable(po_chisq[[1]], digits=c(0,0,0,3), caption="Observed vs. Expected (Poisson)", label="T_po_chisq")

print(xpo_chisq, include.rownames=FALSE)

po_chisq_log <- obs_exp_test_log(obsexp, obsexp$exp_po, 1)

po_chisq_log

xpo_chisq_log <- xtable(po_chisq_log[[1]], digits=c(0,0,0,3), caption="Observed vs. Expected (Poisson, log-scale)",
                          label="T_po_chisq_log")

print(xpo_chisq_log, include.rownames=FALSE)

save(po_chisq_log, po_chisq, xpo_chisq, xpo_chisq_log, file=paste(dir_name, "po_chisq.Rdata", sep=""))

# Testing for Negative Binomial distribution

# loading MASS package

### Package_install(MASS)

# Obtaining the parameters for negbin

nb_estimates <- fitdistr(enve_test$extortions, "Negative Binomial")

nb_estimates

# Generating the neg bin expected frequencies

obsexp$exp_nb <- dnbinom(0:(length(obsexp$Events)-1), size=nb_estimates$estimate[1], mu=nb_estimates$estimate[2]) *
                          length(enve_test$extortions)

# NB Tests

nb_chisq <- obs_exp_test(obsexp, obsexp$exp_nb, 2)

nb_chisq

xnb_chisq <- xtable(nb_chisq[[1]], digits=c(0,0,0,3), caption="Observed vs. Expected (Negatve Binomial)", label="T_nb_chisq")

print(xnb_chisq, include.rownames=FALSE)

nb_chisq_log <- obs_exp_test_log(obsexp, obsexp$exp_nb, 2)

nb_chisq_log

xnb_chisq_log <- xtable(nb_chisq_log[[1]], digits=c(0,0,0,3), caption="Observed vs. Expected (Negative Binomial, log-scale)",
                          label="T_nb_chisq_log")

print(xnb_chisq_log, include.rownames=FALSE)

save(nb_estimates, nb_chisq_log, nb_chisq, xnb_chisq, xnb_chisq_log, file=paste(dir_name, "nb_chisq.Rdata", sep=""))

xobsexp <- xtable(obsexp, digits=c(0,0,0,3,3),
                  caption="Observed and expected frequencies under Poisson and Negative Binomial distributions",
                  label="T_obsexp")

print(xobsexp, include.rownames=FALSE)

save(obsexp, xobsexp, file=paste(dir_name, "obsexp.Rdata", sep=""))

## Plots of this

# Plot the observed distribution

### Package_install(ggplot2)
### Package_install(Cairo)

plot.obs <- ggplot(obsexp, aes(x=Events, y=Obs)) + geom_bar(stat="identity") + ylab("Frequency")

plot.log.obs <- ggplot(obsexp, aes(x=Events, y=clog(Obs))) + geom_bar(stat="identity") + ylab("log(Frequency + 1)")

plot.exp_po <- ggplot(obsexp, aes(x=Events, y=exp_po)) + geom_bar(stat="identity") + ylab("Frequency")

plot.log.exp_po <- ggplot(obsexp, aes(x=Events, y=clog(exp_po))) + geom_bar(stat="identity") + ylab("log(Frequency + 1)")

plot.exp_nb <- ggplot(obsexp, aes(x=Events, y=exp_nb)) + geom_bar(stat="identity") + ylab("Frequency")

plot.log.exp_nb <- ggplot(obsexp, aes(x=Events, y=clog(exp_nb))) + geom_bar(stat="identity") + ylab("log(Frequency + 1)")

# Save ggplot objects

dist.plots <- list(plot.obs=plot.obs, plot.log.obs=plot.log.obs, plot.exp_po=plot.exp_po,
  plot.log.exp_po=plot.log.exp_po, plot.exp_nb=plot.exp_nb, plot.log.exp_nb=plot.log.exp_nb)

save(dist.plots, file=paste(dir_name, "plots_dist_ext.Rdata", sep=""))

# Save ggplots as images

for (i in 1:length(dist.plots))
{
  ggfile <- paste(dir_name, names(dist.plots[i]), ".pdf", sep="")
  ggsave(dist.plots[[i]], file=ggfile, width=5, height=4)
}

for (i in 1:length(dist.plots))
{
  ggfile <- paste(dir_name, names(dist.plots[i]), ".png", sep="")
  ggsave(dist.plots[[i]], file=ggfile, width=5, height=4, type="cairo-png")
}

# Independent Variables
summ_table <- data.frame(variable=0, N=0, prevalence=0, incidence=0, mean=0, sd=0, min=0, max=0)

ind <- 1
for (i in 1:length(enve_test))
  {
  if (colnames(enve_test)[i] %in% c("extortions", "bribes"))
    {
    summ_table[ind,1] <- colnames(enve_test)[i]
    summ_table[ind,2] <- length(enve_test[,i])
    summ_table[ind,3] <- length(enve_test[enve_test[,i] != 0,i])
    summ_table[ind,4] <- sum(enve_test[,i])
    summ_table[ind,5] <- mean(enve_test[,i])
    summ_table[ind,6] <- sd(enve_test[,i])
    summ_table[ind,7] <- min(enve_test[,i])
    summ_table[ind,8] <- max(enve_test[,i])
    }

  else if (colnames(enve_test)[i] == "years")
    {
    summ_table[ind,1] <- colnames(enve_test)[i]
    summ_table[ind,2] <- length(enve_test[,i])
    summ_table[ind,5] <- mean(enve_test[,i])
    summ_table[ind,6] <- sd(enve_test[,i])
    summ_table[ind,7] <- min(enve_test[,i])
    summ_table[ind,8] <- max(enve_test[,i])
    }

  else if (colnames(enve_test)[i] %in% c("sector", "size", "subsector"))
    {
    for (a in 1:length(levels(enve_test[,i])))
      {
      summ_table[ind,1] <- levels(enve_test[,i])[a]
      summ_table[ind,2] <- length(enve_test[enve_test[,i] == levels(enve_test[,i])[a],i])
      summ_table[ind,5] <- summ_table[ind,2]/length(enve_test[,1])
      ind <- ind + 1
      }
    }
  ind <- ind + 1
  }


summ_table[length(summ_table[,1])+1,] <- c(NA, length(homicidios[,"tasahom"]), NA, NA,
                                           mean(homicidios[,"tasahom"]), sd(homicidios[,"tasahom"]),
                                           min(homicidios[,"tasahom"]), max(homicidios[,"tasahom"]))

summ_table[length(summ_table[,1]),1] <- "state murder rt"

summ_table <- summ_table[!is.na(summ_table[,1]),]

summ_table

xsumm_table <- xtable(summ_table, digits=c(0,0,0,0,0,3,3,0,0), caption="Descriptive Statistics", label="T_summ")

print(xsumm_table, include.rownames=FALSE)

save(summ_table, xsumm_table, file=paste(dir_name, "summ_table.Rdata", sep=""))

# Bribes variable
bribes_tab <- data.frame(table(enve_test$bribes))
bribes <- data.frame(Events=bribes_tab$Var1, Freq=bribes_tab$Freq)
colnames(bribes)[1] <- "Events"
bribes$Events <- as.integer(as.character(bribes$Events))
obs_b <- data.frame(Events=0:max(bribes$Events))
bribes <- merge(bribes, obs_b, by="Events", all.y=TRUE)
bribes[is.na(bribes[,2]),2] <- 0

bribes

xbribes <- xtable(format(bribes), caption="Distribution of corruption victimisations (bribery)",
                    label="T_bribes")

print(xbribes, include.rownames=FALSE)

save(bribes, xbribes, file=paste(dir_name, "bribes.Rdata", sep=""))

# Plot the distribution of bribes

plot.bribes <- ggplot(bribes, aes(x=Events, y=Freq)) + geom_bar(stat="identity") + ylab("Frequency")

plot.log_bribes <- ggplot(bribes, aes(x=Events, y=clog(Freq))) + geom_bar(stat="identity") + ylab("log(Frequency + 1)")

plot.ext_bribes <- ggplot(enve_test, aes(x=bribes, y=extortions)) + geom_jitter() + geom_smooth(method="lm") +
                  scale_x_continuous(breaks=0:max(enve_test$bribes)) + ylab("Extortion events") + xlab("Bribes")

save(plot.bribes, plot.log_bribes, plot.ext_bribes, file=paste(dir_name, "plots_bribes.Rdata", sep=""))

ggsave(plot.bribes, file=paste(dir_name, "plot_bribes.pdf", sep=""), width=5, height=4)
ggsave(plot.log_bribes, file=paste(dir_name, "plot_log_bribes.pdf", sep=""), width=5, height=4)
ggsave(plot.ext_bribes, file=paste(dir_name, "plot_ext_bribes.pdf", sep=""), width=5, height=4)

ggsave(plot.bribes, file=paste(dir_name, "plot_bribes.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.log_bribes, file=paste(dir_name, "plot_log_bribes.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.ext_bribes, file=paste(dir_name, "plot_ext_bribes.png", sep=""), width=5, height=4, type="cairo-png")

# Examine the relationship between bribes and extortions

temp_ext <- as.factor(enve_test$extortions)
levels(temp_ext)[6:length(levels(temp_ext))] <- "5+"
temp_bribes <- as.factor(enve_test$bribes)
levels(temp_bribes)[6:length(levels(temp_bribes))] <- "5+"

ext_bribes <- ftable(temp_bribes, temp_ext)

ext_bribes

xext_bribes <- xtable(format(ext_bribes), caption="The distribution of extortions and bribery victimisations",
                        labels="T_ext_bribes")

print(xext_bribes, include.rownames=FALSE)

chisq.ext_bribes <- chisq.test(ext_bribes)

chisq.ext_bribes

cor.ext_bribes <- with(enve_test, cor.test(bribes, extortions, method="pearson"))

cor.ext_bribes

save(ext_bribes, xext_bribes, chisq.ext_bribes, cor.ext_bribes, file=paste(dir_name, "ext_bribes.Rdata", sep=""))

## Years variable

temp_years <- cut(enve_test$years, c(0,2,11,31,51,Inf), right=FALSE)

ext_years <- ftable(temp_years, temp_ext)

ext_years

xext_years <- xtable(format(ext_years), caption="The distribution of extortion victimisations per year ranges",
                        lab="t_ext_years")

print(xext_years, include.rownames=FALSE)

chisq.ext_years <- chisq.test(ext_years)
chisq.ext_years

cor.ext_years <- with(enve_test, cor.test(extortions, years, method="pearson"))
cor.ext_years

save(ext_years, xext_years, chisq.ext_years, cor.ext_years, file=paste(dir_name, "ext_years.Rdata", sep=""))

# Plots of ext_years relationship

ey_df <- data.frame(ext_years)

plot.ey <- ggplot(ey_df, aes(x=temp_ext, y=Freq, fill=temp_years)) +
                    geom_bar(stat="identity") +
                    facet_grid(temp_years~., scale="free") +
                    ylab("Frequency") + xlab("Events")

plot.log_ey <- ggplot(ey_df, aes(x=temp_ext, y=clog(Freq), fill=temp_years)) +
                    geom_bar(stat="identity") +
                    facet_grid(temp_years~., scale="free") +
                    ylab("log(Frequency + 1)") + xlab("Events")

plot.ext_years <- ggplot(enve_test, aes(x=years, y=extortions)) + geom_jitter() + geom_smooth(method="lm") +
                          xlab("years") + ylab("Extortions")

# Save ggplot objects
save(plot.ey, plot.log_ey, plot.ext_years, file=paste(dir_name, "plots_ey.Rdata", sep=""))

# Save ggplots as images
ggsave(plot.ey, file=paste(dir_name, "plot_ey.pdf", sep=""), width=5, height=4)
ggsave(plot.log_ey, file=paste(dir_name, "plot_log_ey.pdf", sep=""), width=5, height=4)
ggsave(plot.ext_years, file=paste(dir_name, "plot_ext_years.pdf", sep=""), width=5, height=4)

ggsave(plot.ey, file=paste(dir_name, "plot_ey.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.log_ey, file=paste(dir_name, "plot_log_ey.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.ext_years, file=paste(dir_name, "plot_ext_years.png", sep=""), width=5, height=4, type="cairo-png")

## Sector

ext_sector <- ftable(enve_test$sector, temp_ext)

ext_sector

xext_sector <- xtable(format(ext_sector), caption="Exortion victimisations by sector", label="T_ext_sector")

print(xext_sector, include.rownames=FALSE)

chisq.ext_sector <- chisq.test(ext_sector)

chisq.ext_sector

save(ext_sector, xext_sector, chisq.ext_sector, file=paste(dir_name, "ext_sector.Rdata", sep=""))

# Plots by sector

es_df <- data.frame(ext_sector)

plot.es <- ggplot(es_df, aes(x=temp_ext, y=Freq, fill=Var1)) +
                  geom_bar(stat="identity") +
                  facet_grid(Var1~., scale="free") +
                  ylab("Frequency") + xlab("Events")

plot.log_es <- ggplot(es_df, aes(x=temp_ext, y=clog(Freq), fill=Var1)) +
                  geom_bar(stat="identity") +
                  facet_grid(Var1~., scale="free") +
                  ylab("log(Frequency + 1)") + xlab("Events")

# save ggplot objects
save(plot.es, plot.log_es, file=paste(dir_name, "plots_ext_sector.Rdata", sep=""))

ggsave(plot.es, file=paste(dir_name, "plot_es.pdf", sep=""), width=5, height=4)
ggsave(plot.log_es, file=paste(dir_name, "plot_log_es.pdf", sep=""), width=5, height=4)

ggsave(plot.es, file=paste(dir_name, "plot_es.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.log_es, file=paste(dir_name, "plot_log_es.png", sep=""), width=5, height=4, type="cairo-png")

## Subsector

sec_subsec <- with(enve_test, ftable(subsector, sector))

sec_subsec

xsec_subsec <- xtable(format(sec_subsec), caption="Subsectors within sectors", label="T_sec_subsec")

print(xsec_subsec, include.rownames=FALSE)

ext_subsector <- ftable(enve_test$subsector, temp_ext)

ext_subsector

xext_subsector <- xtable(format(ext_subsector), caption="Exortion victimisations by subsector", label="T_ext_subsector")

print(xext_subsector, include.rownames=FALSE)

chisq.ext_subsector <- chisq.test(ext_subsector)

chisq.ext_subsector

save(ext_subsector, xext_subsector, chisq.ext_subsector, sec_subsec, file=paste(dir_name, "ext_subsector.Rdata", sep=""))

# Plots by subsector

ess_df <- data.frame(ext_subsector)

plot.ess <- ggplot(ess_df, aes(x=temp_ext, y=Freq, fill=Var1)) +
                  geom_bar(stat="identity") +
                  facet_grid(Var1~., scale="free") +
                  ylab("Frequency") + xlab("Events")

plot.log_ess <- ggplot(ess_df, aes(x=temp_ext, y=clog(Freq), fill=Var1)) +
                  geom_bar(stat="identity") +
                  facet_grid(Var1~., scale="free") +
                  ylab("log(Frequency + 1)") + xlab("Events")

# save ggplot objects
save(plot.ess, plot.log_ess, file=paste(dir_name, "plots_ext_subsector.Rdata", sep=""))

ggsave(plot.ess, file=paste(dir_name, "plot_ess.pdf", sep=""), width=5, height=4)
ggsave(plot.log_ess, file=paste(dir_name, "plot_log_ess.pdf", sep=""), width=5, height=4)

ggsave(plot.ess, file=paste(dir_name, "plot_ess.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.log_ess, file=paste(dir_name, "plot_log_ess.png", sep=""), width=5, height=4, type="cairo-png")

## Size

ext_size <- ftable(enve_test$size, temp_ext)

ext_size

xext_size <- xtable(format(ext_size), caption="Exortion victimisations by size", label="T_ext_size")

print(xext_size, include.rownames=FALSE)

chisq.ext_size <- chisq.test(ext_size)

chisq.ext_size

save(ext_size, xext_size, chisq.ext_size, file=paste(dir_name, "ext_size.Rdata", sep=""))

# Plots by size

ez_df <- data.frame(ext_size)

plot.ez <- ggplot(ez_df, aes(x=temp_ext, y=Freq, fill=Var1)) +
                  geom_bar(stat="identity") +
                  facet_grid(Var1~., scale="free") +
                  ylab("Frequency") + xlab("Events")

plot.log_ez <- ggplot(ez_df, aes(x=temp_ext, y=clog(Freq), fill=Var1)) +
                  geom_bar(stat="identity") +
                  facet_grid(Var1~., scale="free") +
                  ylab("log(Frequency + 1)") + xlab("Events")

# save ggplot objects
save(plot.ez, plot.log_ez, file=paste(dir_name, "plots_ext_size.Rdata", sep=""))

ggsave(plot.ez, file=paste(dir_name, "plot_ez.pdf", sep=""), width=5, height=4)
ggsave(plot.log_ez, file=paste(dir_name, "plot_log_ez.pdf", sep=""), width=5, height=4)

ggsave(plot.ez, file=paste(dir_name, "plot_ez.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.log_ez, file=paste(dir_name, "plot_log_ez.png", sep=""), width=5, height=4, type="cairo-png")

## Area level influences
state_ext_dist <- ftable(enve_test$NOM_ENT, temp_ext)

state_ext_dist

xstate_ext_dist <- xtable(format(state_ext_dist), caption="Distribution of extortions by state.", label="T_s_ext_dist")

print(xstate_ext_dist, include.rownames=FALSE)

state_ext <- with(enve_test, table(CVE_ENT, extortions))

state_hom <- homicidios[,c(1,4,5,6)]

state_inc <- t(t(state_ext)*as.integer(colnames(state_ext)))

state_summ1 <- data.frame(data.frame(margin.table(state_ext[,2:ncol(state_ext)],1))/
                            data.frame(margin.table(state_ext[,1:ncol(state_ext)],1))*1000)

state_summ1 <- cbind(state_summ1, data.frame(margin.table(state_inc,1))[,2]/
                       data.frame(margin.table(state_ext[,1:ncol(state_ext)],1))*1000)

state_summ1 <- cbind(state_summ1, data.frame(margin.table(state_ext,1))[,2])

colnames(state_summ1) <- c("Prevalence", "Incidence", "N")

state_summ1 <- cbind(state_summ1,
                     Concentration=state_summ1$Incidence/state_summ1$Prevalence)

state_summ1 <- cbind(state_summ1, CVE_ENT= as.integer(rownames(state_summ1)))

state_summ1 <- merge(state_summ1, state_hom, by="CVE_ENT")

state_summ1 <- state_summ1[,c(1,7,8,4,2,3,5,6)]

state_summ1

xstate_summ1 <- xtable(state_summ1[,c(2,4:8)], caption="Key indicators of extortion victimisation by state",
                          labels="T_state_summ")

print(xstate_summ1, include.rownames=FALSE)

save(state_ext_dist, xstate_ext_dist, state_summ1, xstate_summ1, file=paste(dir_name, "states.Rdata", sep=""))

# Area prevalence and concentration

plot.inc_preval <- ggplot(state_summ1, aes(x=Prevalence, y=Incidence)) + geom_point() + geom_smooth(method="lm")

cor.inc_preval <- with(state_summ1, cor.test(Incidence, Prevalence))

cor.inc_preval

plot.con_preval <- ggplot(state_summ1, aes(x=Prevalence, y=Concentration)) + geom_point() + geom_smooth(method="lm")

cor.con_preval <- with(state_summ1, cor.test(Concentration, Prevalence))

cor.con_preval

# save these last objects
save(plot.inc_preval, cor.inc_preval, plot.con_preval, cor.con_preval, file=paste(dir_name, "preval_con.Rdata", sep=""))

# save these last ggplots as images
ggsave(plot.inc_preval, file=paste(dir_name, "plot_inc_preval.pdf", sep=""), width=5, height=4)
ggsave(plot.con_preval, file=paste(dir_name, "plot_con_preval.pdf", sep=""), width=5, height=4)

ggsave(plot.inc_preval, file=paste(dir_name, "plot_inc_preval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.con_preval, file=paste(dir_name, "plot_con_preval.png", sep=""), width=5, height=4, type="cairo-png")

# Area-level influences: murder rate

plot.mur_inc <- ggplot(state_summ1, aes(x=tasahom, y=Incidence)) +
                          geom_point() + geom_smooth(method="lm")  + xlab("Murder rate")

cor.mur_inc <- with(state_summ1, cor.test(Incidence, tasahom))

cor.mur_inc

plot.mur_preval <- ggplot(state_summ1, aes(x=tasahom, y=Prevalence)) +
                          geom_point() + geom_smooth(method="lm") + xlab("Murder rate")

cor.mur_preval <- with(state_summ1, cor.test(Prevalence, tasahom))

cor.mur_preval

plot.mur_con <- ggplot(state_summ1, aes(x=tasahom, y=Concentration)) +
                            geom_point() + geom_smooth(method="lm") + xlab("Murder rate")

cor.mur_con <- with(state_summ1, cor.test(Concentration, tasahom))

cor.mur_con

# Save murder rate related objects

save(plot.mur_inc, cor.mur_inc, plot.mur_preval, cor.mur_preval, plot.mur_con, cor.mur_con,
        file=paste(dir_name, "mur_ext.Rdata", sep=""))

# Save the ggplot objects as images
ggsave(plot.mur_inc, file=paste(dir_name, "plot_mur_inc.pdf", sep=""), width=5, height=4)
ggsave(plot.mur_preval, file=paste(dir_name, "plot_mur_preval.pdf", sep=""), width=5, height=4)
ggsave(plot.mur_con, file=paste(dir_name, "plot_mur_con.pdf", sep=""), width=5, height=4)

ggsave(plot.mur_inc, file=paste(dir_name, "plot_mur_inc.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.mur_preval, file=paste(dir_name, "plot_mur_preval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.mur_con, file=paste(dir_name, "plot_mur_con.png", sep=""), width=5, height=4, type="cairo-png")

## Modelling
# Make sure we are using the correct lme4 version

packageVersion("lme4")

# First round, all models using sector, from poisson to different types of me-NB

# 1. Poisson GLM
m1 <- glm(extortions ~ bribes + tasahom + years + sector + size, data=enve_test,
          family="poisson")

summary(m1)

xm1 <- xtable(m1, caption="Poisson GLM", label="T_m1")

print(xm1)

# 2. NB GLM

m2 <- glm.nb(extortions ~ bribes + tasahom + years + sector + size, data=enve_test)

summary(m2)

xm2 <- xtable(m2, caption="Negative Binomial GLM", label="T_m2")

print(xm2)

m2.1 <- glm.nb(extortions ~ bribes + tasahom + years + sector, data=enve_test)

summary(m2.1)

xm2.1 <- xtable(m2.1, caption="Negative Binomial GLM", label="T_m2.1")

print(xm2.1)

m2.2 <- glm.nb(extortions ~ bribes + tasahom + years, data=enve_test)

summary(m2.2)

xm2.2 <- xtable(m2.2, caption="Negative Binomial GLM", label="T_m2.2")

print(xm2.2)

m2.3 <- glm.nb(extortions ~ bribes + tasahom, data=enve_test)

summary(m2.3)

xm2.3 <- xtable(m2.3, caption="Negative Binomial GLM", label="T_m2.3")

print(xm2.3)

m2.4 <- glm.nb(extortions ~ bribes, data=enve_test)

summary(m2.4)

xm2.4 <- xtable(m2.4, caption="Negative Binomial GLM", label="T_m2.4")

print(xm2.4)

m2.null <- glm.nb(extortions ~ 1, data=enve_test)

summary(m2.null)

xm2.null <- xtable(m2.null, caption="Negative Binomial GLM", label="T_m2.null")

print(xm2.null)

## Compare different glm.nb

anova.m2 <- anova(m2, test="Chisq")

anova.m2

xam2 <- xtable(anova.m2, caption="Analysis of deviance of Negative Binomial Model", label="T_am2")

print(xam2)

tx.m2.x <- texreg(list(m2.null, m2.4, m2.3, m2.2, m2.1, m2), caption="Comparison of all NB models",
                  label="T_tx_m2x", booktabs=TRUE)

tx.m2.x

anova.m2.x <- anova(m2.null, m2.4, m2.3, m2.2, m2.1, m2, test="Chisq")

anova.m2.x

xam2.x <- xtable(anova.m2.x, caption="ANOVA test between variables of the NB models", label="T_xam2x")

# Compare NB and Poisson
tx.m1_m2 <- texreg(list(m1, m2), caption="Comparison of Poisson and NB GLMs", label="T_m1m2", booktabs=TRUE)

tx.m1_m2

lr.m1_m2 <- lrtest(m1, m2)

lr.m1_m2

xlr.m1_m2 <- xtable(lr.m1_m2, caption="Likelihood ratio test between Poisson and NB GLMs", label="T_lrm1m2")

print(xlr.m1_m2)

save(m1, m2, m2.1, m2.2, m2.3, m2.4, m2.null, anova.m2, xam2, tx.m2.x, anova.m2.x, xam2.x,
  xm1, xm2, tx.m1_m2, lr.m1_m2, xlr.m1_m2, file=paste(dir_name, "glms.Rdata", sep=""))

# 3. Poisson GLMM

m3 <- glmer(extortions ~ bribes +  tasahom + years + sector + size + (1 | NOM_ABR), data=enve_test, family="poisson")

summary(m3)

# 4. Negative Binomial GLMM

m4.0 <- glmer.nb(extortions ~ bribes + tasahom + years + sector + size + (1 | NOM_ABR), data=enve_test)

summary(m4.0)

anova.m4.0 <- anova(m4.0, test="Chisq")

anova.m4.0

xam4.0 <- xtable(anova.m4.0, caption="Analysis of deviance of Negative Binomial Mixed Model", label="T_am4.0")

print(xam4.0)

# Comparison between Poisson and NB GLMMs

tx.m3_m4 <- texreg(list(m3, m4.0), caption="Comparison of Poisson and NB Mixed Models", label="T_m3m4", booktabs=TRUE)

tx.m3_m4

lr.m3_m4 <- lrtest(m3, m4.0)

lr.m3_m4

xlr.m3_m4 <- xtable(lr.m3_m4, caption="Likelihood ratio test between Poisson and NB Mixed Models", label="T_lrm3m4")

print(xlr.m3_m4)

# 4.x Investigating different variables in the NB GLMMs

m4.1 <- glmer.nb(extortions ~ bribes + tasahom + years + sector +
                    (1 | NOM_ABR), data=enve_test)

m4.2 <- glmer.nb(extortions ~ bribes + tasahom + years +
                   (1 | NOM_ABR), data=enve_test)

m4.3 <- glmer.nb(extortions ~ bribes + tasahom +
                   (1 | NOM_ABR), data=enve_test)

m4.4 <- glmer.nb(extortions ~ bribes +
                   (1 | NOM_ABR), data=enve_test)

m4.null <- glmer.nb(extortions ~ 1 +
                   (1 | NOM_ABR), data=enve_test)

# Compare all NB GLMMs
tx.m4.x <- texreg(list(m4.0, m4.1, m4.2, m4.3, m4.4, m4.null), caption="Comparison of all NB mixed models",
                  label="T_tx_m4x", booktabs=TRUE)

tx.m4.x

anova.m4.x <- anova(m4.null, m4.4, m4.3, m4.2, m4.1, m4.0, test="Chisq")

anova.m4.x

xam4.x <- xtable(anova.m4.x, caption="ANOVA test between variables of the NB mixed models", label="T_xam4x")

## Using glmmADMB
# 4. Negative Binomial GLMM_ADMB

ADm4.0 <- glmmadmb(extortions ~ bribes + tasahom + years + sector + size + (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 40000")

summary(ADm4.0)

anova.ADm4.0 <- anova(ADm4.0, test="Chisq")

anova.ADm4.0

xaADm4.0 <- xtable(anova.ADm4.0, caption="Analysis of deviance of Negative Binomial Mixed Model", label="T_aADm4.0")

print(xaADm4.0)

# Comparison between Poisson and NB GLMMs

tx.m3_ADm4 <- texreg(list(m3, ADm4.0), caption="Comparison of Poisson and NB Mixed Models", label="T_m3ADm4", booktabs=TRUE)

tx.m3_ADm4

lr.m3_ADm4 <- lrtest(m3, ADm4.0)

lr.m3_ADm4

xlr.m3_ADm4 <- xtable(lr.m3_ADm4, caption="Likelihood ratio test between Poisson and NB Mixed Models", label="T_lrm3ADm4")

print(xlr.m3_ADm4)

# 4.x Investigating different variables in the NB GLMMs_ADMB

ADm4.1 <- glmmadmb(extortions ~ bribes + tasahom + years + sector +
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 40000")

ADm4.2 <- glmmadmb(extortions ~ bribes + tasahom + years +
                   (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 40000")

ADm4.3 <- glmmadmb(extortions ~ bribes + tasahom +
                   (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 40000")

ADm4.4 <- glmmadmb(extortions ~ bribes +
                   (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 40000")

ADm4.null <- glmmadmb(extortions ~ 1 +
                   (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 40000")

# Compare all NB GLMMs_ADMB
tx.ADm4.x <- texreg(list(ADm4.0, ADm4.1, ADm4.2, ADm4.3, ADm4.4, ADm4.null), caption="Comparison of all NB mixed models",
                  label="T_tx_ADm4x", booktabs=TRUE)

tx.ADm4.x

anova.ADm4.x <- anova(ADm4.null, ADm4.4, ADm4.3, ADm4.2, ADm4.1, ADm4.0, test="Chisq")

anova.ADm4.x

xaADm4.x <- xtable(anova.ADm4.x, caption="ANOVA test between variables of the NB mixed models", label="T_xaADm4x")


save(m3, m4.0, anova.m4.0, xam4.0, tx.m3_m4, lr.m3_m4, xlr.m3_m4,
        m4.1, m4.2, m4.3, m4.4, m4.null, tx.m4.x, anova.m4.x, xam4.x,
        ADm4.0, anova.ADm4.0, xaADm4.0, tx.m3_ADm4, lr.m3_ADm4, xlr.m3_ADm4,
        ADm4.1, ADm4.2, ADm4.3, ADm4.4, ADm4.null, tx.ADm4.x, anova.ADm4.x, xaADm4.x,
        file=paste(dir_name, "mixed_models1.Rdata", sep=""))

# Second round using subsector

# 1. Poisson GLM
n1 <- glm(extortions ~ bribes + tasahom + years + subsector + size, data=enve_test,
          family="poisson")

summary(n1)

xn1 <- xtable(n1, caption="Poisson GLM", label="T_n1")

print(xn1)

# 2. NB GLM

n2 <- glm.nb(extortions ~ bribes + tasahom + years + subsector + size, data=enve_test)

summary(n2)

xn2 <- xtable(n2, caption="Negative Binomial GLM", label="T_n2")

print(xn2)

n2.1 <- glm.nb(extortions ~ bribes + tasahom + years +subsector, data=enve_test)

summary(n2.1)

xn2.1 <- xtable(n2.1, caption="Negative Binomial GLM", label="T_n2.1")

print(xn2.1)

n2.2 <- glm.nb(extortions ~ bribes + tasahom + years, data=enve_test)

summary(n2.2)

xn2.2 <- xtable(n2.2, caption="Negative Binomial GLM", label="T_n2.2")

print(xn2.2)

n2.3 <- glm.nb(extortions ~ bribes + tasahom, data=enve_test)

summary(n2.3)

xn2.3 <- xtable(n2.3, caption="Negative Binomial GLM", label="T_n2.3")

print(xn2.3)

n2.4 <- glm.nb(extortions ~ bribes, data=enve_test)

summary(n2.4)

xn2.4 <- xtable(n2.4, caption="Negative Binomial GLM", label="T_n2.4")

print(xn2.4)

n2.null <- glm.nb(extortions ~ 1, data=enve_test)

summary(n2.null)

xn2.null <- xtable(n2.null, caption="Negative Binomial GLM", label="T_n2.null")

print(xn2.null)

## Compare different glm.nb

anova.n2 <- anova(n2, test="Chisq")

anova.n2

xan2 <- xtable(anova.n2, caption="Analysis of deviance of Negative Binomial Model", label="T_an2")

print(xan2)

tx.n2.x <- texreg(list(n2.null, n2.4, n2.3, n2.2, n2.1, n2), caption="Comparison of all NB models",
                  label="T_tx_n2x", booktabs=TRUE)

tx.n2.x

anova.n2.x <- anova(n2.null, n2.4, n2.3, n2.2, n2.1, n2, test="Chisq")

anova.n2.x

xan2.x <- xtable(anova.n2.x, caption="ANOVA test between variables of the NB models", label="T_xan2x")

# Compare NB and Poisson
tx.n1_n2 <- texreg(list(n1, n2), caption="Comparison of Poisson and NB GLMs", label="T_n1n2", booktabs=TRUE)

tx.n1_n2

lr.n1_n2 <- lrtest(n1, n2)

lr.n1_n2

xlr.n1_n2 <- xtable(lr.n1_n2, caption="Likelihood ratio test between Poisson and NB GLMs", label="T_lrn1n2")

print(xlr.n1_n2)

save(n1, n2, n2.1, n2.2, n2.3, n2.4, n2.null, anova.n2, xan2, tx.n2.x, anova.n2.x, xan2.x,
   xn1, xn2, tx.n1_n2, lr.n1_n2, xlr.n1_n2, file=paste(dir_name, "glms_nver.Rdata", sep=""))

# 3. Poisson GLMM

n3 <- glmer(extortions ~ bribes +  tasahom + years + subsector + size + (1 | NOM_ABR), data=enve_test, family="poisson")

summary(n3)

# 4. Negative Binomial GLMM

n4.0 <- glmer.nb(extortions ~ bribes + tasahom + years + subsector + size + (1 | NOM_ABR), data=enve_test)

summary(n4.0)

anova.n4.0 <- anova(n4.0, test="Chisq")

anova.n4.0

xan4.0 <- xtable(anova.n4.0, caption="Analysis of deviance of Negative Binomial Mixed Model", label="T_an4.0")

print(xan4.0)

# Comparison between Poisson and NB GLMMs

tx.n3_n4 <- texreg(list(n3, n4.0), caption="Comparison of Poisson and NB Mixed Models", label="T_n3n4", booktabs=TRUE)

tx.n3_n4

lr.n3_n4 <- lrtest(n3, n4.0)

lr.n3_n4

xlr.n3_n4 <- xtable(lr.n3_n4, caption="Likelihood ratio test between Poisson and NB Mixed Models", label="T_lrn3n4")

print(xlr.n3_n4)

# 4.x Investigating different variables in the NB GLMMs

n4.1 <- glmer.nb(extortions ~ bribes + tasahom + years + subsector +
                    (1 | NOM_ABR), data=enve_test)

n4.2 <- glmer.nb(extortions ~ bribes + tasahom + years +
                   (1 | NOM_ABR), data=enve_test)

n4.3 <- glmer.nb(extortions ~ bribes + tasahom +
                   (1 | NOM_ABR), data=enve_test)

n4.4 <- glmer.nb(extortions ~ bribes +
                   (1 | NOM_ABR), data=enve_test)

n4.null <- glmer.nb(extortions ~ 1 +
                   (1 | NOM_ABR), data=enve_test)

# Compare all NB GLMMs
tx.n4.x <- texreg(list(n4.0, n4.1, n4.2, n4.3, n4.4, n4.null), caption="Comparison of all NB mixed models",
                  label="T_tx_n4x", booktabs=TRUE)

tx.n4.x

anova.n4.x <- anova(n4.null, n4.4, n4.3, n4.2, n4.1, n4.0, test="Chisq")

anova.n4.x

xan4.x <- xtable(anova.n4.x, caption="ANOVA test between variables of the NB mixed models", label="T_xan4x")


#### Using glmmADMB
# 4. Negative Binomial GLMM_ADMB

ADn4.0 <- glmmadmb(extortions ~ bribes + tasahom + years + subsector + size + (1 | NOM_ABR), data=enve_test,
                  family="nbinom", zeroInflation=FALSE, extra.args="-ndi 40000")

summary(ADn4.0)

anova.ADn4.0 <- anova(ADn4.0, test="Chisq")

anova.ADn4.0

xaADn4.0 <- xtable(anova.ADn4.0, caption="Analysis of deviance of Negative Binomial Mixed Model", label="T_aADn4.0")

print(xaADn4.0)

# Comparison between Poisson and NB GLMMs

tx.n3_ADn4 <- texreg(list(n3, ADn4.0), caption="Comparison of Poisson and NB Mixed Models", label="T_n3ADn4", booktabs=TRUE)

tx.n3_ADn4

lr.n3_ADn4 <- lrtest(n3, ADn4.0)

lr.n3_ADn4

xlr.n3_ADn4 <- xtable(lr.n3_ADn4, caption="Likelihood ratio test between Poisson and NB Mixed Models", label="T_lrn3ADn4")

print(xlr.n3_ADn4)

# 4.x Investigating different variables in the NB GLMMs

ADn4.1 <- glmmadmb(extortions ~ bribes + tasahom + years + subsector +
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 40000")

ADn4.2 <- glmmadmb(extortions ~ bribes + tasahom + years +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 40000")

ADn4.3 <- glmmadmb(extortions ~ bribes + tasahom +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 40000")

ADn4.4 <- glmmadmb(extortions ~ bribes +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 40000")

ADn4.null <- glmmadmb(extortions ~ 1 +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 40000")

# Compare all NB GLMMs
tx.ADn4.x <- texreg(list(ADn4.0, ADn4.1, ADn4.2, ADn4.3, ADn4.4, ADn4.null), caption="Comparison of all NB mixed models",
                  label="T_tx_ADn4x", booktabs=TRUE)

tx.ADn4.x

anova.ADn4.x <- anova(ADn4.null, ADn4.4, ADn4.3, ADn4.2, ADn4.1, ADn4.0, test="Chisq")

anova.ADn4.x

xaADn4.x <- xtable(anova.ADn4.x, caption="ANOVA test between variables of the NB mixed models", label="T_xaADn4x")

save(n3, n4.0, anova.n4.0, xan4.0, tx.n3_n4, lr.n3_n4, xlr.n3_n4,
        n4.1, n4.2, n4.3, n4.4, n4.null, tx.n4.x, anova.n4.x, xan4.x,
        ADn4.0, anova.ADn4.0, xaADn4.0, tx.n3_ADn4, lr.n3_ADn4, xlr.n3_ADn4,
        ADn4.1, ADn4.2, ADn4.3, ADn4.4, ADn4.null, tx.ADn4.x, anova.ADn4.x, xaADn4.x,
        file=paste(dir_name, "mixed_models2.Rdata", sep=""))

# Comparison between  round one and round two
# GLM
tx.m2_n2 <- texreg(list(m2, n2), caption="NB Models using sector and subsector covariates", label="T_m2n2")

tx.m2_n2

anova.m2_n2 <- anova(m2, n2, test="Chisq")

anova.m2_n2

xam2n2 <- xtable(anova.m2_n2, caption="ANOVA between the NB models using sector and subsector", label="T_xanm2n2")

print(xam2n2)

# GLMM
tx.m4_n4 <- texreg(list(m4.0, n4.0), caption="NB Mixed Models using sector and subsector covariates", label="T_m4n4")

tx.m4_n4

anova.m4_n4 <- anova(m4.0, n4.0, test="Chisq")

anova.m4_n4

xam4n4 <- xtable(anova.m4_n4, caption="ANOVA between the NB mixed models using sector and subsector", label="T_xanm4n4")

print(xam4n4)

# GLMM_ADMB
tx.ADm4_ADn4 <- texreg(list(ADm4.0, ADn4.0), caption="NB Mixed Models using sector and subsector covariates", label="T_ADm4ADn4")

tx.ADm4_ADn4

anova.ADm4_ADn4 <- anova(ADm4.0, ADn4.0, test="Chisq")

anova.ADm4_ADn4

xaADm4ADn4 <- xtable(anova.ADm4_ADn4, caption="ANOVA between the NB mixed models using sector and subsector", label="T_xanADm4ADn4")

print(xaADm4ADn4)

save(tx.m2_n2, anova.m2_n2, xam2n2,
      tx.m4_n4, anova.m4_n4, xam4n4,
      tx.ADm4_ADn4, anova.ADm4_ADn4, xaADm4ADn4, file=paste(dir_name, "nbmodels12.Rdata", sep=""))

# Compare NB GLM to NB GLMM

tx.m2_m4_ADm4.0 <- texreg(list(m2, m4.0, ADm4.0), caption="Comparison of NB GLM to Mixed Models", label="T_m2m4ADm4", booktabs=TRUE)

tx.m2_m4_ADm4.0

lr.m2_m4 <- lrtest(m2, m4.0)

lr.m2_m4

xlr.m2_m4 <- xtable(lr.m2_m4, caption="Likelihood ratio test between NB GLM and Mixed Models", label="T_lrm2_m4")

print(xlr.m2_m4)

lr.m2_ADm4 <- lrtest(m2, ADm4.0)

lr.m2_ADm4

xlr.m2_ADm4 <- xtable(lr.m2_ADm4, caption="Likelihood ratio test between NB GLM and Mixed Models", label="T_lrm2_ADm4")

print(xlr.m2_ADm4)

## from round 2

tx.n2_n4_ADn4.0 <- texreg(list(n2, n4.0, ADn4.0), caption="Comparison of NB GLM to Mixed Models", label="T_n2n4ADn4", booktabs=TRUE)

tx.n2_n4_ADn4.0

lr.n2_n4 <- lrtest(n2, n4.0)

lr.n2_n4

xlr.n2_n4 <- xtable(lr.n2_n4, caption="Likelihood ratio test between NB GLM and Mixed Models", label="T_lrn2_n4")

print(xlr.n2_n4)

lr.n2_ADn4 <- lrtest(n2, ADn4.0)

lr.n2_ADn4

xlr.n2_ADn4 <- xtable(lr.n2_ADn4, caption="Likelihood ratio test between NB GLM and Mixed Models", label="T_lrn2_ADn4")

print(xlr.n2_ADn4)

save(tx.m2_m4_ADm4.0, lr.m2_m4, xlr.m2_m4, tx.n2_n4_ADn4.0, lr.n2_n4, xlr.n2_n4,
      lr.n2_ADn4, xlr.n2_ADn4, file=paste(dir_name, "nbGLMvGLMM.Rdata", sep=""))

###### END OF FILE #######
