##########################################################################
##########################################################################
##########################################################################
##########################################################################

## ENVE_script.R

##########################################################################
##########################################################################
##########################################################################
##########################################################################

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
enve_test$subsector <- relevel(enve_test$subsector, ref="Retail")
levels(enve_test$subsector)

enve_test$restbar <- enve_test$subsector
hotindx <- which(levels(enve_test$restbar) == "HotelsRestBar")
levels(enve_test$restbar)[-hotindx] <- "Other"
enve_test$restbar <- relevel(enve_test$restbar, ref="Other")
levels(enve_test$restbar)

enve_test$years <- 2013 - as.numeric(as.character(enve_all$P3))

intyears <- classIntervals(enve_test$years, 5, style="quantile")
intyears$brks

enve_test$yearsquant <- cut(enve_test$years, intyears$brks, right=FALSE)

intyears2 <- classIntervals(enve_test$years, 6, style="quantile")
intyears2$brks

enve_test$yearsquant2 <- cut(enve_test$years, intyears2$brks, right=FALSE)


enve_test <- merge(enve_test, homicidios, by="CVE_ENT", all.x=TRUE)

enve_test$extortions[is.na(enve_test$extortions)] <- 0
enve_test$bribes[is.na(enve_test$bribes)] <- 0

head(enve_test, 25)

tail(enve_test, 25)


##########################################################################
##########################################################################
##########################################################################
##########################################################################

# EDA

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Distribution of extortion victimisations

ext_dist <- data.frame(table(enve_test$extortions))

colnames(ext_dist) <- c("Events", "Prevalence")

ext_dist$Events<- as.integer(as.character(ext_dist$Events))

ext_dist$Incidence <- ext_dist$Events * ext_dist$Prevalence

ext_dist$preval_per <- prop.table(ext_dist$Prevalence)*100

ext_dist$victim_per[2:length(ext_dist$Events)] <- prop.table(
                                                    ext_dist[2:length(
                                                      ext_dist$Events),2])*100

ext_dist$incid_per <- prop.table(ext_dist$Incidence)*100

ext_dist

### Package_install(xtable)

xext_dist <- xtable(ext_dist, digits=c(0,0,0,0,3,3,3),
                      caption="The distribution of extortion victimisations",
                      label="T_dist")

print(xext_dist, include.rownames=FALSE)

# Testing for Poisson distribution

# analysing the mean variance relationship

## Add the index of overdispersion calculation

n <- length(enve_test$extortions)
mean_ext <- mean(enve_test$extortions)
var_ext <- var(enve_test$extortions)

mean_ext
var_ext

var_mean_ratio <- var_ext/mean_ext
var_mean_ratio

index_ext <- id.test(enve_test$extortions)
index_ext

vmr_df <- data.frame(Mean=mean_ext, Variance=var_ext, Ratio=var_mean_ratio,
                      Index=unname(index_ext[1]), Pvalue=unname(index_ext[2]),
                      DF=unname(index_ext[3]))

vmr_df


xvmr_df <- xtable(vmr_df, digits=4,
          caption="If the variance-mean ratio is larger than one, there is over-disperison",
                    label="T_vmr")

print(xvmr_df, include.rownames=FALSE)



# Create DF to fit obs v expected for counts

obsexp <- data.frame(Events=0:ext_dist[length(ext_dist$Events),1])

obsexp <- merge(obsexp, ext_dist, by="Events", all.x=TRUE)

obsexp <- obsexp[,c("Events", "Prevalence")]

colnames(obsexp) <- c("Events", "Obs")

obsexp$Obs[is.na(obsexp$Obs)] <- 0

obsexp

# Generate Poisson expected frequencies

obsexp$exp_po <- dpois(0:(length(obsexp$Events)-1), lambda=mean_ext)
                            * length(enve_test$extortions)

####### Add the KS tests

# Poisson test

po_chisq <- obs_exp_test(obsexp, obsexp$exp_po, 1)

po_chisq

xpo_chisq <- xtable(po_chisq[[1]], digits=c(0,0,0,3),
              caption="Observed vs. Expected (Poisson)", label="T_po_chisq")

print(xpo_chisq, include.rownames=FALSE)

## KS Test, requires library(dgof)
ks.ext.po <- ks.test(enve_test$extortions, ecdf(rpois(0:(n-1),
                      lambda=mean_ext)),  alternative="two.sided")

ks.ext.po

# Testing for Negative Binomial distribution

# loading MASS package

### Package_install(MASS)

# Obtaining the parameters for negbin

nb_estimates <- fitdistr(enve_test$extortions, "Negative Binomial")

nb_estimates

# Generating the neg bin expected frequencies

obsexp$exp_nb <- dnbinom(0:(length(obsexp$Events)-1), size=nb_estimates$estimate[1],
                          mu=nb_estimates$estimate[2]) * length(enve_test$extortions)

# NB Tests

nb_chisq <- obs_exp_test(obsexp, obsexp$exp_nb, 2)

nb_chisq

xnb_chisq <- xtable(nb_chisq[[1]], digits=c(0,0,0,3),
              caption="Observed vs. Expected (Negatve Binomial)", label="T_nb_chisq")

print(xnb_chisq, include.rownames=FALSE)


## KS Test, requires library(dgof)
ks.ext.nb <- ks.test(enve_test$extortions, ecdf(rnbinom(0:(n-1),
                                              size=nb_estimates$estimate[1],
                                                mu=nb_estimates$estimate[2])),
                                                 alternative="two.sided")

ks.ext.nb

# table with both
xobsexp <- xtable(obsexp, digits=c(0,0,0,3,3),
caption="Observed and expected frequencies under Poisson and Negative Binomial distributions",
                  label="T_obsexp")

print(xobsexp, include.rownames=FALSE)

## Plots of this

oe_dfpo <- data.frame(Events=rep(0:40, 2), Obs=c(obsexp$Obs, obsexp$exp_po),
                    Class=c(rep("Observed",41), rep("Poisson", 41)))

oe_dfpo[oe_dfpo[,2] < 1,2] <- 0

oe_dfpo

oe_df <- data.frame(Events=rep(0:40, 3), Obs=c(obsexp$Obs, obsexp$exp_po,
                                               obsexp_true$exp_nb),
                    Class=c(rep("Observed",41), rep("Poisson", 41), rep("NB", 41)))

oe_df[oe_df[,2] < 1,2] <- 0

oe_df

# Plot the observed distribution

######## Generate the plots using the new aesthetic.

### Package_install(ggplot2)
### Package_install(Cairo)

plot.obs <- ggplot(obsexp, aes(x=Events, y=clog10(Obs))) + geom_bar(stat="identity") +
              + ylab("Frequency (log10 scale)") +
              scale_y_continuous(labels=c(0,10,100,1000,10000,100000)) + theme_bw()

plot.exp_po <- ggplot(obsexp, aes(x=Events, y=clog10(exp_po))) + geom_bar(stat="identity") +
              + ylab("Frequency (log10 scale)") +
              scale_y_continuous(labels=c(0,10,100,1000,10000,100000)) + theme_bw()

plot.exp_nb <- ggplot(obsexp, aes(x=Events, y=clog10(exp_nb))) + geom_bar(stat="identity") +
              + ylab("Frequency (log10 scale)") +
              scale_y_continuous(labels=c(0,10,100,1000,10000,100000)) + theme_bw()

plot.obspo <- ggplot(oe_dfpo, aes(x=Events, y=clog10(Obs), fill=Class)) +
  geom_bar(stat="identity", position = "dodge") +
  ylab("Frequency (log10 scale)") +
  scale_y_continuous(labels=c(0,10,100,1000,10000,100000)) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_grey() +
  theme_bw() +
  theme(legend.position="bottom")

plot.oe <- ggplot(oe_df, aes(x=Events, y=clog10(Obs), fill=Class)) +
  geom_bar(stat="identity", position = "dodge") +
  ylab("Frequency (log10 scale)") +
  scale_y_continuous(labels=c(0,10,100,1000,10000,100000)) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_grey() +
  theme_bw() +
  theme(legend.position="bottom")

# Save ggplot objects

dist.plots <- list(plot.obs=plot.obs, plot.exp_po=plot.exp_po, plot.exp_nb=plot.exp_nb,
  plot.obspo=plot.obspo, plot.oe=plot.oe)


# Save ggplots as images

for (i in 1:length(dist.plots))
{
  ggfile <- paste(dir_name, names(dist.plots[i]), ".pdf", sep="")
  ggsave(dist.plots[[i]], file=ggfile, width=6, height=2.7)
}

for (i in 1:length(dist.plots))
{
  ggfile <- paste(dir_name, names(dist.plots[i]), ".png", sep="")
  ggsave(dist.plots[[i]], file=ggfile, width=6, height=2.7, type="cairo-png")
}

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Independent Variables

##########################################################################
##########################################################################
##########################################################################
##########################################################################

summ_table <- data.frame(variable=0, N=0, prevalence=0, incidence=0, mean=0,
                          sd=0, min=0, max=0)

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

  else if (colnames(enve_test)[i] %in% c("sector", "size", "subsector",
                                          "restbar", "yearsquant", "yearsquant2"))
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

# homicido absoluto
summ_table[length(summ_table[,1])+1,] <- c(NA, length(homicidios[,"denuncias_homs"]), NA, NA,
                                           mean(homicidios[,"denuncias_homs"]), sd(homicidios[,"denuncias_homs"]),
                                           min(homicidios[,"denuncias_homs"]), max(homicidios[,"denuncias_homs"]))

summ_table[length(summ_table[,1]),1] <- "Murders"

#population
summ_table[length(summ_table[,1])+1,] <- c(NA, length(homicidios[,"poblacion"]), NA, NA,
                                           mean(homicidios[,"poblacion"]), sd(homicidios[,"poblacion"]),
                                           min(homicidios[,"poblacion"]), max(homicidios[,"poblacion"]))

summ_table[length(summ_table[,1]),1] <- "Population"

summ_table <- summ_table[!is.na(summ_table[,1]),]

summ_table

xsumm_table <- xtable(summ_table, digits=c(0,0,0,0,0,3,3,0,0), caption="Descriptive Statistics", label="T_summ")

print(xsumm_table, include.rownames=FALSE)

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Bribes variable

##########################################################################
##########################################################################
##########################################################################
##########################################################################

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

plot.bribes <- ggplot(bribes, aes(x=Events, y=Freq)) + geom_bar(stat="identity") +
  ylab("Frequency") +
  theme_bw()

plot.log_bribes <- ggplot(bribes_d, aes(x=Events, y=clog10(Freq))) +
  geom_bar(stat="identity") +
  ylab("Frequency (log10 scale)") +
  scale_y_continuous(labels=c(0,10,100,1000,10000,100000)) +
  theme_bw()

plot.ext_bribes <- ggplot(enve_test, aes(x=bribes, y=extortions)) + geom_jitter() +
  geom_smooth(method="lm") +
  geom_smooth(method="lm") + ylab("Extortion events") + xlab("Bribes") +
  theme_bw()

ggsave(plot.bribes, file=paste(dir_name, "plot_bribes.pdf", sep=""), width=5, height=4)
ggsave(plot.log_bribes, file=paste(dir_name, "plot_log_bribes.pdf", sep=""), width=5, height=4)
ggsave(plot.ext_bribes, file=paste(dir_name, "plot_ext_bribes.pdf", sep=""), width=5, height=4)

ggsave(plot.bribes, file=paste(dir_name, "plot_bribes.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.log_bribes, file=paste(dir_name, "plot_log_bribes.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.ext_bribes, file=paste(dir_name, "plot_ext_bribes.png", sep=""), width=5, height=4, type="cairo-png")

# Examine the relationship between bribes and extortions

ext_bribesA <- ftable(enve_test$bribes, enve_test$extortions)

ext_bribesA

temp_ext <- as.factor(enve_test$extortions)
levels(temp_ext)[6:length(levels(temp_ext))] <- "5+"
temp_bribes <- as.factor(enve_test$bribes)
levels(temp_bribes)[6:length(levels(temp_bribes))] <- "5+"

ext_bribes <- ftable(temp_bribes, temp_ext)

ext_bribes

xext_bribes <- xtable(format(ext_bribes), caption="The distribution of extortions and bribery victimisations",
                        labels="T_ext_bribes")

print(xext_bribes, include.rownames=FALSE)

#Using simulated p-values
chisq.ext_bribes <- chisq.test(ext_bribes, simulate.p.value = TRUE, B=9999)
chisq.ext_bribes

##Cramers V
cv.ext_bribes <- cv.test(ext_bribes)
cv.ext_bribes

#Pearson's correlation
cor.ext_bribes <- with(enve_test, cor.test(bribes, extortions, method="pearson"))
cor.ext_bribes

##########################################################################
##########################################################################
##########################################################################
##########################################################################

## Years variable

##########################################################################
##########################################################################
##########################################################################
##########################################################################

ext_years <- ftable(Years=enve_test$yearsquant, temp_ext)

ext_years

xext_years <- xtable(format(ext_years), caption="The distribution of extortion victimisations per year quintiles",
                        lab="t_ext_years")

print(xext_years, include.rownames=FALSE)

chisq.ext_years <- chisq.test(ext_years)
chisq.ext_years

chisq.ext_years <- chisq.test(ext_years, simulate.p.value = TRUE, B=9999)
chisq.ext_years

cv.ext_years <- cv.test(ext_years)
cv.ext_years

## Quantile version 2
ext_years2 <- ftable(Years=enve_test$yearsquant2, temp_ext)

ext_years2

xext_years2 <- xtable(format(ext_years2), caption="The distribution of extortion victimisations per year quantiles",
                        lab="t_ext_years2")

print(xext_years2, include.rownames=FALSE)

chisq.ext_years2 <- chisq.test(ext_years2)
chisq.ext_years2

chisq.ext_years2 <- chisq.test(ext_years2, simulate.p.value = TRUE, B=9999)
chisq.ext_years2

cv.ext_years2 <- cv.test(ext_years2)
cv.ext_years2

### using the raw years variable

cor.ext_years <- with(enve_test, cor.test(extortions, years, method="pearson"))
cor.ext_years

# Plots of ext_years relationship

ey_df <- data.frame(ext_years)

plot.ey <- ggplot(ey_df, aes(x=temp_ext, y=Freq, fill=Years)) +
                    geom_bar(stat="identity") +
                    facet_grid(Years~.) +
                    ylab("Frequency") + xlab("Events") + theme_bw()

plot.log_ey <- ggplot(ey_df, aes(x=temp_ext, y=clog10(Freq), fill=Years)) +
                    geom_bar(stat="identity") +
                    facet_grid(Years~.) +
                    ylab("Frequency (log10 scale)") + xlab("Events") + theme_bw() #+
                    #scale_y_continuous(labels=c(0,10,100,1000,10000,100000))

# For quantiles2
ey_df2 <- data.frame(ext_years2)

plot.ey2 <- ggplot(ey_df2, aes(x=temp_ext, y=Freq, fill=Years)) +
                  geom_bar(stat="identity") +
                  facet_grid(Years~.) +
                  ylab("Frequency") + xlab("Events") + theme_bw()

plot.log_ey2 <- ggplot(ey_df2, aes(x=temp_ext, y=clog10(Freq), fill=Years)) +
                  geom_bar(stat="identity") +
                  facet_grid(Years~.) +
                  ylab("Frequency (log10 scale)") + xlab("Events") + theme_bw() #+
                  #scale_y_continuous(labels=c(0,10,100,1000,10000,100000))

# For raw years number
plot.ext_years <- ggplot(enve_test, aes(x=years, y=extortions)) + geom_jitter() +
                          geom_smooth(method="lm") +
                          xlab("Years") + ylab("Extortions") + theme_bw()

# Save ggplots as images
ggsave(plot.ey, file=paste(dir_name, "plot_ey.pdf", sep=""), width=5, height=4)
ggsave(plot.log_ey, file=paste(dir_name, "plot_log_ey.pdf", sep=""), width=5, height=4)
ggsave(plot.ey2, file=paste(dir_name, "plot_ey.pdf", sep=""), width=5, height=4)
ggsave(plot.log_ey2, file=paste(dir_name, "plot_log_ey.pdf", sep=""), width=5, height=4)
ggsave(plot.ext_years, file=paste(dir_name, "plot_ext_years.pdf", sep=""), width=5, height=4)

ggsave(plot.ey, file=paste(dir_name, "plot_ey.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.log_ey, file=paste(dir_name, "plot_log_ey.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.ey2, file=paste(dir_name, "plot_ey.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.log_ey2, file=paste(dir_name, "plot_log_ey.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.ext_years, file=paste(dir_name, "plot_ext_years.png", sep=""), width=5, height=4, type="cairo-png")

##########################################################################
##########################################################################
##########################################################################
##########################################################################

## Sector

##########################################################################
##########################################################################
##########################################################################
##########################################################################

ext_sector <- ftable(enve_test$sector, temp_ext)

ext_sector

xext_sector <- xtable(format(ext_sector), caption="Bribery victimisations by sector", label="T_ext_sector")

print(xext_sector, include.rownames=FALSE)

chisq.ext_sector <- chisq.test(ext_sector)
chisq.ext_sector

cv.ext_sector <- cv.test(ext_sector)
cv.ext_sector

##########################################################################
##########################################################################
##########################################################################
##########################################################################

## Subsector

##########################################################################
##########################################################################
##########################################################################
##########################################################################

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

chisq.ext_subsector <- chisq.test(ext_subsector, simulate.p.value=TRUE, B=9999)
chisq.ext_subsector

cv.ext_subsector<- cv.test(ext_subsector)
cv.ext_subsector

##########################################################################
##########################################################################
##########################################################################
##########################################################################

## restbar

##########################################################################
##########################################################################
##########################################################################
##########################################################################

ext_restbar <- ftable(enve_test$restbar, temp_ext)

ext_restbar

xext_restbar <- xtable(format(ext_restbar), caption="Exortion victimisations of rest bar vs collapsed subectors", label="T_ext_restbar")

print(xext_restbar, include.rownames=FALSE)

chisq.ext_restbar <- chisq.test(ext_restbar)
chisq.ext_restbar

chisq.ext_restbar <- chisq.test(ext_restbar, simulate.p.value=TRUE, B=9999)
chisq.ext_restbar

cv.ext_restbar<- cv.test(ext_restbar)
cv.ext_restbar

##########################################################################
##########################################################################
##########################################################################
##########################################################################

## Size

##########################################################################
##########################################################################
##########################################################################
##########################################################################

ext_size <- ftable(enve_test$size, temp_ext)

ext_size

xext_size <- xtable(format(ext_size), caption="Exortion victimisations by size", label="T_ext_size")

print(xext_size, include.rownames=FALSE)

chisq.ext_size <- chisq.test(ext_size)
chisq.ext_size

cv.ext_size <- cv.test(ext_size)
cv.ext_size

##########################################################################
##########################################################################
##########################################################################
##########################################################################

## Area level influences

##########################################################################
##########################################################################
##########################################################################
##########################################################################

state_ext_dist <- ftable(enve_test$NOM_ENT, temp_ext)

state_ext_dist

xstate_ext_dist <- xtable(format(state_ext_dist), caption="Distribution of extortions by state.", label="T_s_ext_dist")

print(xstate_ext_dist, include.rownames=FALSE)

state_ext <- with(enve_test, table(CVE_ENT, extortions))

state_hom <- homicidios

state_inc <- t(t(state_ext)*as.integer(colnames(state_ext)))

state_summ1 <- data.frame(data.frame(margin.table(state_ext[,2:ncol(state_ext)],1))/
                            data.frame(margin.table(state_ext[,1:ncol(state_ext)],1))*1000)

state_summ1 <- cbind(state_summ1, data.frame(margin.table(state_inc,1))[,2]/
                       data.frame(margin.table(state_ext[,1:ncol(state_ext)],1))*1000)

state_summ1 <- cbind(state_summ1, data.frame(margin.table(state_ext,1))[,2])

state_bribes <- with(enve_test, table(CVE_ENT, bribes))

state_brinc <- t(t(state_bribes)*as.integer(colnames(state_bribes)))

state_summ1 <- cbind(state_summ1,
                          data.frame(margin.table(state_bribes[,2:ncol(state_bribes)],1))/
                          data.frame(margin.table(state_bribes[,1:ncol(state_bribes)],1))*1000)

state_summ1 <- cbind(state_summ1, data.frame(margin.table(state_brinc,1))[,2]/
                       data.frame(margin.table(state_bribes[,1:ncol(state_bribes)],1))*1000)


colnames(state_summ1) <- c("Prevalence", "Incidence", "N", "Bribes Preval.", "Bribes Inci.")

state_summ1 <- cbind(state_summ1,
                     Concentration=state_summ1$Incidence/state_summ1$Prevalence)

state_summ1 <- cbind(state_summ1,
                      `Bribes Conc.`=state_summ1$`Bribes Inci.`/state_summ1$`Bribes Preval.`)

state_summ1 <- cbind(state_summ1, CVE_ENT= as.integer(rownames(state_summ1)))

state_summ1 <- merge(state_summ1, state_hom, by="CVE_ENT")

state_summ1

xstate_summ1 <- xtable(state_summ1, caption="Key indicators of extortion victimisation by state",
                          labels="T_state_summ")

print(xstate_summ1, include.rownames=FALSE)

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Area prevalence and concentration

##########################################################################
##########################################################################
##########################################################################
##########################################################################

plot.inc_preval <- ggplot(state_summ1, aes(x=Prevalence, y=Incidence)) + geom_point() + geom_smooth(method="lm") + theme_bw()

cor.inc_preval <- with(state_summ1, cor.test(Incidence, Prevalence))

cor.inc_preval

plot.con_preval <- ggplot(state_summ1, aes(x=Prevalence, y=Concentration)) + geom_point() + geom_smooth(method="lm") + theme_bw()

cor.con_preval <- with(state_summ1, cor.test(Concentration, Prevalence))

cor.con_preval

# save these last ggplots as images
ggsave(plot.inc_preval, file=paste(dir_name, "plot_inc_preval.pdf", sep=""), width=5, height=4)
ggsave(plot.con_preval, file=paste(dir_name, "plot_con_preval.pdf", sep=""), width=5, height=4)

ggsave(plot.inc_preval, file=paste(dir_name, "plot_inc_preval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.con_preval, file=paste(dir_name, "plot_con_preval.png", sep=""), width=5, height=4, type="cairo-png")

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Area-level influences: murder rate

##########################################################################
##########################################################################
##########################################################################
##########################################################################

plot.mur_inc <- ggplot(state_summ1, aes(x=tasahom, y=Incidence)) +
                          geom_point() + geom_smooth(method="lm")  + xlab("Murder rate") +
                          theme_bw()

cor.mur_inc <- with(state_summ1, cor.test(Incidence, tasahom))

cor.mur_inc

plot.mur_preval <- ggplot(state_summ1, aes(x=tasahom, y=Prevalence)) +
                          geom_point() + geom_smooth(method="lm") + xlab("Murder rate") +
                          theme_bw()

cor.mur_preval <- with(state_summ1, cor.test(Prevalence, tasahom))

cor.mur_preval

plot.mur_con <- ggplot(state_summ1, aes(x=tasahom, y=Concentration)) +
                            geom_point() + geom_smooth(method="lm") + xlab("Murder rate") +
                            theme_bw()

cor.mur_con <- with(state_summ1, cor.test(Concentration, tasahom))

cor.mur_con

# Save the ggplot objects as images
ggsave(plot.mur_inc, file=paste(dir_name, "plot_mur_inc.pdf", sep=""), width=5, height=4)
ggsave(plot.mur_preval, file=paste(dir_name, "plot_mur_preval.pdf", sep=""), width=5, height=4)
ggsave(plot.mur_con, file=paste(dir_name, "plot_mur_con.pdf", sep=""), width=5, height=4)

ggsave(plot.mur_inc, file=paste(dir_name, "plot_mur_inc.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.mur_preval, file=paste(dir_name, "plot_mur_preval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.mur_con, file=paste(dir_name, "plot_mur_con.png", sep=""), width=5, height=4, type="cairo-png")

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Area-level influences: absolute murder raw and log transformed
# denuncias_homs

##########################################################################
##########################################################################
##########################################################################
##########################################################################

plot.raw_inc <- ggplot(state_summ1, aes(x=denuncias_homs, y=Incidence)) +
                          geom_point() + geom_smooth(method="lm")  + xlab("Murders (Abs.)") +
                          theme_bw()

cor.raw_inc <- with(state_summ1, cor.test(Incidence, denuncias_homs))

cor.raw_inc

plot.raw_preval <- ggplot(state_summ1, aes(x=denuncias_homs, y=Prevalence)) +
                          geom_point() + geom_smooth(method="lm") + xlab("Murders (Abs.)") +
                          theme_bw()

cor.raw_preval <- with(state_summ1, cor.test(Prevalence, denuncias_homs))

cor.raw_preval

plot.raw_con <- ggplot(state_summ1, aes(x=denuncias_homs, y=Concentration)) +
                            geom_point() + geom_smooth(method="lm") + xlab("Murders (Abs.)") +
                            theme_bw()

cor.raw_con <- with(state_summ1, cor.test(Concentration, denuncias_homs))

cor.raw_con

# Save the ggplot objects as images
ggsave(plot.raw_inc, file=paste(dir_name, "plot_raw_inc.pdf", sep=""), width=5, height=4)
ggsave(plot.raw_preval, file=paste(dir_name, "plot_raw_preval.pdf", sep=""), width=5, height=4)
ggsave(plot.raw_con, file=paste(dir_name, "plot_raw_con.pdf", sep=""), width=5, height=4)

ggsave(plot.raw_inc, file=paste(dir_name, "plot_raw_inc.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.raw_preval, file=paste(dir_name, "plot_raw_preval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.raw_con, file=paste(dir_name, "plot_raw_con.png", sep=""), width=5, height=4, type="cairo-png")

## log transformed

plot.lograw_inc <- ggplot(state_summ1, aes(x=log(denuncias_homs), y=Incidence)) +
                          geom_point() + geom_smooth(method="lm")  + xlab("Murders (log)") +
                          theme_bw()

cor.lograw_inc <- with(state_summ1, cor.test(Incidence, log(denuncias_homs)))

cor.lograw_inc

plot.lograw_preval <- ggplot(state_summ1, aes(x=log(denuncias_homs), y=Prevalence)) +
                          geom_point() + geom_smooth(method="lm") + xlab("Murders (log)") +
                          theme_bw()

cor.lograw_preval <- with(state_summ1, cor.test(Prevalence, log(denuncias_homs)))

cor.lograw_preval

plot.lograw_con <- ggplot(state_summ1, aes(x=log(denuncias_homs), y=Concentration)) +
                            geom_point() + geom_smooth(method="lm") + xlab("Murders (log)") +
                            theme_bw()

cor.lograw_con <- with(state_summ1, cor.test(Concentration, log(denuncias_homs)))

cor.lograw_con

# Save the ggplot objects as images
ggsave(plot.lograw_inc, file=paste(dir_name, "plot_lograw_inc.pdf", sep=""), width=5, height=4)
ggsave(plot.lograw_preval, file=paste(dir_name, "plot_lograw_preval.pdf", sep=""), width=5, height=4)
ggsave(plot.lograw_con, file=paste(dir_name, "plot_lograw_con.pdf", sep=""), width=5, height=4)

ggsave(plot.lograw_inc, file=paste(dir_name, "plot_lograw_inc.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.lograw_preval, file=paste(dir_name, "plot_lograw_preval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.lograw_con, file=paste(dir_name, "plot_lograw_con.png", sep=""), width=5, height=4, type="cairo-png")


##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Area-level influences: population raw and log transformed
# poblacion

##########################################################################
##########################################################################
##########################################################################
##########################################################################

plot.pop_inc <- ggplot(state_summ1, aes(x=poblacion, y=Incidence)) +
                          geom_point() + geom_smooth(method="lm")  + xlab("Population") +
                          theme_bw()

cor.pop_inc <- with(state_summ1, cor.test(Incidence, poblacion))

cor.pop_inc

plot.pop_preval <- ggplot(state_summ1, aes(x=poblacion, y=Prevalence)) +
                          geom_point() + geom_smooth(method="lm") + xlab("Population") +
                          theme_bw()

cor.pop_preval <- with(state_summ1, cor.test(Prevalence, poblacion))

cor.pop_preval

plot.pop_con <- ggplot(state_summ1, aes(x=poblacion, y=Concentration)) +
                            geom_point() + geom_smooth(method="lm") + xlab("Population") +
                            theme_bw()

cor.pop_con <- with(state_summ1, cor.test(Concentration, poblacion))

cor.pop_con

# Save the ggplot objects as images
ggsave(plot.pop_inc, file=paste(dir_name, "plot_pop_inc.pdf", sep=""), width=5, height=4)
ggsave(plot.pop_preval, file=paste(dir_name, "plot_pop_preval.pdf", sep=""), width=5, height=4)
ggsave(plot.pop_con, file=paste(dir_name, "plot_pop_con.pdf", sep=""), width=5, height=4)

ggsave(plot.pop_inc, file=paste(dir_name, "plot_pop_inc.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.pop_preval, file=paste(dir_name, "plot_pop_preval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.pop_con, file=paste(dir_name, "plot_pop_con.png", sep=""), width=5, height=4, type="cairo-png")

## log transformed

plot.logpop_inc <- ggplot(state_summ1, aes(x=log(poblacion), y=Incidence)) +
                          geom_point() + geom_smooth(method="lm")  + xlab("Population") +
                          theme_bw()

cor.logpop_inc <- with(state_summ1, cor.test(Incidence, log(poblacion)))

cor.logpop_inc

plot.logpop_preval <- ggplot(state_summ1, aes(x=log(poblacion), y=Prevalence)) +
                          geom_point() + geom_smooth(method="lm") + xlab("Population") +
                          theme_bw()

cor.logpop_preval <- with(state_summ1, cor.test(Prevalence, log(poblacion)))

cor.logpop_preval

plot.logpop_con <- ggplot(state_summ1, aes(x=log(poblacion), y=Concentration)) +
                            geom_point() + geom_smooth(method="lm") + xlab("Population") +
                            theme_bw()

cor.logpop_con <- with(state_summ1, cor.test(Concentration, log(poblacion)))

cor.logpop_con

# Save the ggplot objects as images
ggsave(plot.logpop_inc, file=paste(dir_name, "plot_logpop_inc.pdf", sep=""), width=5, height=4)
ggsave(plot.logpop_preval, file=paste(dir_name, "plot_logpop_preval.pdf", sep=""), width=5, height=4)
ggsave(plot.logpop_con, file=paste(dir_name, "plot_logpop_con.pdf", sep=""), width=5, height=4)

ggsave(plot.logpop_inc, file=paste(dir_name, "plot_logpop_inc.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.logpop_preval, file=paste(dir_name, "plot_logpop_preval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.logpop_con, file=paste(dir_name, "plot_logpop_con.png", sep=""), width=5, height=4, type="cairo-png")


#################### aereal exploration of bribery ######################
##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Area-level influences: murder rate v bribes

##########################################################################
##########################################################################
##########################################################################
##########################################################################

plot.mur_brinc <- ggplot(state_summ1, aes(x=tasahom, y=`Bribes Inci.`)) +
                          geom_point() + geom_smooth(method="lm")  + xlab("Murder rate") +
                          theme_bw()

cor.mur_brinc <- with(state_summ1, cor.test(`Bribes Inci.`, tasahom))

cor.mur_brinc

plot.mur_bpreval <- ggplot(state_summ1, aes(x=tasahom, y=`Bribes Preval.`)) +
                          geom_point() + geom_smooth(method="lm") + xlab("Murder rate") +
                          theme_bw()

cor.mur_bpreval <- with(state_summ1, cor.test(`Bribes Preval.`, tasahom))

cor.mur_bpreval

plot.mur_bcon <- ggplot(state_summ1, aes(x=tasahom, y=`Bribes Conc.`)) +
                            geom_point() + geom_smooth(method="lm") + xlab("Murder rate") +
                            theme_bw()

cor.mur_bcon <- with(state_summ1, cor.test(`Bribes Conc.`, tasahom))

cor.mur_bcon

# Save the ggplot objects as images
ggsave(plot.mur_brinc, file=paste(dir_name, "plot_mur_brinc.pdf", sep=""), width=5, height=4)
ggsave(plot.mur_bpreval, file=paste(dir_name, "plot_mur_bpreval.pdf", sep=""), width=5, height=4)
ggsave(plot.mur_bcon, file=paste(dir_name, "plot_mur_bcon.pdf", sep=""), width=5, height=4)

ggsave(plot.mur_brinc, file=paste(dir_name, "plot_mur_brinc.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.mur_bpreval, file=paste(dir_name, "plot_mur_bpreval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.mur_bcon, file=paste(dir_name, "plot_mur_bcon.png", sep=""), width=5, height=4, type="cairo-png")

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Area-level influences: absolute murder raw and log transformed v bribes
# denuncias_homs

##########################################################################
##########################################################################
##########################################################################
##########################################################################

plot.raw_brinc <- ggplot(state_summ1, aes(x=denuncias_homs, y=`Bribes Inci.`)) +
                          geom_point() + geom_smooth(method="lm")  + xlab("Murders (Abs.)") +
                          theme_bw()

cor.raw_brinc <- with(state_summ1, cor.test(`Bribes Inci.`, denuncias_homs))

cor.raw_brinc

plot.raw_bpreval <- ggplot(state_summ1, aes(x=denuncias_homs, y=`Bribes Preval.`)) +
                          geom_point() + geom_smooth(method="lm") + xlab("Murders (Abs.)") +
                          theme_bw()

cor.raw_bpreval <- with(state_summ1, cor.test(`Bribes Preval.`, denuncias_homs))

cor.raw_bpreval

plot.raw_bcon <- ggplot(state_summ1, aes(x=denuncias_homs, y=`Bribes Conc.`)) +
                            geom_point() + geom_smooth(method="lm") + xlab("Murders (Abs.)") +
                            theme_bw()

cor.raw_bcon <- with(state_summ1, cor.test(`Bribes Conc.`, denuncias_homs))

cor.raw_bcon

# Save the ggplot objects as images
ggsave(plot.raw_brinc, file=paste(dir_name, "plot_raw_brinc.pdf", sep=""), width=5, height=4)
ggsave(plot.raw_bpreval, file=paste(dir_name, "plot_raw_bpreval.pdf", sep=""), width=5, height=4)
ggsave(plot.raw_bcon, file=paste(dir_name, "plot_raw_bcon.pdf", sep=""), width=5, height=4)

ggsave(plot.raw_brinc, file=paste(dir_name, "plot_raw_brinc.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.raw_bpreval, file=paste(dir_name, "plot_raw_bpreval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.rawb_con, file=paste(dir_name, "plot_raw_bcon.png", sep=""), width=5, height=4, type="cairo-png")

## log transformed

plot.lograw_brinc <- ggplot(state_summ1, aes(x=log(denuncias_homs), y=`Bribes Inci.`)) +
                          geom_point() + geom_smooth(method="lm")  + xlab("Murders (log)") +
                          theme_bw()

cor.lograw_brinc <- with(state_summ1, cor.test(`Bribes Inci.`, log(denuncias_homs)))

cor.lograw_brinc

plot.lograw_bpreval <- ggplot(state_summ1, aes(x=log(denuncias_homs), y=`Bribes Preval.`)) +
                          geom_point() + geom_smooth(method="lm") + xlab("Murders (log)") +
                          theme_bw()

cor.lograw_bpreval <- with(state_summ1, cor.test(`Bribes Preval.`, log(denuncias_homs)))

cor.lograw_bpreval

plot.lograw_bcon <- ggplot(state_summ1, aes(x=log(denuncias_homs), y=`Bribes Conc.`)) +
                            geom_point() + geom_smooth(method="lm") + xlab("Murders (log)") +
                            theme_bw()

cor.lograw_bcon <- with(state_summ1, cor.test(`Bribes Conc.`, log(denuncias_homs)))

cor.lograw_bcon

# Save the ggplot objects as images
ggsave(plot.lograw_brinc, file=paste(dir_name, "plot_lograw_brinc.pdf", sep=""), width=5, height=4)
ggsave(plot.lograw_bpreval, file=paste(dir_name, "plot_lograw_bpreval.pdf", sep=""), width=5, height=4)
ggsave(plot.lograw_bcon, file=paste(dir_name, "plot_lograw_bcon.pdf", sep=""), width=5, height=4)

ggsave(plot.lograw_brinc, file=paste(dir_name, "plot_lograw_brinc.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.lograw_bpreval, file=paste(dir_name, "plot_lograw_bpreval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.lograw_bcon, file=paste(dir_name, "plot_lograw_bcon.png", sep=""), width=5, height=4, type="cairo-png")


##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Area-level influences: population raw and log transformed v bribes
# poblacion

##########################################################################
##########################################################################
##########################################################################
##########################################################################

plot.pop_brinc <- ggplot(state_summ1, aes(x=poblacion, y=`Bribes Inci.`)) +
                          geom_point() + geom_smooth(method="lm")  + xlab("Population") +
                          theme_bw()

cor.pop_brinc <- with(state_summ1, cor.test(`Bribes Inci.`, poblacion))

cor.pop_brinc

plot.pop_bpreval <- ggplot(state_summ1, aes(x=poblacion, y=`Bribes Preval.`)) +
                          geom_point() + geom_smooth(method="lm") + xlab("Population") +
                          theme_bw()

cor.pop_bpreval <- with(state_summ1, cor.test(`Bribes Preval.`, poblacion))

cor.pop_bpreval

plot.pop_bcon <- ggplot(state_summ1, aes(x=poblacion, y=`Bribes Conc.`)) +
                            geom_point() + geom_smooth(method="lm") + xlab("Population") +
                            theme_bw()

cor.pop_bcon <- with(state_summ1, cor.test(`Bribes Conc.`, poblacion))

cor.pop_bcon

# Save the ggplot objects as images
ggsave(plot.pop_brinc, file=paste(dir_name, "plot_pop_brinc.pdf", sep=""), width=5, height=4)
ggsave(plot.pop_bpreval, file=paste(dir_name, "plot_pop_bpreval.pdf", sep=""), width=5, height=4)
ggsave(plot.pop_bcon, file=paste(dir_name, "plot_pop_bcon.pdf", sep=""), width=5, height=4)

ggsave(plot.pop_brinc, file=paste(dir_name, "plot_pop_brinc.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.pop_bpreval, file=paste(dir_name, "plot_pop_bpreval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.pop_bcon, file=paste(dir_name, "plot_pop_bcon.png", sep=""), width=5, height=4, type="cairo-png")

## log transformed

plot.logpop_brinc <- ggplot(state_summ1, aes(x=log(poblacion), y=`Bribes Inci.`)) +
                          geom_point() + geom_smooth(method="lm")  + xlab("Population") +
                          theme_bw()

cor.logpop_brinc <- with(state_summ1, cor.test(`Bribes Inci.`, log(poblacion)))

cor.logpop_brinc

plot.logpop_bpreval <- ggplot(state_summ1, aes(x=log(poblacion), y=`Bribes Preval.`)) +
                          geom_point() + geom_smooth(method="lm") + xlab("Population") +
                          theme_bw()

cor.logpop_bpreval <- with(state_summ1, cor.test(`Bribes Preval.`, log(poblacion)))

cor.logpop_bpreval

plot.logpop_con <- ggplot(state_summ1, aes(x=log(poblacion), y=`Bribes Conc.`)) +
                            geom_point() + geom_smooth(method="lm") + xlab("Population") +
                            theme_bw()

cor.logpop_bcon <- with(state_summ1, cor.test(`Bribes Conc.`, log(poblacion)))

cor.logpop_bcon

# Save the ggplot objects as images
ggsave(plot.logpop_brinc, file=paste(dir_name, "plot_logpop_brinc.pdf", sep=""), width=5, height=4)
ggsave(plot.logpop_bpreval, file=paste(dir_name, "plot_logpop_bpreval.pdf", sep=""), width=5, height=4)
ggsave(plot.logpop_bcon, file=paste(dir_name, "plot_logpop_bcon.pdf", sep=""), width=5, height=4)

ggsave(plot.logpop_brinc, file=paste(dir_name, "plot_logpop_brinc.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.logpop_bpreval, file=paste(dir_name, "plot_logpop_bpreval.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.logpop_bcon, file=paste(dir_name, "plot_logpop_bcon.png", sep=""), width=5, height=4, type="cairo-png")



##########################################################################
##########################################################################
##########################################################################
##########################################################################

## Years variable vs Bribes

##########################################################################
##########################################################################
##########################################################################
##########################################################################

bribes_years <- ftable(Years=enve_test$yearsquant, temp_bribes)

bribes_years

xbribes_years <- xtable(format(bribes_years), caption="The distribution of bribery victimisations per year quintiles",
                        lab="t_bribes_years")

print(xbribes_years, include.rownames=FALSE)

chisq.bribes_years <- chisq.test(bibes_years)
chisq.bribes_years

chisq.bribes_years <- chisq.test(bribes_years, simulate.p.value = TRUE, B=9999)
chisq.bribes_years

cv.bribes_years <- cv.test(bribes_years)
cv.bribes_years

## Quantile version 2
bribes_years2 <- ftable(Years=enve_test$yearsquant2, temp_bribes)

bribes_years2

xbribes_years2 <- xtable(format(bribes_years2), caption="The distribution of bribery victimisations per year quantiles",
                        lab="t_bribes_years2")

print(xbribes_years2, include.rownames=FALSE)

chisq.bribes_years2 <- chisq.test(bribes_years2)
chisq.bribes_years2

chisq.bribes_years2 <- chisq.test(bribes_years2, simulate.p.value = TRUE, B=9999)
chisq.bribes_years2

cv.bribes_years2 <- cv.test(bribes_years2)
cv.bribes_years2

### using the raw years variable

cor.bribes_years <- with(enve_test, cor.test(bribes, years, method="pearson"))
cor.bribes_years

# Plots of bribes_years relationship

by_df <- data.frame(bribes_years)

plot.by <- ggplot(by_df, aes(x=temp_bribes, y=Freq, fill=Years)) +
                    geom_bar(stat="identity") +
                    facet_grid(Years~.) +
                    ylab("Frequency") + xlab("Events") + theme_bw()

plot.log_by <- ggplot(by_df, aes(x=temp_bribes, y=clog10(Freq), fill=Years)) +
                    geom_bar(stat="identity") +
                    facet_grid(Years~.) +
                    ylab("Frequency (log10 scale)") + xlab("Events") + theme_bw() #+
                    #scale_y_continuous(labels=c(0,10,100,1000,10000,100000))

# For quantiles2
by_df2 <- data.frame(bribes_years2)

plot.by2 <- ggplot(by_df2, aes(x=temp_bribes, y=Freq, fill=Years)) +
                  geom_bar(stat="identity") +
                  facet_grid(Years~.) +
                  ylab("Frequency") + xlab("Events") + theme_bw()

plot.log_by2 <- ggplot(by_df2, aes(x=temp_bribes, y=clog10(Freq), fill=Years)) +
                  geom_bar(stat="identity") +
                  facet_grid(Years~.) +
                  ylab("Frequency (log10 scale)") + xlab("Events") + theme_bw() #+
                  #scale_y_continuous(labels=c(0,10,100,1000,10000,100000))

# For raw years number
plot.bribes_years <- ggplot(enve_test, aes(x=years, y=bribes)) + geom_jitter() +
                          geom_smooth(method="lm") +
                          xlab("Years") + ylab("Extortions") + theme_bw()

# Save ggplots as images
ggsave(plot.by, file=paste(dir_name, "plot_by.pdf", sep=""), width=5, height=4)
ggsave(plot.log_by, file=paste(dir_name, "plot_log_by.pdf", sep=""), width=5, height=4)
ggsave(plot.by2, file=paste(dir_name, "plot_by.pdf", sep=""), width=5, height=4)
ggsave(plot.log_by2, file=paste(dir_name, "plot_log_by.pdf", sep=""), width=5, height=4)
ggsave(plot.bribes_years, file=paste(dir_name, "plot_bribes_years.pdf", sep=""), width=5, height=4)

ggsave(plot.by, file=paste(dir_name, "plot_by.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.log_by, file=paste(dir_name, "plot_log_by.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.by2, file=paste(dir_name, "plot_by.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.log_by2, file=paste(dir_name, "plot_log_by.png", sep=""), width=5, height=4, type="cairo-png")
ggsave(plot.bribes_years, file=paste(dir_name, "plot_bribes_years.png", sep=""), width=5, height=4, type="cairo-png")

##########################################################################
##########################################################################
##########################################################################
##########################################################################

## Sector v bribes

##########################################################################
##########################################################################
##########################################################################
##########################################################################

bribes_sector <- ftable(enve_test$sector, temp_bribes)

bribes_sector

xbribes_sector <- xtable(format(bribes_sector), caption="Bribery victimisations by sector", label="T_bribes_sector")

print(xbribes_sector, include.rownames=FALSE)

chisq.bribes_sector <- chisq.test(bribes_sector)
chisq.bribes_sector

cv.bribes_sector <- cv.test(bribes_sector)
cv.bribes_sector

##########################################################################
##########################################################################
##########################################################################
##########################################################################

## Subsector v bribes

##########################################################################
##########################################################################
##########################################################################
##########################################################################

bribes_subsector <- ftable(enve_test$subsector, temp_bribes)

bribes_subsector

xbribes_subsector <- xtable(format(bribes_subsector), caption="Bribery victimisations by subsector", label="T_bribes_subsector")

print(xbribes_subsector, include.rownames=FALSE)

chisq.bribes_subsector <- chisq.test(bribes_subsector)
chisq.bribes_subsector

chisq.bribes_subsector <- chisq.test(bribes_subsector, simulate.p.value=TRUE, B=9999)
chisq.bribes_subsector

cv.bribes_subsector<- cv.test(bribes_subsector)
cv.bribes_subsector

##########################################################################
##########################################################################
##########################################################################
##########################################################################

## restbar v bribes

##########################################################################
##########################################################################
##########################################################################
##########################################################################

bribes_restbar <- ftable(enve_test$restbar, temp_bribes)

bribes_restbar

xbribes_restbar <- xtable(format(bribes_restbar), caption="Bribery victimisations of rest bar vs collapsed subectors", label="T_bribes_restbar")

print(xbribes_restbar, include.rownames=FALSE)

chisq.bribes_restbar <- chisq.test(bribes_restbar)
chisq.bribes_restbar

chisq.bribes_restbar <- chisq.test(bribes_restbar, simulate.p.value=TRUE, B=9999)
chisq.bribes_restbar

cv.bribes_restbar<- cv.test(bribes_restbar)
cv.bribes_restbar

##########################################################################
##########################################################################
##########################################################################
##########################################################################

## Size v bribes

##########################################################################
##########################################################################
##########################################################################
##########################################################################

bribes_size <- ftable(enve_test$size, temp_bribes)

bribes_size

xbribes_size <- xtable(format(bribes_size), caption="Bribery victimisations by size", label="T_bribes_size")

print(xbribes_size, include.rownames=FALSE)

chisq.bribes_size <- chisq.test(bribes_size)
chisq.bribes_size

cv.bribes_size <- cv.test(bribes_size)
cv.bribes_size


##########################################################################
##########################################################################
##########################################################################
##########################################################################

## Modelling

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# First round, all models using subsector and yearsquant and tasahom

# 1. Poisson GLM
m1 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector + size, data=enve_test,
          family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m1)
confint(m1)
dev.stat(m1)

xm1 <- xtable(m1, caption="Poisson GLM", label="T_m1")

print(xm1)

# 2. NB GLM

m2 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector + size, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m2)
confint(m2)
dev.stat(m2)

xm2 <- xtable(m2, caption="Negative Binomial GLM", label="T_m2")

print(xm2)

m2.1 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m2.1)
confint(m2.1)
dev.stat(m2.1)

xm2.1 <- xtable(m2.1, caption="Negative Binomial GLM", label="T_m2.1")

print(xm2.1)

m2.2 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m2.2)
confint(m2.2)
dev.stat(m2.2)

xm2.2 <- xtable(m2.2, caption="Negative Binomial GLM", label="T_m2.2")

print(xm2.2)

m2.3 <- glmmadmb(extortions ~ bribes + tasahom, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m2.3)
confint(m2.3)
dev.stat(m2.3)

xm2.3 <- xtable(m2.3, caption="Negative Binomial GLM", label="T_m2.3")

print(xm2.3)

m2.4 <- glmmadmb(extortions ~ bribes, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m2.4)
confint(m2.4)
dev.stat(m2.4)

xm2.4 <- xtable(m2.4, caption="Negative Binomial GLM", label="T_m2.4")

print(xm2.4)

m2.null <- glmmadmb(extortions ~ 1, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m2.null)
confint(m2.null)
dev.stat(m2.null)

xm2.null <- xtable(m2.null, caption="Negative Binomial GLM", label="T_m2.null")

print(xm2.null)

## Compare different nb models

anova.m2 <- anova(m2)

anova.m2

xam2 <- xtable(anova.m2, caption="Analysis of deviance of Negative Binomial Model", label="T_am2")

print(xam2)

tx.m2.x <- texreg(list(m2.null, m2.4, m2.3, m2.2, m2.1, m2), caption="Comparison of all NB models",
                  label="T_tx_m2x", booktabs=TRUE)

tx.m2.x

anova.m2.x <- anova(m2.null, m2.4, m2.3, m2.2, m2.1, m2)

anova.m2.x

xam2.x <- xtable(anova.m2.x, caption="ANOVA test between variables of the NB models", label="T_xam2x")

# Compare NB and Poisson
tx.m1_m2 <- texreg(list(m1, m2), caption="Comparison of Poisson and NB GLMs", label="T_m1m2", booktabs=TRUE)

tx.m1_m2

lr.m1_m2 <- lrtest(m1, m2)

lr.m1_m2

xlr.m1_m2 <- xtable(lr.m1_m2, caption="Likelihood ratio test between Poisson and NB GLMs", label="T_lrm1m2")

print(xlr.m1_m2)

# 3. Poisson GLMM

m3 <- glmmadmb(extortions ~ bribes +  tasahom + yearsquant + subsector + size +
                (1 | NOM_ABR), data=enve_test, family="poisson",,
                family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m3)
confint(m3)
dev.stat(m3)

# 4. Negative Binomial GLMM

m4.0 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m4.0)
confint(m4.0)
dev.stat(m4.0)

anova.m4.0 <- anova(m4.0)

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

m4.1 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector +
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m4.1)
confint(m4.1)
dev.stat(m4.1)

m4.2 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m4.2)
confint(m4.2)
dev.stat(m4.2)

m4.3 <- glmmadmb(extortions ~ bribes + tasahom +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m4.3)
confint(m4.3)
dev.stat(m4.3)

m4.4 <- glmmadmb(extortions ~ bribes +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m4.4)
confint(m4.4)
dev.stat(m4.4)

m4.null <- glmmadmb(extortions ~ 1 +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(m4.null)
confint(m4.null)
dev.stat(m4.null)

# Compare all NB GLMMs
tx.m4.x <- texreg(list(m4.0, m4.1, m4.2, m4.3, m4.4, m4.null), caption="Comparison of all NB mixed models",
                  label="T_tx_m4x", booktabs=TRUE)

tx.m4.x

anova.m4.x <- anova(m4.null, m4.4, m4.3, m4.2, m4.1, m4.0)

anova.m4.x

xam4.x <- xtable(anova.m4.x, caption="ANOVA test between variables of the NB mixed models", label="T_xam4x")

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Second round using restbar

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# 1. Poisson GLM
n1 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + restbar + size, data=enve_test,
          family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n1)
confint(n1)
dev.stat(n1)

xn1 <- xtable(n1, caption="Poisson GLM", label="T_n1")

print(xn1)

# 2. NB GLM

n2 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + restbar + size, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n2)
confint(n2)
dev.stat(n2)

xn2 <- xtable(n2, caption="Negative Binomial GLM", label="T_n2")

print(xn2)

n2.1 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + restbar, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n2.1)
confint(n2.1)
dev.stat(n2.1)

xn2.1 <- xtable(n2.1, caption="Negative Binomial GLM", label="T_n2.1")

print(xn2.1)

n2.2 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n2.2)
confint(n2.2)
dev.stat(n2.2)

xn2.2 <- xtable(n2.2, caption="Negative Binomial GLM", label="T_n2.2")

print(xn2.2)

n2.3 <- glmmadmb(extortions ~ bribes + tasahom, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n2.3)
confint(n2.3)
dev.stat(n2.3)

xn2.3 <- xtable(n2.3, caption="Negative Binomial GLM", label="T_n2.3")

print(xn2.3)

n2.4 <- glmmadmb(extortions ~ bribes, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n2.4)
confint(n2.4)
dev.stat(n2.4)

xn2.4 <- xtable(n2.4, caption="Negative Binomial GLM", label="T_n2.4")

print(xn2.4)

n2.null <- glmmadmb(extortions ~ 1, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n2.null)
confint(n2.null)
dev.stat(n2.null)

xn2.null <- xtable(n2.null, caption="Negative Binomial GLM", label="T_n2.null")

print(xn2.null)

## Compare different nb models

anova.n2 <- anova(n2)

anova.n2

xan2 <- xtable(anova.n2, caption="Analysis of deviance of Negative Binomial Model", label="T_an2")

print(xan2)

tx.n2.x <- texreg(list(n2.null, n2.4, n2.3, n2.2, n2.1, n2), caption="Comparison of all NB models",
                  label="T_tx_n2x", booktabs=TRUE)

tx.n2.x

anova.n2.x <- anova(n2.null, n2.4, n2.3, n2.2, n2.1, n2)

anova.n2.x

xan2.x <- xtable(anova.n2.x, caption="ANOVA test between variables of the NB models", label="T_xan2x")

# Compare NB and Poisson
tx.n1_n2 <- texreg(list(n1, n2), caption="Comparison of Poisson and NB GLMs", label="T_n1n2", booktabs=TRUE)

tx.n1_n2

lr.n1_n2 <- lrtest(n1, n2)

lr.n1_n2

xlr.n1_n2 <- xtable(lr.n1_n2, caption="Likelihood ratio test between Poisson and NB GLMs", label="T_lrn1n2")

print(xlr.n1_n2)

# 3. Poisson GLMM

n3 <- glmmadmb(extortions ~ bribes +  tasahom + yearsquant + restbar + size +
                (1 | NOM_ABR), data=enve_test, family="poisson",,
                family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n3)
confint(n3)
dev.stat(n3)

# 4. Negative Binomial GLMM

n4.0 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + restbar + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n4.0)
confint(n4.0)
dev.stat(n4.0)

anova.n4.0 <- anova(n4.0)

anova.n4.0

xan4.0 <- xtable(anova.n4.0, caption="Analysis of deviance of Negative Binomial Mixed Model", label="T_an4.0")

print(xan4.0)

# Comparison between Poisson and NB GLMMs

tx.n3_n4 <- texreg(list(n3, n4.0), caption="Comparison of Poisson and NB Mixed Models", label="T_m3m4", booktabs=TRUE)

tx.n3_n4

lr.n3_n4 <- lrtest(n3, n4.0)

lr.n3_n4

xlr.n3_n4 <- xtable(lr.n3_n4, caption="Likelihood ratio test between Poisson and NB Mixed Models", label="T_lrn3n4")

print(xlr.n3_n4)

# 4.x Investigating different variables in the NB GLMMs

n4.1 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + restbar +
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n4.1)
confint(n4.1)
dev.stat(n4.1)

n4.2 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n4.2)
confint(n4.2)
dev.stat(n4.2)

n4.3 <- glmmadmb(extortions ~ bribes + tasahom +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n4.3)
confint(n4.3)
dev.stat(n4.3)

n4.4 <- glmmadmb(extortions ~ bribes +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n4.4)
confint(n4.4)
dev.stat(n4.4)

n4.null <- glmmadmb(extortions ~ 1 +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(n4.null)
confint(n4.null)
dev.stat(n4.null)

# Compare all NB GLMMs
tx.n4.x <- texreg(list(n4.0, n4.1, n4.2, n4.3, n4.4, n4.null), caption="Comparison of all NB mixed models",
                  label="T_tx_n4x", booktabs=TRUE)

tx.n4.x

anova.n4.x <- anova(n4.null, n4.4, n4.3, n4.2, n4.1, n4.0)

anova.n4.x

xan4.x <- xtable(anova.n4.x, caption="ANOVA test between variables of the NB mixed models", label="T_xam4x")

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Comparison between  round one and round two

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# GLM
tx.m2_n2 <- texreg(list(m2, n2), caption="NB Models using subsector and restbar covariates", label="T_m2n2")

tx.m2_n2

anova.m2_n2 <- anova(m2, n2)

anova.m2_n2

xam2n2 <- xtable(anova.m2_n2, caption="ANOVA between the NB models using subsector and restbar", label="T_xanm2n2")

print(xam2n2)

# GLMM
tx.m4_n4 <- texreg(list(m4.0, n4.0), caption="NB Mixed Models using subsector and restbar covariates", label="T_m4n4")

tx.m4_n4

anova.m4_n4 <- anova(m4.0, n4.0)

anova.m4_n4

xam4n4 <- xtable(anova.m4_n4, caption="ANOVA between the NB mixed models using sector and restbar", label="T_xanm4n4")

print(xam4n4)

# Compare NB GLM to NB GLMM

tx.m2_m4 <- texreg(list(m2, m4.0), caption="Comparison of NB GLM to Mixed Models", label="T_m2m4", booktabs=TRUE)

tx.m2_m4

lr.m2_m4 <- lrtest(m2, m4.0)

lr.m2_m4

xlr.m2_m4 <- xtable(lr.m2_m4, caption="Likelihood ratio test between NB GLM and Mixed Models", label="T_lrm2_m4")

print(xlr.m2_m4)

## from round 2

tx.n2_n4 <- texreg(list(n2, n4.0), caption="Comparison of NB GLM to Mixed Models", label="T_n2n4ADn4", booktabs=TRUE)

tx.n2_n4

lr.n2_n4 <- lrtest(n2, n4.0)

lr.n2_n4

xlr.n2_n4 <- xtable(lr.n2_n4, caption="Likelihood ratio test between NB GLM and Mixed Models", label="T_lrn2_n4")

print(xlr.n2_n4)

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# The 2 rounds but now using log of pop and murder raw

##########################################################################
##########################################################################
##########################################################################
##########################################################################


# First round, all models using subsector and yearsquant and denuncias and poblacion

# 1. Poisson GLM
o1 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + subsector + size, data=enve_test,
          family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o1)
confint(o1)
dev.stat(o1)

xo1 <- xtable(o1, caption="Poisson GLM", label="T_o1")

print(xo1)

# 2. NB GLM

o2 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + subsector + size, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o2)
confint(o2)
dev.stat(o2)

xo2 <- xtable(o2, caption="Negative Binomial GLM", label="T_o2")

print(xo2)

o2.1 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + subsector, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o2.1)
confint(o2.1)
dev.stat(o2.1)

xo2.1 <- xtable(o2.1, caption="Negative Binomial GLM", label="T_o2.1")

print(xo2.1)

o2.2 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o2.2)
confint(o2.2)
dev.stat(o2.2)

xo2.2 <- xtable(o2.2, caption="Negative Binomial GLM", label="T_o2.2")

print(xo2.2)

o2.3 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion), data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o2.3)
confint(o2.3)
dev.stat(o2.3)

xo2.3 <- xtable(o2.3, caption="Negative Binomial GLM", label="T_o2.3")

print(xo2.3)

o2.4 <- glmmadmb(extortions ~ bribes, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o2.4)
confint(o2.4)
dev.stat(o2.4)

xo2.4 <- xtable(o2.4, caption="Negative Binomial GLM", label="T_o2.4")

print(xo2.4)

o2.null <- glmmadmb(extortions ~ 1, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o2.null)
confint(o2.null)
dev.stat(o2.null)

xo2.null <- xtable(o2.null, caption="Negative Binomial GLM", label="T_o2.null")

print(xo2.null)

## Compare different nb models

anova.o2 <- anova(o2)

anova.o2

xao2 <- xtable(anova.o2, caption="Analysis of deviance of Negative Binomial Model", label="T_ao2")

print(xao2)

tx.o2.x <- texreg(list(o2.null, o2.4, o2.3, o2.2, o2.1, o2), caption="Comparison of all NB models",
                  label="T_tx_o2x", booktabs=TRUE)

tx.o2.x

anova.o2.x <- anova(o2.null, o2.4, o2.3, o2.2, o2.1, o2)

anova.o2.x

xao2.x <- xtable(anova.o2.x, caption="ANOVA test between variables of the NB models", label="T_xao2x")

# Compare NB and Poisson
tx.o1_o2 <- texreg(list(o1, o2), caption="Comparison of Poisson and NB GLMs", label="T_o1o2", booktabs=TRUE)

tx.o1_o2

lr.o1_o2 <- lrtest(o1, o2)

lr.o1_o2

xlr.o1_o2 <- xtable(lr.o1_o2, caption="Likelihood ratio test between Poisson and NB GLMs", label="T_lro1o2")

print(xlr.o1_o2)

# 3. Poisson GLMM

o3 <- glmmadmb(extortions ~ bribes +  log(denuncias_homs) + log(poblacion) + yearsquant + subsector + size +
                (1 | NOM_ABR), data=enve_test, family="poisson",,
                family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o3)
confint(o3)
dev.stat(o3)

# 4. Negative Binomial GLMM

o4.0 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o4.0)
confint(o4.0)
dev.stat(o4.0)

anova.o4.0 <- anova(o4.0)

anova.o4.0

xao4.0 <- xtable(anova.o4.0, caption="Analysis of deviance of Negative Binomial Mixed Model", label="T_ao4.0")

print(xao4.0)

# Comparison between Poisson and NB GLMMs

tx.o3_o4 <- texreg(list(o3, o4.0), caption="Comparison of Poisson and NB Mixed Models", label="T_o3o4", booktabs=TRUE)

tx.o3_o4

lr.o3_o4 <- lrtest(o3, o4.0)

lr.o3_o4

xlr.o3_o4 <- xtable(lr.o3_o4, caption="Likelihood ratio test between Poisson and NB Mixed Models", label="T_lro3o4")

print(xlr.o3_o4)

# 4.x Investigating different variables in the NB GLMMs

o4.1 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + subsector +
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o4.1)
confint(o4.1)
dev.stat(o4.1)

o4.2 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o4.2)
confint(o4.2)
dev.stat(o4.2)

o4.3 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o4.3)
confint(o4.3)
dev.stat(o4.3)

o4.4 <- glmmadmb(extortions ~ bribes +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o4.4)
confint(o4.4)
dev.stat(o4.4)

o4.null <- glmmadmb(extortions ~ 1 +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(o4.null)
confint(o4.null)
dev.stat(o4.null)

# Compare all NB GLMMs
tx.o4.x <- texreg(list(o4.0, o4.1, o4.2, o4.3, o4.4, o4.null), caption="Comparison of all NB mixed models",
                  label="T_tx_o4x", booktabs=TRUE)

tx.o4.x

anova.o4.x <- anova(o4.null, o4.4, o4.3, o4.2, o4.1, o4.0)

anova.o4.x

xao4.x <- xtable(anova.o4.x, caption="ANOVA test between variables of the NB mixed models", label="T_xao4x")

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Second round using restbar

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# 1. Poisson GLM
p1 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + restbar + size, data=enve_test,
          family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p1)
confint(p1)
dev.stat(p1)

xp1 <- xtable(p1, caption="Poisson GLM", label="T_p1")

print(xp1)

# 2. NB GLM

p2 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + restbar + size, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p2)
confint(p2)
dev.stat(p2)

xp2 <- xtable(p2, caption="Negative Binomial GLM", label="T_p2")

print(xp2)

p2.1 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + restbar, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p2.1)
confint(p2.1)
dev.stat(p2.1)

xp2.1 <- xtable(p2.1, caption="Negative Binomial GLM", label="T_p2.1")

print(xp2.1)

p2.2 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p2.2)
confint(p2.2)
dev.stat(p2.2)

xp2.2 <- xtable(p2.2, caption="Negative Binomial GLM", label="T_p2.2")

print(xp2.2)

p2.3 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion), data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p2.3)
confint(p2.3)
dev.stat(p2.3)

xp2.3 <- xtable(p2.3, caption="Negative Binomial GLM", label="T_p2.3")

print(xp2.3)

p2.4 <- glmmadmb(extortions ~ bribes, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p2.4)
confint(p2.4)
dev.stat(p2.4)

xp2.4 <- xtable(p2.4, caption="Negative Binomial GLM", label="T_p2.4")

print(xp2.4)

p2.null <- glmmadmb(extortions ~ 1, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p2.null)
confint(p2.null)
dev.stat(p2.null)

xp2.null <- xtable(p2.null, caption="Negative Binomial GLM", label="T_p2.null")

print(xp2.null)

## Compare different nb models

anova.p2 <- anova(p2)

anova.p2

xap2 <- xtable(anova.p2, caption="Analysis of deviance of Negative Binomial Model", label="T_ap2")

print(xap2)

tx.p2.x <- texreg(list(p2.pull, p2.4, p2.3, p2.2, p2.1, p2), caption="Comparison of all NB models",
                  label="T_tx_p2x", booktabs=TRUE)

tx.p2.x

anova.p2.x <- anova(p2.null, p2.4, p2.3, p2.2, p2.1, p2)

anova.p2.x

xap2.x <- xtable(anova.p2.x, caption="ANOVA test between variables of the NB models", label="T_xap2x")

# Compare NB and Poisson
tx.p1_p2 <- texreg(list(p1, p2), caption="Comparison of Poisson and NB GLMs", label="T_n1n2", booktabs=TRUE)

tx.p1_p2

lr.p1_p2 <- lrtest(p1, p2)

lr.p1_p2

xlr.p1_p2 <- xtable(lr.p1_p2, caption="Likelihood ratio test between Poisson and NB GLMs", label="T_lrp1p2")

print(xlr.p1_p2)

# 3. Poisson GLMM

p3 <- glmmadmb(extortions ~ bribes +  log(denuncias_homs) + log(poblacion) + yearsquant + restbar + size +
                (1 | NOM_ABR), data=enve_test, family="poisson",,
                family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p3)
confint(p3)
dev.stat(p3)

# 4. Negative Binomial GLMM

p4.0 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + restbar + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p4.0)
confint(p4.0)
dev.stat(p4.0)

anova.p4.0 <- anova(p4.0)

anova.p4.0

xap4.0 <- xtable(anova.p4.0, caption="Analysis of deviance of Negative Binomial Mixed Model", label="T_ap4.0")

print(xap4.0)

# Comparison between Poisson and NB GLMMs

tx.p3_p4 <- texreg(list(p3, p4.0), caption="Comparison of Poisson and NB Mixed Models", label="T_m3m4", booktabs=TRUE)

tx.p3_p4

lr.p3_p4 <- lrtest(p3, p4.0)

lr.p3_p4

xlr.p3_p4 <- xtable(lr.p3_p4, caption="Likelihood ratio test between Poisson and NB Mixed Models", label="T_lrp3p4")

print(xlr.p3_p4)

# 4.x Investigating different variables in the NB GLMMs

p4.1 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + restbar +
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p4.1)
confint(p4.1)
dev.stat(p4.1)

p4.2 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p4.2)
confint(p4.2)
dev.stat(p4.2)

p4.3 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p4.3)
confint(p4.3)
dev.stat(p4.3)

p4.4 <- glmmadmb(extortions ~ bribes +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p4.4)
confint(p4.4)
dev.stat(p4.4)

p4.null <- glmmadmb(extortions ~ 1 +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000")

summary(p4.null)
confint(p4.null)
dev.stat(p4.null)

# Compare all NB GLMMs
tx.p4.x <- texreg(list(p4.0, p4.1, p4.2, p4.3, p4.4, p4.pull), caption="Comparison of all NB mixed models",
                  label="T_tx_p4x", booktabs=TRUE)

tx.p4.x

anova.p4.x <- anova(p4.null, p4.4, p4.3, p4.2, p4.1, p4.0)

anova.p4.x

xap4.x <- xtable(anova.p4.x, caption="ANOVA test between variables of the NB mixed models", label="T_xam4x")

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Comparison between using murder rate and raw numbers

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# GLM
tx.m2_o2 <- texreg(list(m2, o2), caption="NB Models using murder rate or log covariates", label="T_m2o2")

tx.m2_o2

anova.m2_o2 <- anova(m2, o2)

anova.m2_o2

xam2o2 <- xtable(anova.m2_o2, caption="ANOVA between the NB models using murder rate or log", label="T_xanm2o2")

print(xam2o2)

# GLMM
tx.m4_o4 <- texreg(list(m4.0, o4.0), caption="NB Mixed Models using murder rate or log covariates", label="T_m4o4")

tx.m4_o4

anova.m4_o4 <- anova(m4.0, o4.0)

anova.m4_o4

xam4o4 <- xtable(anova.m4_o4, caption="ANOVA between the NB mixed models using murder rate or log", label="T_xanm4o4")

print(xam4o4)

# Compare NB GLM to NB GLMM

tx.o2_o4 <- texreg(list(o2, o4.0), caption="Comparison of NB GLM to Mixed Models", label="T_o2o4", booktabs=TRUE)

tx.o2_o4

lr.o2_o4 <- lrtest(o2, o4.0)

lr.o2_o4

xlr.o2_o4 <- xtable(lr.o2_o4, caption="Likelihood ratio test between NB GLM and Mixed Models", label="T_lro2_o4")

print(xlr.o2_o4)

## from round 2

tx.p2_p4 <- texreg(list(n2, n4.0), caption="Comparison of NB GLM to Mixed Models", label="T_n2n4ADn4", booktabs=TRUE)

tx.p2_p4

lr.p2_p4 <- lrtest(p2, p4.0)

lr.p2_p4

xlr.p2_p4 <- xtable(lr.p2_p4, caption="Likelihood ratio test between NB GLM and Mixed Models", label="T_lrp2_p4")

print(xlr.p2_p4)

## n v p
# GLM
tx.n2_p2 <- texreg(list(n2, p2), caption="NB Models using murder rate or log covariates", label="T_n2p2")

tx.n2_p2

anova.n2_p2 <- anova(n2, p2)

anova.n2_p2

xan2p2 <- xtable(anova.n2_p2, caption="ANOVA between the NB models using murder rate or log", label="T_xan2p2")

print(xan2p2)

# GLMM
tx.n4_p4 <- texreg(list(n4.0, p4.0), caption="NB Mixed Models using murder rate or log covariates", label="T_n4p4")

tx.n4_p4

anova.n4_p4 <- anova(n4.0, p4.0)

anova.n4_p4

xan4p4 <- xtable(anova.n4_p4, caption="ANOVA between the NB mixed models using murder rate or log", label="T_xan4p4")

print(xan4p4)

## o v p
# GLM
tx.o2_p2 <- texreg(list(o2, p2), caption="NB Models using murder rate or log covariates", label="T_o2p2")

tx.o2_p2

anova.o2_p2 <- anova(o2, p2)

anova.o2_p2

xao2p2 <- xtable(anova.o2_p2, caption="ANOVA between the NB models using murder rate or log", label="T_xao2p2")

print(xao2p2)

# GLMM
tx.o4_p4 <- texreg(list(o4.0, p4.0), caption="NB Mixed Models using murder rate or log covariates", label="T_o4p4")

tx.o4_p4

anova.o4_p4 <- anova(o4.0, p4.0)

anova.o4_p4

xao4p4 <- xtable(anova.o4_p4, caption="ANOVA between the NB mixed models using murder rate or log", label="T_xao4p4")

print(xao4p4)

##########################################################################
##########################################################################
##########################################################################
##########################################################################

############################## END OF FILE ###############################

##########################################################################
##########################################################################
##########################################################################
##########################################################################
