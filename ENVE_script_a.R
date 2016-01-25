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
enve_test$subsector <- droplevels(enve_test$subsector)
enve_test$subsector <- relevel(enve_test$subsector, ref="Retail")
levels(enve_test$subsector)

enve_test$restbar <- enve_test$subsector
hotindx <- which(levels(enve_test$restbar) == "HotelsRestBar")
levels(enve_test$restbar)[-hotindx] <- "NotRestBar"
enve_test$restbar <- relevel(enve_test$restbar, ref="NotRestBar")
levels(enve_test$restbar)

enve_test$years <- 2013 - as.numeric(as.character(enve_all$P3))

intyears <- classIntervals(enve_test$years, 5, style="quantile")
intyears$brks

enve_test$yearsquant <- cut(enve_test$years, intyears$brks, right=TRUE, include.lowest = TRUE)

intyears2 <- classIntervals(enve_test$years, 6, style="quantile")
intyears2$brks

enve_test$yearsquant2 <- cut(enve_test$years, intyears2$brks, right=TRUE, include.lowest = TRUE)


enve_test <- merge(enve_test, homicidios, by="CVE_ENT", all.x=TRUE)

enve_test$extortions[is.na(enve_test$extortions)] <- 0
enve_test$bribes[is.na(enve_test$bribes)] <- 0

head(enve_test, 25)

tail(enve_test, 25)


#################### aereal exploration of bribery ######################
##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Area-level influences: bribes vs extortion

##########################################################################
##########################################################################
##########################################################################
##########################################################################

plot.extinc_brinc <- ggplot(homicidios, aes(x=bribes_inci, y=Incidence)) +
                          geom_point() + geom_smooth(method="lm")  + xlab("Bribes Incidence") +
                          theme_bw()

cor.extinc_brinc <- with(state_summ1, cor.test(`Bribes Inci.`, tasahom))

cor.extinc_brinc

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
ggsave(plot.raw_bcon, file=paste(dir_name, "plot_raw_bcon.png", sep=""), width=5, height=4, type="cairo-png")

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

plot.logpop_bcon <- ggplot(state_summ1, aes(x=log(poblacion), y=`Bribes Conc.`)) +
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

chisq.bribes_years <- chisq.test(bribes_years)
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
                    ylab("Frequency (log10 scale)") + xlab("Events") + theme_bw() +
                    scale_y_continuous(limits=c(clog10(0), clog10(100000)), labels=c(0,10,100,1000,10000,100000))

# For quantiles2
by_df2 <- data.frame(bribes_years2)

plot.by2 <- ggplot(by_df2, aes(x=temp_bribes, y=Freq, fill=Years)) +
                  geom_bar(stat="identity") +
                  facet_grid(Years~.) +
                  ylab("Frequency") + xlab("Events") + theme_bw()

plot.log_by2 <- ggplot(by_df2, aes(x=temp_bribes, y=clog10(Freq), fill=Years)) +
                  geom_bar(stat="identity") +
                  facet_grid(Years~.) +
                  ylab("Frequency (log10 scale)") + xlab("Events") + theme_bw() +
                  scale_y_continuous(limits=c(clog10(0), clog10(100000)), labels=c(0,10,100,1000,10000,100000))

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

xbribes_restbar <- xtable(format(bribes_restbar), caption="Bribery victimisations of rest bar vs collapsed subectors",
                          label="T_bribes_restbar")

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
          family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(m1)
confint(m1)
dev.stat(m1)


# 2. NB GLM

m2 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector + size, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(m2)
confint(m2)
dev.stat(m2)

m2.1 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(m2.1)
confint(m2.1)
dev.stat(m2.1)

m2.2 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(m2.2)
confint(m2.2)
dev.stat(m2.2)

m2.3 <- glmmadmb(extortions ~ bribes + tasahom, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(m2.3)
confint(m2.3)
dev.stat(m2.3)

m2.4 <- glmmadmb(extortions ~ bribes, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(m2.4)
confint(m2.4)
dev.stat(m2.4)

m2.null <- glmmadmb(extortions ~ 1, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(m2.null)
confint(m2.null)
dev.stat(m2.null)

## Compare different nb models

tx.m2.x <- texreg(list(m2.null, m2.4, m2.3, m2.2, m2.1, m2), caption="Comparison of all NB models",
                  label="T_tx_m2x", booktabs=TRUE)

tx.m2.x

anova.m2.x <- anova(m2.null, m2.4, m2.3, m2.2, m2.1, m2)

anova.m2.x

xam2.x <- xtable(anova.m2.x, caption="ANOVA test between variables of the NB models", label="T_xam2x")
print(xam2.x)

# Compare NB and Poisson
tx.m1_m2 <- texreg(list(m1, m2), caption="Comparison of Poisson and NB GLMs", label="T_m1m2", booktabs=TRUE)

tx.m1_m2

lr.m1_m2 <- lrtest(m1, m2)

lr.m1_m2

xlr.m1_m2 <- xtable(lr.m1_m2, caption="Likelihood ratio test between Poisson and NB GLMs", label="T_lrm1m2")

print(xlr.m1_m2)

# 3. Poisson GLMM

m3 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
               family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000",
               admb.opts = admbControl(noinit = FALSE))

summary(m3)
confint(m3)
dev.stat(m3)

# 4. Negative Binomial GLMM

m4.0 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(m4.0)
confint(m4.0)
dev.stat(m4.0)

# Comparison between Poisson and NB GLMMs

tx.m3_m4 <- texreg(list(m3, m4.0), caption="Comparison of Poisson and NB Mixed Models", label="T_m3m4",
                   booktabs=TRUE)

tx.m3_m4

lr.m3_m4 <- lrtest(m3, m4.0)

lr.m3_m4

xlr.m3_m4 <- xtable(lr.m3_m4, caption="Likelihood ratio test between Poisson and NB Mixed Models",
                    label="T_lrm3m4")

print(xlr.m3_m4)

# 4.x Investigating different variables in the NB GLMMs

m4.1 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector +
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(m4.1)
confint(m4.1)
dev.stat(m4.1)

m4.2 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(m4.2)
confint(m4.2)
dev.stat(m4.2)

m4.3 <- glmmadmb(extortions ~ bribes + tasahom +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(m4.3)
confint(m4.3)
dev.stat(m4.3)

m4.4 <- glmmadmb(extortions ~ bribes +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(m4.4)
confint(m4.4)
dev.stat(m4.4)

m4.null <- glmmadmb(extortions ~ 1 +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                   admb.opts = admbControl(noinit = FALSE))

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
          family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(n1)
confint(n1)
dev.stat(n1)

# 2. NB GLM

n2 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + restbar + size, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(n2)
confint(n2)
dev.stat(n2)

# Interaction effect
n2.int <- glmmadmb(extortions ~ bribes + tasahom + (yearsquant + restbar) * size + yearsquant:restbar, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(n2.int)
confint(n2.int)
dev.stat(n2.int)

# End interaction
n2.1 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + restbar, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(n2.1)
confint(n2.1)
dev.stat(n2.1)


n2.2 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(n2.2)
confint(n2.2)
dev.stat(n2.2)


n2.3 <- glmmadmb(extortions ~ bribes + tasahom, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(n2.3)
confint(n2.3)
dev.stat(n2.3)


n2.4 <- glmmadmb(extortions ~ bribes, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(n2.4)
confint(n2.4)
dev.stat(n2.4)

n2.null <- glmmadmb(extortions ~ 1, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(n2.null)
confint(n2.null)
dev.stat(n2.null)

## Compare different nb models

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
                (1 | NOM_ABR), data=enve_test, family="poisson",
               zeroInflation=FALSE, extra.args="-ndi 60000",
               admb.opts = admbControl(noinit = FALSE))

summary(n3)
confint(n3)
dev.stat(n3)

# 4. Negative Binomial GLMM

n4.0 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + restbar + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(n4.0)
confint(n4.0)
dev.stat(n4.0)

# Interactions
n4.int <- glmmadmb(extortions ~ bribes + tasahom + (yearsquant + restbar) * size + yearsquant:restbar +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(n4.int)
confint(n4.int)
dev.stat(n4.int)


# Comparison between Poisson and NB GLMMs

tx.n3_n4 <- texreg(list(n3, n4.0), caption="Comparison of Poisson and NB Mixed Models", label="T_m3m4",
                   booktabs=TRUE)

tx.n3_n4

lr.n3_n4 <- lrtest(n3, n4.0)

lr.n3_n4

xlr.n3_n4 <- xtable(lr.n3_n4, caption="Likelihood ratio test between Poisson and NB Mixed Models",
                    label="T_lrn3n4")

print(xlr.n3_n4)

# 4.x Investigating different variables in the NB GLMMs

n4.1 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + restbar +
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(n4.1)
confint(n4.1)
dev.stat(n4.1)

n4.2 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(n4.2)
confint(n4.2)
dev.stat(n4.2)

n4.3 <- glmmadmb(extortions ~ bribes + tasahom +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(n4.3)
confint(n4.3)
dev.stat(n4.3)

n4.4 <- glmmadmb(extortions ~ bribes +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(n4.4)
confint(n4.4)
dev.stat(n4.4)

n4.null <- glmmadmb(extortions ~ 1 +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                   admb.opts = admbControl(noinit = FALSE))

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

xam2n2 <- xtable(anova.m2_n2, caption="ANOVA between the NB models using subsector and restbar",
                 label="T_xanm2n2")

print(xam2n2)

# GLMM
tx.m4_n4 <- texreg(list(m4.0, n4.0), caption="NB Mixed Models using subsector and restbar covariates",
                   label="T_m4n4")

tx.m4_n4

anova.m4_n4 <- anova(m4.0, n4.0)

anova.m4_n4

xam4n4 <- xtable(anova.m4_n4, caption="ANOVA between the NB mixed models using sector and restbar",
                 label="T_xanm4n4")

print(xam4n4)

# Compare NB GLM to NB GLMM

tx.m2_m4 <- texreg(list(m2, m4.0), caption="Comparison of NB GLM to Mixed Models", label="T_m2m4",
                   booktabs=TRUE)

tx.m2_m4

lr.m2_m4 <- lrtest(m2, m4.0)

lr.m2_m4

xlr.m2_m4 <- xtable(lr.m2_m4, caption="Likelihood ratio test between NB GLM and Mixed Models", label="T_lrm2_m4")

print(xlr.m2_m4)

## from round 2

tx.n2_n4 <- texreg(list(n2, n4.0), caption="Comparison of NB GLM to Mixed Models", label="T_n2n4ADn4",
                   booktabs=TRUE)

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
o1 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + subsector + size,
               data=enve_test,
          family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(o1)
confint(o1)
dev.stat(o1)

# 2. NB GLM

o2 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + subsector + size,
               data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(o2)
confint(o2)
dev.stat(o2)


o2.1 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + subsector, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(o2.1)
confint(o2.1)
dev.stat(o2.1)

o2.2 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(o2.2)
confint(o2.2)
dev.stat(o2.2)

o2.3 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion), data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(o2.3)
confint(o2.3)
dev.stat(o2.3)


o2.4 <- glmmadmb(extortions ~ bribes, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(o2.4)
confint(o2.4)
dev.stat(o2.4)

o2.null <- glmmadmb(extortions ~ 1, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(o2.null)
confint(o2.null)
dev.stat(o2.null)


## Compare different nb models


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
                (1 | NOM_ABR), data=enve_test, family="poisson",
               zeroInflation=FALSE, extra.args="-ndi 60000",
               admb.opts = admbControl(noinit = FALSE))

summary(o3)
confint(o3)
dev.stat(o3)

# 4. Negative Binomial GLMM

o4.0 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(o4.0)
confint(o4.0)
dev.stat(o4.0)


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
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(o4.1)
confint(o4.1)
dev.stat(o4.1)

o4.2 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(o4.2)
confint(o4.2)
dev.stat(o4.2)

o4.3 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(o4.3)
confint(o4.3)
dev.stat(o4.3)

o4.4 <- glmmadmb(extortions ~ bribes +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(o4.4)
confint(o4.4)
dev.stat(o4.4)

o4.null <- glmmadmb(extortions ~ 1 +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                   admb.opts = admbControl(noinit = FALSE))

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
          family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(p1)
confint(p1)
dev.stat(p1)

# 2. NB GLM

p2 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + restbar + size, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(p2)
confint(p2)
dev.stat(p2)

p2.1 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + restbar, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(p2.1)
confint(p2.1)
dev.stat(p2.1)


p2.2 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(p2.2)
confint(p2.2)
dev.stat(p2.2)

p2.3 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion), data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(p2.3)
confint(p2.3)
dev.stat(p2.3)


p2.4 <- glmmadmb(extortions ~ bribes, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(p2.4)
confint(p2.4)
dev.stat(p2.4)

p2.null <- glmmadmb(extortions ~ 1, data=enve_test,
          family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
          admb.opts = admbControl(noinit = FALSE))

summary(p2.null)
confint(p2.null)
dev.stat(p2.null)

## Compare different nb models

tx.p2.x <- texreg(list(p2.null, p2.4, p2.3, p2.2, p2.1, p2), caption="Comparison of all NB models",
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
                (1 | NOM_ABR), data=enve_test, family="poisson",
               zeroInflation=FALSE, extra.args="-ndi 60000",
               admb.opts = admbControl(noinit = FALSE))

summary(p3)
confint(p3)
dev.stat(p3)

# 4. Negative Binomial GLMM

p4.0 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + restbar + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.0)
confint(p4.0)
dev.stat(p4.0)

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
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.1)
confint(p4.1)
dev.stat(p4.1)

p4.2 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.2)
confint(p4.2)
dev.stat(p4.2)

p4.3 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.3)
confint(p4.3)
dev.stat(p4.3)

p4.4 <- glmmadmb(extortions ~ bribes +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.4)
confint(p4.4)
dev.stat(p4.4)

p4.null <- glmmadmb(extortions ~ 1 +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                   admb.opts = admbControl(noinit = FALSE))

summary(p4.null)
confint(p4.null)
dev.stat(p4.null)

# Compare all NB GLMMs
tx.p4.x <- texreg(list(p4.0, p4.1, p4.2, p4.3, p4.4, p4.null), caption="Comparison of all NB mixed models",
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
