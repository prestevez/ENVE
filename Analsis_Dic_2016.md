---
title: "Repeat extortion of Mexican businesses"
author: "Patricio R. Estevez Soto"
email: "patricio.estevez.14@ucl.ac.uk"
date: "6/12/2016"
output:
  md_document:
    variant: "markdown"
pandoc_args: "--smart"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,
                      comment="",
                      cache=TRUE,
                      dev=c("png", "CairoPDF"),
                      error=TRUE)
```

# Introduction

Script to analyze patterns of extortion victimization against Mexican businesses.

# Set up, data input and pre-process

## Session info

We first check details of the session and system, and for reproducibility, we set the random seed.

```{r session, cache=FALSE}
starttime <- proc.time()
date()
sessionInfo()
set.seed(42)
options(scipen=0)
```

## Load packages and functions

Next we load the packages that we will use.

```{r packages}
library(foreign)
library(ggplot2)
library(Cairo)
library(knitr)
library(car)
library(texreg)
library(lmtest)
library(MASS)
library(lme4)
library(glmmADMB)
library(classInt)
library(dgof)
```

## Load data

```{r victim-level}
enve_all <- read.dbf("enve2014cuest_ciega_2014.dbf")
cat_entidades <- read.csv("cat_entidades.csv", head=TRUE)
homicidios <- read.csv("homicidios_values.csv", header=TRUE)
homicidios <- merge(homicidios, cat_entidades, by="CVE_ENT", all.x=TRUE)
scode <- read.csv("secode.csv", head=TRUE)
scode$Code <- scode$Code*10000

# Prepare data for analysis
# Selecting only the relevant variables

enve_test <- data.frame(extortions=as.integer(as.character(enve_all$P26_10)))

enve_test$extortion_victim <- enve_all$P25_10
enve_test$extortions[enve_test$extortion_victim == 2] <- 0
summary(enve_test$extortions)
table(enve_test$extortions)

enve_test$rep_extortion_victim <- factor(enve_test$extortions)
levels(enve_test$rep_extortion_victim) <- c(0, 0,
                    rep(1, length(levels(enve_test$rep_extortion_victim)) - 2))

summary(enve_test$rep_extortion_victim)


enve_test$CVE_UNICA <- as.integer(as.character(enve_all$ID_CONSECU))

enve_test$bribes <- as.integer(as.character(enve_all$P33))
summary(enve_test$bribes)

# 4 bribe cats
enve_test$bribe1 <- enve_all$P29_1
enve_test$bribe2 <- enve_all$P30_1
enve_test$bribe3 <- enve_all$P31_1
enve_test$bribe4 <- enve_all$P32_1

enve_test$bribes[with(enve_test,
                        bribe1 == 2 &
                        bribe2 == 2 &
                        bribe3 == 2 &
                        bribe4 == 2)] <- 0

summary(enve_test$bribes)

enve_test$bribe_victim <- factor(enve_test$bribes)
levels(enve_test$bribe_victim) <- c(0,
                                    rep(1, length(levels(enve_test$bribe_victim)) - 1))
summary(enve_test$bribe_victim)

enve_test$rep_bribe <- factor(enve_test$bribes)
levels(enve_test$rep_bribe) <- c(0, 0, rep(1,
                                           length(levels(enve_test$rep_bribe)) - 2))
summary(enve_test$rep_bribe)

enve_test$bribe_cats <- factor(enve_test$bribes)
levels(enve_test$bribe_cats) <- c(0, 1, 2, rep("3+",
                                            length(levels(enve_test$bribe_cats)) - 3))
summary(enve_test$bribe_cats)

enve_test$CVE_ENT <- as.integer(as.character(enve_all$CVE_ENT))

enve_test$size <- enve_all$ID_ESTRATO
levels(enve_test$size) <- c("Large", "Medium", "Small", "Micro")

enve_test$sector <- enve_all$SECTOR_FIN

# subsector
enve_test$tempsub <- as.integer(as.character(enve_all$P1_1B))
enve_test$subsector <- cut(enve_test$tempsub, scode$Code, right=FALSE)
levels(enve_test$subsector) <- scode$Sector
enve_test$subsector <- droplevels(enve_test$subsector)
enve_test$subsector <- relevel(enve_test$subsector, ref="Retail")
levels(enve_test$subsector)


enve_test$hotrestbar <- enve_test$subsector
hotindx <- which(levels(enve_test$hotrestbar) == "HotelsRestBar")
levels(enve_test$hotrestbar)[-hotindx] <- 0
levels(enve_test$hotrestbar) <- c(0,1)

enve_test$years <- 2013 - as.numeric(as.character(enve_all$P3))
intyears <- classIntervals(enve_test$years, 5, style="quantile")
enve_test$yearsquant <- cut(enve_test$years, intyears$brks, right=TRUE,
                            include.lowest = TRUE)

enve_test <- merge(enve_test, homicidios, by="CVE_ENT", all.x=TRUE)

length(enve_test$extortions[is.na(enve_test$extortions)])
length(enve_test$bribes[is.na(enve_test$bribes)])

## enve_test$extortions[is.na(enve_test$extortions)] <- 0
## enve_test$bribes[is.na(enve_test$bribes)] <- 0


```

# EDA

## Extoriton EDA

```{r EDA-extortions}
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

kable(ext_dist)

```

## Bribes EDA

```{r EDA-bribes}
bribes_tab <- data.frame(table(enve_test$bribes))
bribes <- data.frame(Events=bribes_tab$Var1, Freq=bribes_tab$Freq)
colnames(bribes)[1] <- "Events"
bribes$Events <- as.integer(as.character(bribes$Events))
obs_b <- data.frame(Events=0:max(bribes$Events))
bribes <- merge(bribes, obs_b, by="Events", all.y=TRUE)
bribes[is.na(bribes[,2]),2] <- 0

bribes

kable(bribes)

```

## Years vs. extortions plot

```{r years}
### using the raw years variable

cor.ext_years <- with(enve_test, cor.test(extortions, years, method="pearson"))
cor.ext_years

# For raw years number
ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth(method="lm") +
                        xlab("Years") +
                        ylab("Extortions") +
                        theme_bw()

ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth(method="lm") +
                        xlab("Years (sqrt scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="sqrt")


ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth(method="lm") +
                        xlab("Years (log scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="log1p")

ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        xlab("Years") +
                        ylab("Extortions") +
                        theme_bw() +
                        geom_density_2d()

ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        xlab("Years (sqrt scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="sqrt") +
                        geom_density_2d()


ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        xlab("Years (log scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="log1p") +
                        geom_density_2d()

ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth() +
                        xlab("Years") +
                        ylab("Extortions") +
                        theme_bw()

ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth() +
                        xlab("Years (sqrt scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="sqrt")


ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth() +
                        xlab("Years (log scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="log1p")

ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth(method="loess") +
                        xlab("Years") +
                        ylab("Extortions") +
                        theme_bw()

ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth(method="loess") +
                        xlab("Years (sqrt scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="sqrt")


ggplot(enve_test, aes(x=years, y=extortions)) +
                        geom_point() +
                        geom_smooth(method="loess") +
                        xlab("Years (log scale)") +
                        ylab("Extortions") +
                        theme_bw() +
                        coord_trans(x="log1p")
```


# Model

```{r model}

# Negative Binomial GLMM

m <- glmmadmb(extortions ~ bribes + tasahom + yearsquant +
                subsector + size + (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE,
                 extra.args="-ndi 60000", admb.opts = admbControl(noinit = FALSE))

summary(m)

```

Using lme4 instead of glmmadmb

```{r model-lme4}

m_lme4 <- glmer.nb(extortions ~ bribes + tasahom + yearsquant +
                subsector + size + (1 | NOM_ABR), data=enve_test)

summary(m_lme4)

screenreg(list(m,m_lme4))
```

# Benchmark stats

```{r timing, cache=FALSE}
endtime <- proc.time()
time <- endtime - starttime
time

print(paste("the script took", round(time[3]/60,2),
              "minutes to run.", sep=" "))
```
