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

# 3. Poisson GLMM

m3 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
               family="poisson", zeroInflation=FALSE, extra.args="-ndi 60000",
               admb.opts = admbControl(noinit = FALSE))

summary(m3)
confint(m3)
dev.stat(m3)

m3.lme4 <- glmer(extortions ~ bribes + tasahom + yearsquant + subsector +
                size + (1 | NOM_ABR), data=enve_test,
               family="poisson")

summary(m3.lme4)
confint(m3.lme4)
# dev.stat(m3.lme4)

# 4. Negative Binomial GLMM

m4.0 <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(m4.0)
confint(m4.0)
dev.stat(m4.0)
# VIF
vif(m4.0)

m4.0.lm <- lm(extortions ~ bribes + tasahom + yearsquant + subsector +
                size, data=enve_test)

summary(m4.0.lm)
vif(m4.0.lm)

# Comparison between Poisson and NB GLMMs

tx.m3_m4 <- texreg(list(m3, m4.0), caption="Comparison of Poisson and NB Mixed Models", label="T_m3m4",
                   booktabs=TRUE)

tx.m3_m4

lr.m3_m4 <- lrtest(m3, m4.0)

lr.m3_m4

xlr.m3_m4 <- xtable(lr.m3_m4, caption="Likelihood ratio test between Poisson and NB Mixed Models",
                    label="T_lrm3m4")

print(xlr.m3_m4)

tx.m3lme4_m4 <- texreg(list(m3.lme4, m4.0), caption="Comparison of Poisson and NB Mixed Models", label="T_m3m4",
                   booktabs=TRUE)

tx.m3lme4_m4

lr.m3lme4_m4 <- lrtest(m3.lme4, m4.0)

lr.m3lme4_m4

xlr.m3lme4_m4 <- xtable(lr.m3lme4_m4, caption="Likelihood ratio test between Poisson and NB Mixed Models",
                    label="T_lrm3m4")

print(xlr.m3lme4_m4)

# 4.x Investigating different variables in the NB GLMMs

m4.1 <- glmmadmb(extortions ~  tasahom + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(m4.1)
confint(m4.1)
dev.stat(m4.1)

m4.2 <- glmmadmb(extortions ~ bribes + tasahom  + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(m4.2)
confint(m4.2)
dev.stat(m4.2)

m4.3 <- glmmadmb(extortions ~ bribes + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(m4.3)
confint(m4.3)
dev.stat(m4.3)

anova.m4.1 <- anova(m4.1, m4.0)

anova.m4.1

xam4.1 <- xtable(anova.m4.1, caption="ANOVA test between variables of the NB mixed models", label="T_xam4x")

print(xam4.1)

anova.m4.2 <- anova(m4.2, m4.0)

anova.m4.2

xam4.2 <- xtable(anova.m4.2, caption="ANOVA test between variables of the NB mixed models", label="T_xam4x")

print(xam4.2)

anova.m4.3 <- anova(m4.3, m4.0)

anova.m4.3

xam4.3 <- xtable(anova.m4.3, caption="ANOVA test between variables of the NB mixed models", label="T_xam4x")

print(xam4.3)


##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Test for Interactions

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Interactions
n4.int <- glmmadmb(extortions ~ bribes + tasahom + yearsquant + restbar
                + size + yearsquant:restbar + yearsquant:size +
                restbar:size + (1 | NOM_ABR),
                data=enve_test, family="nbinom", zeroInflation=FALSE,
                extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(n4.int)
confint(n4.int)
dev.stat(n4.int)
BIC(n4.int)
#vif
vif(n4.int)

n4.int.lm <- lm(extortions ~ bribes + tasahom + yearsquant + restbar
                + size + yearsquant:restbar + yearsquant:size + restbar:size, data=enve_test)

summary(n4.int.lm)
vif(n4.int.lm)


# Dropping yearsquant
n4.1.int <- glmmadmb(extortions ~ bribes + tasahom  + restbar
                + size + yearsquant:restbar + yearsquant:size +
                restbar:size + (1 | NOM_ABR),
                data=enve_test, family="nbinom", zeroInflation=FALSE,
                extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(n4.1.int)
confint(n4.1.int)
dev.stat(n4.1.int)
BIC(n4.1.int)
#vif
vif(n4.1.int)

anova.n4.1.int <- anova(n4.1.int, n4.int)

anova.n4.1.int

xan4.1.int <- xtable(anova.n4.1.int, caption="ANOVA test dropping yearsquant of the NB mixed models with interactions", label="T_xan41int")

print(xan4.1.int)

## lr test for likelihood ratio for interactions full model
lr.n4.int_m4 <- lrtest(n4.int, m4.0)

lr.n4.int_m4

xlr.n4.int_m4 <- xtable(lr.n4.int_m4, caption="Likelihood ratio test between Interactions and no interactions",
                    label="T_lrm3m4")

print(xlr.n4.int_m4)


##########################################################################
##########################################################################
##########################################################################
##########################################################################

# using log of pop and murder raw

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# 3. Poisson GLMM

o3 <- glmmadmb(extortions ~ bribes +  log(denuncias_homs) + log(poblacion) + yearsquant + subsector + size +
                (1 | NOM_ABR), data=enve_test, family="poisson",
               zeroInflation=FALSE, extra.args="-ndi 60000",
               admb.opts = admbControl(noinit = FALSE))

summary(o3)
confint(o3)
dev.stat(o3)

o3.lme4 <- glmer(extortions ~ bribes +  log(denuncias_homs) + log(poblacion) + yearsquant + subsector + size +
                (1 | NOM_ABR), data=enve_test, family="poisson")

summary(o3.lme4)
confint(o3.lme4)
# dev.stat(o3.lme4)


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

lr.o3.lme4_o4 <- lrtest(o3.lme4, o4.0)

lr.o3.lme4_o4

xlr.o3.lme4_o4 <- xtable(lr.o3.lme4_o4, caption="Likelihood ratio test between Poisson and NB Mixed Models", label="T_lro3o4")

print(lr.o3.lme4_o4)


# 4.x Investigating different variables in the NB GLMMs

o4.1 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + yearsquant + subsector + size +
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(o4.1)
confint(o4.1)
dev.stat(o4.1)

anova.o4.1 <- anova(o4.1, o4.0)
anova.o4.1

o4.2 <- glmmadmb(extortions ~ bribes + log(poblacion) + yearsquant + subsector + size +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(o4.2)
confint(o4.2)
dev.stat(o4.2)

anova.o4.2 <- anova(o4.2, o4.0)
anova.o4.2


o4.3.a <- glmmadmb(extortions ~ bribes + log(denuncias_homs) +
                   (1 | NOM_ABR), data=enve_test,
                   family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(o4.3.a)
confint(o4.3.a)
dev.stat(o4.3.a)

o4.nb <- glmmadmb(extortions ~  log(denuncias_homs) + log(poblacion) + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(o4.nb)
confint(o4.nb)
dev.stat(o4.nb)


##########################################################################
##########################################################################
##########################################################################
##########################################################################

# Second round using restbar ## Aqui me quede Use aereal bribery now

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
