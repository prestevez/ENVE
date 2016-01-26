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


m4.1 <- glmmadmb(extortions ~  tasahom + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(m4.1)
confint(m4.1)
dev.stat(m4.1)


# 4. Negative Binomial GLMM

o4.0 <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(o4.0)
confint(o4.0)
dev.stat(o4.0)
vif(o4.0)


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

# Models exploring aereal bribery

##########################################################################
##########################################################################
##########################################################################
##########################################################################

# 3. Poisson GLMM

p3.a <- glmmadmb(extortions ~ bribes +  tasahom + yearsquant + restbar + size + bribes_preval +
                (1 | NOM_ABR), data=enve_test, family="poisson",
               zeroInflation=FALSE, extra.args="-ndi 60000",
               admb.opts = admbControl(noinit = FALSE))

summary(p3.a)
confint(p3.a)
dev.stat(p3.a)

p3.b <- glmmadmb(extortions ~ bribes +  tasahom + yearsquant + restbar + size + bribes_inci +
                (1 | NOM_ABR), data=enve_test, family="poisson",
               zeroInflation=FALSE, extra.args="-ndi 60000",
               admb.opts = admbControl(noinit = FALSE))

summary(p3.b)
confint(p3.b)
dev.stat(p3.b)

p3.c <- glmmadmb(extortions ~ bribes +  tasahom + yearsquant + restbar + size + bribes_conc +
                (1 | NOM_ABR), data=enve_test, family="poisson",
               zeroInflation=FALSE, extra.args="-ndi 60000",
               admb.opts = admbControl(noinit = FALSE))

summary(p3.c)
confint(p3.c)
dev.stat(p3.c)

# 4. Negative Binomial GLMM

p4.a <- glmmadmb(extortions ~ bribes +  tasahom + yearsquant + restbar + size + bribes_preval +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.a)
confint(p4.a)
dev.stat(p4.a)
vif(p4.a)

p4.b <- glmmadmb(extortions ~ bribes +  tasahom + yearsquant + restbar + size + bribes_inci +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.b)
confint(p4.b)
dev.stat(p4.b)
vif(p4.b)

p4.c <- glmmadmb(extortions ~ bribes +  tasahom + yearsquant + restbar + size + bribes_conc +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.c)
confint(p4.c)
dev.stat(p4.c)
vif(p4.c)

# Comparison between Poisson and NB GLMMs

tx.p3_p4a <- texreg(list(p3.a, p4.a), caption="Comparison of Poisson and NB Mixed Models", label="T_m3m4", booktabs=TRUE)

tx.p3_p4a

lr.p3_p4a <- lrtest(p3.a, p4.a)

lr.p3_p4a

xlr.p3_p4a <- xtable(lr.p3_p4a, caption="Likelihood ratio test between Poisson and NB Mixed Models", label="T_lrp3p4")

print(xlr.p3_p4a)

tx.p3_p4b <- texreg(list(p3.b, p4.b), caption="Comparison of Poisson and NB Mixed Models", label="T_m3m4", booktabs=TRUE)

tx.p3_p4b

lr.p3_p4b <- lrtest(p3.b, p4.b)

lr.p3_p4b

xlr.p3_p4b <- xtable(lr.p3_p4b, caption="Likelihood ratio test between Poisson and NB Mixed Models", label="T_lrp3p4")

print(xlr.p3_p4b)

tx.p3_p4c <- texreg(list(p3.c, p4.c), caption="Comparison of Poisson and NB Mixed Models", label="T_m3m4", booktabs=TRUE)

tx.p3_p4c

lr.p3_p4c <- lrtest(p3.c, p4.c)

lr.p3_p4c

xlr.p3_p4c <- xtable(lr.p3_p4c, caption="Likelihood ratio test between Poisson and NB Mixed Models", label="T_lrp3p4")

print(xlr.p3_p4c)


# compare nb using aereal bribery

lr.p4a_p4b <- lrtest(p4.a, p4.b)

lr.p4a_p4b

lr.p4a_p4c <- lrtest(p4.a, p4.c)

lr.p4a_p4c

lr.p4c_p4b <- lrtest(p4.c, p4.b)

lr.p4c_p4b

## Drop 1 level bribery for each

p4.a1 <- glmmadmb(extortions ~ tasahom + yearsquant + restbar + size + bribes_preval +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.a1)
confint(p4.a1)
dev.stat(p4.a1)
vif(p4.a1)

p4.b1 <- glmmadmb(extortions ~ tasahom + yearsquant + restbar + size + bribes_inci +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.b1)
confint(p4.b1)
dev.stat(p4.b1)
vif(p4.b1)

p4.c1 <- glmmadmb(extortions ~ tasahom + yearsquant + restbar + size + bribes_conc +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.c1)
confint(p4.c1)
dev.stat(p4.c1)
vif(p4.c1)

### Compare these with the model without 1 and 2 level bribery (m4.1)

lr.p4a1_m41 <- lrtest(p4.a1, m4.1)

lr.p4a1_m41

lr.p4b1_m41 <- lrtest(p4.b1, m4.1)

lr.p4b1_m41

lr.p4c1_m41 <- lrtest(p4.c1, m4.1)

lr.p4c1_m41

# compare with themselves

lr.p4a1_p4b1 <- lrtest(p4.a1, p4.b1)

lr.p4a1_p4b1

lr.p4a1_p4c1 <- lrtest(p4.a1, p4.c1)

lr.p4a1_p4c1

lr.p4c1_p4b1 <- lrtest(p4.c1, p4.b1)

lr.p4c1_p4b1

## Compare with full model with one level bribery

lr.p4a1_m40 <- lrtest(p4.a1, m4.0)

lr.p4a1_m40

lr.p4b1_m40 <- lrtest(p4.b1, m4.0)

lr.p4b1_m40

lr.p4c1_m40 <- lrtest(p4.c1, m4.0)

lr.p4c1_m40


## Now with raw state bribery aggregate

p4.d <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + restbar + size + bribes_abvic
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.d)
confint(p4.d)
dev.stat(p4.d)
vif(p4.d)

p4.e <- glmmadmb(extortions ~ bribes + log(denuncias_homs) + log(poblacion) + yearsquant + restbar + size + bribes_abincs
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.e)
confint(p4.e)
dev.stat(p4.e)
vif(p4.e)

# compare the two

lr.p4d_p4e <- lrtest(p4.e, p4.d)

lr.p4d_p4e

# compare with bribery rates model

lr.p4d_p4a <- lrtest(p4.a, p4.d)

lr.p4d_p4a

lr.p4e_p4b <- lrtest(p4.b, p4.e)

lr.p4e_p4b

## Drop one level bribery

p4.d1 <- glmmadmb(extortions ~ log(denuncias_homs) + log(poblacion) + yearsquant + restbar + size + bribes_abvic
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.d1)
confint(p4.d1)
dev.stat(p4.d1)
vif(p4.d1)

p4.e1 <- glmmadmb(extortions ~ log(denuncias_homs) + log(poblacion) + yearsquant + restbar + size + bribes_abincs
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.e1)
confint(p4.e1)
dev.stat(p4.e1)
vif(p4.e1)

# evaluate

lr.p4d1_p4d <- lrtest(p4.d1, p4.d)

lr.p4d1_p4d

lr.p4e1_p4e <- lrtest(p4.e1, p4.e)

lr.p4e1_p4e


# compare with full models o4.0 y o4.nb

lr.p4d1_o4.0 <- lrtest(p4.d1, o4.0)

lr.p4d1_o4.0

lr.p4e1_o4.0 <- lrtest(p4.e1, o4.0)

lr.p4e1_o4.0

lr.p4d1_o4.nb <- lrtest(p4.d1, o4.nb)

lr.p4d1_o4.nb

lr.p4e1_o4.nb <- lrtest(p4.e1, o4.nb)

lr.p4e1_o4.nb

# drop log pop

p4.d2 <- glmmadmb(extortions ~ log(denuncias_homs) + yearsquant + restbar + size + bribes_abvic
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.d2)
confint(p4.d2)
dev.stat(p4.d2)
vif(p4.d2)

p4.e2 <- glmmadmb(extortions ~ log(denuncias_homs) + yearsquant + restbar + size + bribes_abincs
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.e2)
confint(p4.e2)
dev.stat(p4.e2)
vif(p4.e2)

# evaluate

lr.p4d2_p4d1 <- lrtest(p4.d2, p4.d1)

lr.p4d2_p4d1

lr.p4e2_p4e1 <- lrtest(p4.e2, p4.e1)

lr.p4e2_p4e1

# drop log mur

p4.d3 <- glmmadmb(extortions ~ + yearsquant + restbar + size + bribes_abvic
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.d3)
confint(p4.d3)
dev.stat(p4.d3)
vif(p4.d3)

p4.e3 <- glmmadmb(extortions ~ + yearsquant + restbar + size + bribes_abincs
                    (1 | NOM_ABR), data=enve_test,
                    family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(p4.e3)
confint(p4.e3)
dev.stat(p4.e3)
vif(p4.e3)

# evaluate

lr.p4d2_p4d3 <- lrtest(p4.d2, p4.d3)

lr.p4d2_p4d3

lr.p4e2_p4e3 <- lrtest(p4.e2, p4.e3)

lr.p4e2_p4e3


#############################
### Birbery as a DV

b4.0 <- glmmadmb(bribes ~ extortions + tasahom + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(b4.0)
confint(b4.0)
dev.stat(b4.0)
# VIF
vif(b4.0)

## dropping extortion
b4.1 <- glmmadmb(bribes ~ tasahom + yearsquant + subsector + size +
                 (1 | NOM_ABR), data=enve_test,
                 family="nbinom", zeroInflation=FALSE, extra.args="-ndi 60000",
                 admb.opts = admbControl(noinit = FALSE))

summary(b4.1)
confint(b4.1)
dev.stat(b4.1)
# VIF
vif(b4.1)

# evaluate

lr.b4_b41 <- lrtest(b4.0, b4.1)

lr.b4_b41



##########################################################################
##########################################################################
##########################################################################
##########################################################################

############################## END OF FILE ###############################

##########################################################################
##########################################################################
##########################################################################
##########################################################################
