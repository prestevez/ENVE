obsexp$exp_po <- dpois(0:(length(obsexp$Events)-1), lambda=mean_ext) * 
  length(b_test$extortions)

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

# Poisson test

po_chisq <- obs_exp_test(obsexp, obsexp$exp_po, 1)

po_chisq

# Fisher test to compare with Poisson expected distribution
with(obsexp, fisher.test(Obs, exp_po, alternative="two-sided", hybrid = TRUE, 
                         simulate.p.value = FALSE))

with(obsexp, ks.test(Obs, exp_po))

library(MASS)

nb_estimates <- fitdistr(b_test$extortions, "Negative Binomial")

nb_estimates

# Generating the neg bin expected frequencies

obsexp$exp_nb <- dnbinom(0:(length(obsexp$Events)-1), size=nb_estimates$estimate[1], 
                         mu=nb_estimates$estimate[2]) *length(b_test$extortions)


nb_chisq <- obs_exp_test(obsexp, obsexp$exp_nb, 2)

nb_chisq

with(obsexp, fisher.test(Obs, exp_nb))

ks.test(b_test$extortions, rnbinom(0:(length(b_test$extortions)-1), size=nb_estimates$estimate[1],
                                        mu=nb_estimates$estimate[2]))

ks.test(b_test$extortions, "dnbinom", size=nb_estimates$estimate[1],
        mu=nb_estimates$estimate[2])

ks.test(b_test$extortions, "dpois", lambda=mean(b_test$extortions))

ks.test(b_test$extortions, rpois(0:(length(b_test$extortions)-1), lambda=mean(b_test$extortions)))


install.packages("dgof")
library(dgof)

plot(ecdf(b_test$extortions))
plot(ecdf(rpois(0:(length(b_test$extortions)-1), lambda=mean(b_test$extortions))))
plot(ecdf(rnbinom(0:(length(b_test$extortions)-1), size=nb_estimates$estimate[1],
                  mu=nb_estimates$estimate[2])))


ks.test(b_test$extortions, ecdf(rpois(0:(length(b_test$extortions)-1), 
                                      lambda=mean(b_test$extortions))))

ks.test(b_test$extortions, ecdf(rnbinom(0:(length(b_test$extortions)-1), size=nb_estimates$estimate[1],
                                   mu=nb_estimates$estimate[2])))



# Testing with fisher's test between bribes and extortions

temp_ext <- as.factor(b_test$extortions)
levels(temp_ext)[2:length(levels(temp_ext))] <- "Victim"
temp_bribes <- as.factor(b_test$bribes)
levels(temp_bribes)[2:length(levels(temp_bribes))] <- "Victim"

ext_bribes <- ftable(temp_bribes, temp_ext)

ext_bribes

fisher.test(as.matrix(ext_bribes))

chisq.test(ext_bribes)

qchisq(c(.025,.975), n-1) # use only one tailed?

n <- length(b_test$extortions)
v <- var(b_test$extortions)
v
mean_ext
index <- ((n-1)*v)/mean_ext

index


rpois.null <- rpois(n, mean_ext)

var(rpois.null)
mean_ext


index.null <- ((n-1)*var(rpois.null))/mean(rpois.null)

index.null

library(lme4)

summary(m4.0)

confint(m3)

obs_exp

obst <- with(obs_exp, rep(Events, Freq))

ks.test(b_test$extortions, ecdf(rpois(0:(length(b_test$extortions)-1), 
                                      lambda=mean(b_test$extortions))))

ks.test(obst, ecdf(rpois(0:(length(b_test$extortions)-1), 
                                      lambda=mean(b_test$extortions))))

