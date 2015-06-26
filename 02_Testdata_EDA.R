# Descriptive statistics

setwd(dirRdata)
load(file="TestData.Rdata")

setwd(MainWD)

## Distribution of extortion victimizations

ext_dist <- data.frame(summary(as.factor(testset$extorsiones)))
str(ext_dist)

#3 Better to use:
data.frame(table(testset$extorsiones))
str(data.frame(table(testset$extorsiones)))


ext_dist <- data.frame("Events"=as.integer(as.character(rownames(ext_dist))),
                       "People"=ext_dist[,1])
ext_dist$Crimes <- ext_dist$People * ext_dist$Events
ext_dist$perper <- prop.table(ext_dist$People)*100
ext_dist$pervic[2:15] <- prop.table(ext_dist[2:15,2])*100
ext_dist$percrim <- prop.table(ext_dist$Crimes)*100

ext_dist


# Chi-sq test for poission distribution

# create df of obs vs expected for counts 

obsexp <- data.frame("Num."=0:ext_dist[length(ext_dist$Events),1],
                     "Obs."=0, "Exp."=0)

obsexp <- merge(obsexp, ext_dist, by.x="Num.", by.y="Events", all.x=T) 

obsexp <- obsexp[,c("Num.", "People")]
colnames(obsexp)[2] <- "Obs."

obsexp$Obs.[is.na(obsexp$Obs.)] <- 0 

obsexp


## testing for poisson distribution
lamb <- mean(testset$extorsiones)
ext_var <- var(testset$extorsiones)

lamb
ext_var

ext_var/lamb

k <- lamb^2/(ext_var-lamb)

barplot(obsexp$Obs.)

str(obsexp)

# Generate expected values under nb distribution
obsexp$exp_nb <- dnbinom(0:(length(obsexp$Obs.)-1),1,mu=lamb)*length(testset$extorsiones)

barplot(obsexp$exp_nb)
obsexp$exp_nb

#Test for goodnes of fit
cs<-factor(0:30) 
levels(cs)[6:31]<-"5+" 
levels(cs)

ef<-as.vector(tapply(obsexp$exp_nb,cs,sum))
of<-as.vector(tapply(obsexp$Obs.,cs,sum))

ef
of

chinb <- sum((of-ef)^2/ef)

chinb

1-pchisq(chinb,3)

## kfit fun

kfit <- function(x) { 
  lhs <- numeric() 
  rhs <- numeric() 
  y <- 0:(length(x) - 1) 
  j <- 0:(length(x)-2) 
  m <- sum(x * y)/(sum(x)) 
  s2 <- (sum(x * y^2) - sum(x * y)^2/sum(x))/(sum(x)- 1) 
  k1 <- m^2/(s2 - m) 
  a <- numeric(length(x)-1)
  for(i in 1:(length(x) - 1))  a[i] <- sum(x [- c(1:i)]) 
  i <- 0 
  for (k in seq(k1/1.2,2*k1,0.001)) {
    i <- i+1 
    lhs[i] <- sum(x) * log(1 + m/k) 
    rhs[i] <- sum(a/(k + j))  
    }
  k <- seq(k1/1.2,2*k1,0.001) 
  plot(k, abs(lhs-rhs),xlab="k",ylab="Difference",type="l",col="red") 
  d <- min(abs(lhs-rhs)) 
  sdd <- which(abs(lhs-rhs)==d) 
  k[sdd] 
}

kf <- round(kfit(obsexp$Obs.), 3)
k

negbin <- function(x,u,k) 
  (1+u/k)^(-k)*(u/(u+k))^x*gamma(k+x)/(factorial(x)*gamma(k))

nb1 <- length(testset$extorsiones)*(1+lamb/kf)^(-kf)*factorial(kf+(0:30)-1)/
  (factorial(0:30)*factorial(kf-1))*(lamb/(lamb+kf))^(0:30)

nb1

obsexp$exp_nb2 <- nb1

#Test for goodnes of fit
cs<-factor(0:30)
levels(cs)[9:31]<-"8+" 
levels(cs)

ef<-as.vector(tapply(obsexp$exp_nb2,cs,sum))
of<-as.vector(tapply(obsexp$Obs.,cs,sum))

ef
of

chinb <- sum((of-ef)^2/ef)

chinb

1-pchisq(chinb,6)



####
probs <- dpois(0:ext_dist[length(ext_dist$Events),1], lambda=lamb)

# Normal Chi Squared test
test <- chisq.test(x=obsexp$Obs., p=probs)
test

# Chi Squared using simulated p-value
testsim <- chisq.test(x=obsexp$Obs., p=probs, simulate.p.value=TRUE)
testsim

str(test)

obsexp$Exp. <- round(test$expected, 2)
obsexp

barplot(obsexp$Obs.[2:length(obsexp$Obs.)])

# Kolmogorov-Smirnov Tests
ks.ztest_p <- ks.test(testset$extorsiones, "dpois", lamb)
ks.ztest_p
## Testing for negative binomial distribution

fitdistr(testset$extorsiones, "negative binomial")

fitdistr(testset$extorsiones, "Poisson")

fitnb <- fitdistr(testset$extorsiones, "negative binomial")

str(fitnb)

fitnb$estimate[1]

ks.ztest <- ks.test(testset$extorsiones, "dnbinom", size=fitnb$estimate[1], mu=fitnb$estimate[2])

ks.ztest

?dbinom

#### Test some models

?glm

model.gaus <- glm(extorsiones ~ robos + tasahom + personas + clas_viv,
                  data=testset)
summary(model.gaus)

model.pois2 <- glm(extorsiones ~ robos + tasahom + personas + clas_viv + barrera,
                  data=testset, family=poisson(), na.action="na.omit")
summary(model.pois2)
# test for goodness of fit
1-pchisq(summary(model.pois2)$deviance, summary(model.pois2)$df.residual)


model.pois3 <- glm(extorsiones ~ robos + tasahom*NOM_ENT + personas + clas_viv + barrera,
                   data=testset, family=poisson(), na.action="na.omit")
summary(model.pois3)

model.nb <- glm.nb(extorsiones ~ robos + tasahom + personas + clas_viv + barrera,
                   data=testset)
summary(model.nb)

1-pchisq(summary(model.nb)$deviance, summary(model.nb)$df.residual)

pchisq(2 * (logLik(model.nb)-logLik(model.pois2)), df=1, lower.tail = FALSE)

data.frame(exp(model.nb$coefficients))

1-pchisq(16144, 84619)

plot(model.nb)

anova(model.nb, test="Chisq")

drop1(model.nb, test="Chisq")

options(scipen = 99999)

lm <- lm(extorsiones ~ robos + tasahom + personas + clas_viv,
                  data=testset)

summary(lm)
plot(lm)





## Save objects for use latter
setwd(dirRdata)
save(list=ls(all=TRUE), file="TestData.Rdata")

setwd(MainWD)
