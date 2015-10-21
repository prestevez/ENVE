#### Assorted functions for the analysis

# Index of dispersion test

id.test <- function(x)
{
  # Takes a vector and calculates the index of dispersion
  # Then evaluates its significance for Overdispersion only
  # Returns a list with named numbers
  mu <- mean(x)
  names(mu) <- "Mean"
  v <- var(x)
  names(v) <- "Variance"
  n <- length(x)
  df <- n-1
  names(df) <- "df"
  index <- ((df)*v)/mu
  names(index) <- "I"
  pval <- pchisq(index, df, lower.tail=FALSE)
  names(pval) <- "P-value"
  pv95 <- qchisq(.95, df)
  names(pv95) <- "95% Chi-sq"

  return(c(index, pval, df, pv95))
}


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
