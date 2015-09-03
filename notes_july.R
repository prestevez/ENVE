t_size <- data.frame(Size=c("Micro", "Small", "Medium", "Large"),
                     Industry=c("0 - 10", "11 - 50", "51 - 250", "251+"),
                     Commerce=c("0 - 10", "11 - 30", "31 - 100", "101+"),
                     Services=c("0 - 10", "11 - 50", "51 - 100", "101+"))
t_size


library(xtable)
print(xtable(t_size, 
       caption="Business size by sector, classified by number of employees. Adapted from ",
       label="T_Bsize", align = "ccccc"), include.rownames = FALSE)


xsumm_table <- xtable(summ_table, digits=c(0,0,0,0,3,3,3), caption="Descriptive Statistics", label="T_summ")

print(xsumm_table, include.rownames=FALSE)

obsexp

print(xtable(obsexp[,1:3], digits=c(0,0,0,3), 
             caption="Observed and expected (Poisson) distributions", 
             label="T_dist"), include.rownames=FALSE)

ext_dist

print(xtable(ext_dist, digits=c(0,0,0,0,3,3,3), 
             caption="Distribution of extortion vixtimisations", 
             label="T_dist"), include.rownames=FALSE)


vmr <- data.frame(Mean=mean(b_test$extortions), Variance=var(b_test$extortions), 
                  Ratio=(var(b_test$extortions)/mean(b_test$extortions)), Index=index)

vmr

print(xtable(vmr, digits=c(0,3,3,3,1), 
             caption="Extortion mean, variance and index of dispersion", 
             label="T_vmr"), include.rownames=FALSE)


