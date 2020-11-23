#SEM
library(psych)
library(lavaan)
library(semPlot)

#specify the SEM model to be tested
##first part is the measurement models
##second part is the structural model with linear regressions
model='
#part1
E=~E1+E2+E3+E4+E5
A=~A1+A2+A3+A4+A5
C=~C1+C2+C3+C4+C5
N=~N1+N2+N3+N4+N5
O=~O1+O2+O3+O4+O5
#part2
E ~ age 
A ~ age 
C ~ age 
N ~ age 
O ~ education
'
#fot the model using a robust estimator apropriate for ordered categorical data (the Big Five indicators)
fit1<-sem(model, bfi_total[,c(1:25,27,28)],estimator="WLSMV", mimic="Mplus",
          ordered=c("E1","E2","E3","E4","E5","A1","A2","A3","A4","A5","C1",
                    "C2","C3","C4","C5","N1","N2","N3","N4","N5",
                    "O1","O2","O3","O4","O5"))

#request the output
summary(fit1,standardized=TRUE,fit.measures=TRUE,modindices=TRUE)


#print the model diagram using semPlot
semPaths(fit1)



