#packages
library(psych)
library(GPArotation)
library(lavaan)
library(semPlot)

#loading Big Five data from psych
data("bfi")
str(bfi)
View(bfi)
fa(bfi[,1:25],nfactors=1,fm="ml",rotate="oblimin")

#now test a restricted (CFA) version of the best EFA solution
model='

'
#test for multivariate normality
mardia(bfi[,1:25])

#test the model using standard ML
fit1<-cfa(model, bfi[,1:25],estimator="ML",std.lv=TRUE)
summary(fit1,standardized=TRUE,fit.measures=TRUE)

#test the model using the corrected Bollen-Stine chi-square
fit1<-cfa(model, bfi[,1:25],estimator="ML",std.lv=TRUE,test="Bollen.Stine")
summary(fit1,standardized=TRUE,fit.measures=TRUE)

#test the model using DWLS estimator
fit1<-cfa(model, bfi[,1:25],estimator="WLSMV", mimic="Mplus",
          ordered=c("E1","E2","E3","E4","E5","A1","A2","A3","A4","A5",
                    "C1","C2","C3","C4","C5","N1","N2","N3","N4","N5",
                    "O1","O2","O3","O4","O5"),std.lv=TRUE)
summary(fit1,standardized=TRUE,fit.measures=TRUE)

#plot model diagram using semPlot
semPaths(fit1)
help(semPaths)

#display the estimated parameters
coef(fit1)

#name parameters
model='
F1=~cool_loading*v1+v2+v3
'

#fix a parameter to a specified value
model='
F1=~.68*v1+v2+v3
'

#scale the latent variable to have variance = 1
model='
F1=~NA*v1+v2+v3
F1~~1*F1
'

#give a parameter a starting value
model='
F1=~start(.68)*v1+v2+v3
'

#use the Satorra-Bentler scaled chi-square difference test to compare nested models
lavTestLRT(fit1, fit2, test="satorra.2000")


