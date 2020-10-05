### loading packages
library(psych)
library(GPArotation)
library(lavaan)
library(dplyr)

#####################
#Simulate data4
#####################
model1=
  'FG =~ .70*V1+.70*V2+.70*V3+.70*V4+.70*V5+.70*V6+.70*V7+.70*V8+.70*V9
F1 =~ .70*V1+.70*V2+.70*V3
F2 =~ .70*V4+.70*V5+.70*V6
F3 =~ .70*V7+.70*V8+.70*V9
F1~~0*F2
F1~~0*F3
F2~~0*F3
FG~~0*F1
FG~~0*F2
FG~~0*F3
'
data4<-simulateData(model = model1, model.type = "sem", sample.nobs = 500L)

### reading data
data("Holzinger.9")
setwd("C:/Users/hauck/OneDrive/Google Drive/USF/Aulas/2020/AF&SEM/Analyses")

###EFA BIFACTOR TARGET ROTATION
###Specifying a theoretical target matrix 
Targ <- make.keys(9,list(fg=1:9,f1=1:3,f2=4:6,f3=7:9)) 
Targ <- scrub(Targ,isvalue=1)  #fix the 0s, allow the NAs to be estimated
Targ <- list(Targ)
Targ

fit<-fa(Holzinger.9,4,rotate="TargetQ",Target=Targ)
print(fit,sort= FALSE)

####FACTOR CONGRUENCE
#importing theoretical matrix
library(xlsx)
matrix <- read.xlsx("matrix.xlsx", 1)

#BIFACTOR JENNRICH-BENTLER
bif<-fa(data4, nfactors = 4, fm = "minres", rotate="bifactor")
print(fit,sort= FALSE)
biq<-fa(data4, nfactors = 4, fm = "minres", rotate="biquartimin")
print(fit,sort= FALSE)

#CONGRUENCE
fa.congruence(matrix,biq,structure=FALSE)



