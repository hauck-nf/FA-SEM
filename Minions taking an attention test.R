#####################
#load packages
#####################
library(psych)
library(GPArotation)
library(lavaan)
library(dplyr)
library(cut)
library(car)
#####################
#Simulate data3
#####################
#5000 minions take an attention test comprising 8 tasks. 
#Minions have ages from 6 to 10 years.
#Each task requires that minions read a small text and soon after identify the correct stimulus among a set of different figures.

model=
  'F1 =~ .80*V1+.70*V2+.60*V3+.50*V4+.40*V5+.30*V6+.20*V7+.10*V8
  F2 =~ .10*V5+.20*V6+.30*V7+.40*V8
  V1|-2*t1
  V2|-1.5*t1
  V3|-1*t1
  V4|0*t1
  V5|.5*t1
  V6|1*t1
  V7|1.5*t1
  V8|2*t1
F1~~.0*F2
F1~.5*idade
F2~-.3*idade'

data3<-simulateData(model = model, model.type = "sem", sample.nobs = 5000L)
data3$idade <- cut(data3$idade, breaks=c(-Inf, -1, 0, 1, 1.5,Inf), labels=c("6","7","8","9","10"))
data3 <- apply(data3, 2,FUN = function(x) recode(x, "1=0; 2=1"))
data3 <- as.data.frame(data3)


################
#explore the data
################
#Item frequencies
apply(data3,2,table)
#Item correlations
corr.test(data3)

#One factor modeling a latent trait, one factor modeling fatigue
#EFA
fa(data3[,1:8],nfactors=2,rotate="varimax",cor="poly")

#CFA
model1='
Fator=~NA*V1+V2+V3+V4+V5+V6+V7+V8
Fator~~1*Fator
Fadiga=~NA*V5+V6+V7+V8
Fadiga~~1*Fadiga
Fator~~0*Fadiga
'
fit<-sem(model1,data3[,1:8],ordered = c("V1","V2","V3","V4","V5","V6","V7","V8"))
summary(fit, fit.measures = TRUE, standardized=TRUE)

#Age variable as a factor covariate
model2='
Fator=~NA*V1+V2+V3+V4+V5+V6+V7+V8
Fator~~1*Fator
Fadiga=~NA*V5+V6+V7+V8
Fadiga~~1*Fadiga
Fator~~0*Fadiga
Fator~idade
Fadiga~idade
'
fit<-sem(model2,data3,ordered = c("V1","V2","V3","V4","V5","V6","V7","V8"))
summary(fit, fit.measures = TRUE, standardized=TRUE)

#Age capturing the fatigue effect
model3='
Fator=~NA*V1+V2+V3+V4+V5+V6+V7+V8
Fator~~1*Fator
Fator~idade
V5~idade
V6~idade
V7~idade
V8~idade
'
fit<-sem(model3,data3,ordered = c("V1","V2","V3","V4","V5","V6","V7","V8"))
summary(fit, fit.measures = TRUE, standardized=TRUE)
      
               
##########
#NOW HYPOTHESIZE A MODEL FOR A NEW TESTING SITUATION!
#Suppose the minions answered a questionnaire containing 5 items measuring negative life events, and 5 items measuring happiness.
#Items are rated on a 5-point Likert scale
#Now draw a model diagram with a plausible explanation to the observed item responses




