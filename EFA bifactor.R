### loading packages
library(psych)
library(GPArotation)
library(lavaan)
library(dplyr)


### reading data
data("Holzinger.9")

### SCHMID-LEIMAN TRANSFORMATION
fit<- schmid(Holzinger.9, nfactors = 3, fm = "minres", rotate="oblimin")
# Add "cor="poly" if data are categorical
print(fit,sort= FALSE)
omega(Holzinger.9)


### EFA BIFACTOR JENNRICH-BENTLER
fit<-fa(Holzinger.9, nfactors = 4, fm = "minres", rotate="bifactor")
# Add "cor="poly" if data are categorical
print(fit,sort= FALSE)


####TASK
#Now generate data using the following factor model (one general, three specific)
#Analyze the data using the Schmid-Leiman transformation (try omega function), and the Jennrich-Bentler rotation 

model=
  'FG =~ .8*V1+.8*V2+.8*V3+.8*V4+.8*V5+.8*V6+.8*V7+.8*V8+.8*V9
  F1 =~ .6*V1+.6*V2+.6*V3
  F2 =~ .6*V4+.6*V5+.6*V6
  F3 =~ .6*V7+.6*V8+.6*V9
FG~~.0*F1
FG~~.0*F2
FG~~.0*F3
F1~~0.*F2
F1~~0.*F3
F2~~0.*F3
'
data4<-simulateData(model = model, model.type = "sem", sample.nobs = 1000L)

