#####################
#load packages
#####################
library(psych)
library(lavaan)

#####################
#Simulate data1
#####################
model1=
  'F1 =~ .30*V1+.50*V2+.70*V3+.90*V4
F2 =~ .30*V5+.50*V6+.70*V7+.90*V8
F1~~.5*F2'


data1<-simulateData(model = model1, model.type = "sem", meanstructure = FALSE, 
                   int.ov.free = TRUE, int.lv.free = FALSE, conditional.x = FALSE,
                   fixed.x = FALSE,orthogonal = FALSE, std.lv = TRUE, sample.nobs = 500L)
cor.plot(data1)
fa(data1,nfactors=2,fm="ml")
pca(data1,nfactors=2)

#####################
#Simulate data2
#####################
model2=
  'F1 =~ .90*V1+.85*V2+.80*V3+.75*V4+.70*V5+.65*V6+.60*V7+.55*V8+.50*V9+.45*V10+.40*V11+.15*V12'
#Observe that item v12 is specified to load .15 on the latent factor

data2<-simulateData(model = model2, model.type = "sem", meanstructure = FALSE, 
                   int.ov.free = TRUE, int.lv.free = FALSE, conditional.x = FALSE,
                   fixed.x = FALSE,orthogonal = FALSE, std.lv = TRUE, sample.nobs = 500L)
#################################################################
#Execute: EFA with 1 and 2 factors, PCA with 1 and two components
#What can you conclude from the results of these analyses?
#################################################################


