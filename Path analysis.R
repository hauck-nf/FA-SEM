library(plyr)
library(psych)
library(lavaan)
library(semPlot)
library(readxl)

#####################
#Simulate data6
#####################
model1=
  'Z~.5*X
  T~-.5*X
  Y~.5*Z
  Y~.5*T
'
data6<-simulateData(model = model1, model.type = "sem", sample.nobs = 500L,standardized=TRUE)

########
#does buying a videogame (X) make you happier (Y)?
#How do leisure hours (Z) and money (T) enter the equation?
########

#test bivariate correlations
corr.test(data6)

#test different models with the data; try to omit variables Z or T
model=
  'Z~X
  Y~Z
  Y~X
  T~X
  Y~T
  '
fit<-sem(model, data6,estimator="ML",std.lv=TRUE)
summary(fit,standardized=TRUE,fit.measures=TRUE)

semPaths(fit)

##
#theorize and run a path model with the "bfi" data
#first, let's calculate bfi total scores
#load an item dictionary containing variable names, theoretical factor,
#semantic pole (negative = -1; positive = 1), and order of appearance 
#according to the dataset
setwd("C:/Users/hauck/OneDrive/Google Drive/USF/Aulas/2020/AF&SEM/Analyses")#enter the path directory where the item dictionary is located
item_dic_bfi<-read_excel("item_dic_bfi.xlsx")

#score variables using the dictionary info and the psych package
item_dic_bfi$order2 <-ifelse(item_dic_bfi$pole==1, 
                             item_dic_bfi$order, item_dic_bfi$order*-1) 

keys.list <-dlply(item_dic_bfi[ , c("factor","order2")], .(factor))
keys.list <-lapply(keys.list , `[`, , 2)

keys <- make.keys(nvars=dim(item_dic_bfi)[1], keys.list = keys.list,
                  item.labels =item_dic_bfi$coditem)
keys_i <- keys 
rownames(keys_i) <- paste(rownames(keys_i),"i", sep="")

bfi_scores <- scoreItems( keys = keys, items = bfi[,1:25][ , rownames(keys)],
                                 missing=TRUE, impute ="none") 
scores<-as.data.frame(bfi_scores[["scores"]])
bfi_scores[["alpha"]]
bfi_scores[["G6"]]


#add columns with scores to the bfi dataset
bfi_total<-cbind(bfi,scores)

#####
#hypothesize, specify, and test a path model using lavaan!
#####
#step1: saturated model with 0 degrees of freedom
model_path_bfi_saturated=
  '
  E ~ age + education
  A ~ age + education
  C ~ age + education
  N ~ age + education
  O ~ age + education
  '
fit<-sem(model_path_bfi_saturated, bfi_total,estimator="MLR")
summary(fit,standardized=TRUE,fit.measures=TRUE)

#step2: restricted model with >0 degrees of freedom
model_path_bfi_restricted=
  '
  E ~ age 
  A ~ age 
  C ~ age 
  N ~ age 
  O ~ education
  '
fit<-sem(model_path_bfi_restricted, bfi_total,estimator="MLR")
summary(fit,standardized=TRUE,fit.measures=TRUE,modindices=TRUE)





