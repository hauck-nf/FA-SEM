#packages
library(readr)
library(psych)
library(lavaan)
library(semPlot)

#loading Big Five data from psych
banco_1 <- read.table("C:/Users/hauck/OneDrive/IBAP_jornada2020/banco_1.dat", quote="\"", comment.char="")
View(banco_1)
str(banco_1)


#modelo fatorial confirmatorio sem covariavel
modelo_base='
F1=~V1+V2+V3+V4+V5
F2=~V6+V7+V8+V9+V10
'
#teste de normalidade multivariada
mardia(banco_1[,1:10])

#teste do modelo confirmatorio com estimador DWLS
fit_base<-cfa(modelo_base, banco_1[,1:10],estimator="WLSMV", mimic="Mplus",
          ordered=c("V1","V2","V3","V4","V5","V6",
                    "V7","V8","V9","V10"),std.lv=TRUE)
summary(fit_base,standardized=TRUE,fit.measures=TRUE)

#plotar diagrama do modelo usando semPlot
semPaths(fit_base)



#ETAPA 1: MIMIC com efeitos indiretos da covariavel nos itens
mimic1='
F1=~V1+V2+V3+V4+V5
F2=~V6+V7+V8+V9+V10
F1~V11
F2~V11
'
#teste do MIMIC1
fit_mimic1<-cfa(mimic1, banco_1[,1:11],estimator="WLSMV", mimic="Mplus",
              ordered=c("V1","V2","V3","V4","V5","V6",
                        "V7","V8","V9","V10"),std.lv=TRUE)
summary(fit_mimic1,standardized=TRUE,fit.measures=TRUE)

#plotar diagrama do modelo usando semPlot
semPaths(fit_mimic1)



#ETAPA 2: MIMIC com efeitos indiretos e diretos da covariavel nos itens
mimic2='
F1=~V1+V2+V3+V4+V5
F2=~V6+V7+V8+V9+V10
F1~V11
F2~V11
V1~0*V11
V2~V11
V3~V11
V4~V11
V5~V11
V6~0*V11
V7~V11
V8~V11
V9~V11
V10~V11
'
#teste do MIMIC2
fit_mimic2<-cfa(mimic2, banco_1[,1:11],estimator="WLSMV", mimic="Mplus",
                ordered=c("V1","V2","V3","V4","V5","V6",
                          "V7","V8","V9","V10"),std.lv=TRUE)
summary(fit_mimic2,standardized=TRUE,fit.measures=TRUE)

#plotar diagrama do modelo usando semPlot
semPaths(fit_mimic2)


#ETAPA 3: MIMIC com efeitos indiretos e diretos da covariavel nos itens
##liberando itens fixados em 0 na Etapa2
mimic3='
F1=~V1+V2+V3+V4+V5
F2=~V6+V7+V8+V9+V10
F1~V11
F2~V11
V1~V11
V2~V11
V3~0*V11
V4~V11
V5~V11
V6~V11
V7~V11
V8~0*V11
V9~V11
V10~V11
'
#teste do MIMIC2
fit_mimic3<-cfa(mimic3, banco_1[,1:11],estimator="WLSMV", mimic="Mplus",
                ordered=c("V1","V2","V3","V4","V5","V6",
                          "V7","V8","V9","V10"),std.lv=TRUE)
summary(fit_mimic3,standardized=TRUE,fit.measures=TRUE)

#plotar diagrama do modelo usando semPlot
semPaths(fit_mimic2)



#ALTERNATIVA
summary(fit_mimic1,standardized=TRUE,fit.measures=TRUE,modindices = TRUE)
