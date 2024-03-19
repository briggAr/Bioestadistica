install.packages("leaps")
library(leaps)
library(dplyr)
library(tidyr)
library(car)
library(ggplot2)
library(readxl)
library(hrbrthemes)




base_datos<- read.table("Colombia_datos.csv",header = TRUE, sep=",")

filtro_tiempo <- filter(base_datos, REGION != "NA", LANG!="NA", YEAR <= 2017)

lang = as.factor(filtro_tiempo$LANG)
region = as.factor(filtro_tiempo$REGION)
casos = filtro_tiempo$CASES
año = filtro_tiempo$YEAR
densidad= filtro_tiempo$POB_PCAB
elevacion = filtro_tiempo$URB_ELEV_MEAN
temperatura =  filtro_tiempo$T_MEAN

dengue = data.frame(cbind(casos,lang, region, densidad, elevacion, temperatura))


#seleccion del modelo

modelSelect<- regsubsets(casos~., data = dengue, nvmax = 10, method = "forward")
summary(modelSelect)

#mejor modelo
factores1 = lm(casos~densidad,data=dengue)
summary(factores1)
factores2 = lm(casos~ region+ densidad,data=dengue)
summary(factores2)
factores3 = lm(casos~ region+ densidad+elevacion,data=dengue)
summary(factores3)
factores4 = lm(casos~region+ densidad+elevacion+temperatura,data=dengue)
summary(factores4)
factores5 = lm(casos~region+ densidad+elevacion+temperatura,data=dengue)
summary(factores5)
factores6 = lm(casos~lang + region+ dengue$densidad+dengue$elevacion+dengue$temperatura)
summary(factores6)


#prediccion

filtro_predict <-  filter(base_datos,REGION != "NA", LANG !="NA", YEAR >= 2018 )

lang_p = as.factor(filtro_predict$LANG)
region_p = as.factor(filtro_predict$REGION)
año_p = filtro_predict$YEAR
densidad_p = filtro_predict$POB_PCAB
elevacion_p = filtro_predict$URB_ELEV_MEAN
temperatura_p = filtro_predict$T_MEAN

dengue_pred=  data.frame(cbind(lang_p, region_p, densidad_p, elevacion_p, temperatura_p))


prediccion = predict(factores6, newdata = dengue_pred, interval = 'prediction')
dbpred <- data.frame(cbind(dengue_pred, prediccion, año_p))


plot(dengue,aes(y=casos, x=lang+ region+ densidad+ elevacion+ temperatura))+geom_point()+geom_smooth(method="lm")

Plot


Plot2 <-
  plot(dbpred,aes(y=fit,x=lang_p+region_p+ densidad_p+ elevacion_p+ temperatura_p))+geom_point()+geom_smooth(method="lm")
Plot2 + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")
