# intervalos confianza
library("readxl")

df<-read_excel("Datos Regresion IC.xlsx")
# plotear los datos
plot(df$Pull_Strength,df$Wire_Length)
# ajustar la regresion
fit<- lm(Pull_Strength~Wire_Length,data=df)
summary(fit)
# crear un nuevo dataset para predecir 
range(df$Wire_Length)
newx<-c(1,seq(min(df$Wire_Length),max(df$Wire_Length),by=0.8))
newdata<-data.frame(df,newx)

coeficientes <- confint(fit)
confianza<- predict(fit,data.frame(Wire_Length=c(1,2,3,4,5)),interval = "confidence")

prediccion <- predict(fit,data.frame(Wire_Length=c(1,2,3,4,5)),interval = "prediction")

plot(df$Wire_Length,coeficientes[,1])
lines(df$Wire_Length,coeficientes[,2])
lines(df$Wire_Length,coeficientes[,3])

#### 
df<-read_excel("Datos Regresion IC.xlsx")
# plotear los datos
plot(df$Pull_Strength,df$Wire_Length)
# ajustar la regresion
reg<- lm(Pull_Strength~Wire_Length,data=df)
coef(reg)

bootsreg<-function(data, id){
  reg<- lm(Pull_Strength~Wire_Length,data=df[id,])
  coef(reg)
}

b<-boot::boot(df,bootsreg,100001)

boot::boot.ci(b,index=2,type="perc")

# hacer el mismo proceso para los intervalos de prediccion 

reg<- lm(Pull_Strength~Wire_Length,data=df)
beta<-coef(reg)
sum(beta*c(1,109))

bootsreg<-function(data, id){
  xnew<-109
  reg<- lm(Pull_Strength~Wire_Length,data=df[id,])
  beta<-coef(reg)
  sum(beta*c(1,xnew))
}

b<-boot::boot(df,bootsreg,1001)

boot::boot.ci(b,type="perc")

hist(b)
