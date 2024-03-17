install.packages("readxl")
install.packages("hrbrthemes")
library(readxl)
library(ggplot2)
library(hrbrthemes)
#parte 1
#Upload the data
datos<-read_excel("Datos_lab10.xlsx", sheet ="Datos_lab10")

Model <- lm(PAM ~ Edad+ Peso  +Altura, data = datos)
summary(Model)

#parte 2

#Create the linear regression
plot(datos$PAM, pch = 16, col = "blue") #Plot the results
abline(Model) #Add a regression line

