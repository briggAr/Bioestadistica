#instalamos las librerias
install.packages("dplyr")
install.packages("tidyr")
install.packages("car")
#las llamamos
library(dplyr)
library(tidyr)
library(car)
library(ggplot2)

#abrimos nuestra base de datos 
datos<- read.table("BrainStroke.csv",header = TRUE, sep=",")

#primer filtro por edaad( mayor o igual a 18) y que si hayan tenido un ACV.
filtro_edad <- filter(datos, age>=18, stroke==1)

#segundo filtro para obtener solo los que tienen enfermedad del corazón
heart_1 <- filter(filtro_edad,heart_disease==1)

#segundo filtro para obtener solo los que tienen hipertensión
hyp <-filter(filtro_edad, hypertension==1)


#realizamos el prop.test según la longitud de heart_1 e hyp.
#el tamaño de la muestra es la misma porque el fitrado de ambas es desde el primer filtro

prop.test(x=c(66,47), n=c(246,246),
          alternative='greater', conf.level=0.95)


#de este promt test se concluye que la proporcion de gente con hypertension
#es mayor a la que tiene heart_disease.


# PROCEDIMIENTO PARA ANOVA

#realizamos el filtrado de los IMC para cada categoria

h_imc <- select(hyp, bmi)  # para los que tienen hipertension
h0_imc<- select(filter(filtro_edad, hypertension==0), bmi) #para los que no tienen hipertension
hd_imc<-select(heart_1, bmi) # para los que tienen enfermedad del corazon
hd0_imc <- select(filter(filtro_edad, heart_disease==0), bmi) #para los que no tienen enfermedad del corazon


# se definen los nombres de las categorias y se multiplican por la longitud de los filtrados de arriba
h <- matrix(rep(c("si hipertension"), 66))
h0 <- matrix(rep(c("no hipertension"),180))
hd <- matrix(rep(c("si heart disease"),47))
hd0 <-matrix(rep(c("no heart disease"),199))

#juntamos por filas los nombres para cada categoria
tratamientos <-rbind(h, h0, hd, hd0)    

# juntamos por filas los valores de los IMC para cada categoria conservando el orden de los nombres 
valores <- rbind(h_imc, h0_imc, hd_imc, hd0_imc)       

# concatenamos por columnas los tratamientos (nombres de las categorias) y sus respectivos valores de IMC
datos <- cbind(tratamientos, valores)

#convertimos en un data frame
data_f <- as.data.frame(datos)




#realizamos el ANOVA con el dataframe anterormente creado
res <- aov (data_f$bmi~data_f$tratamientos)

#visualizamos el ANOVA
summary(res)


#SUPUESTOS DE NORMALIDAD
hist(data_f$bmi)
plot(density(data_f$bmi))

#grafica Q-Q norm
qqnorm(data_f$bmi)

#linea de tendencia para Q-Q norm
qqline(data_f$bmi)

#prueba shapiro
shapiro.test(data_f$bmi)


#grafica de la varianza 
ggplot(data_f) +
  aes(x = tratamientos , y =bmi) +
  geom_point()+ 
  theme_classic()

 
# H0: La varianza es igual entre los tratamientos
# H1: la varian es diferente entre los tratamientos

# La prueba de levene
leveneTest(res)

#tukey test
tukey_res<-TukeyHSD(res)

#visualizamos tukey test
tukey_res
