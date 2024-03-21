
library("readxl")


#punto 1
#importamos los datos
punto1 = read_excel("pulso.xlsx")
#realizamos la prueba kruskal
kruskal.test(pulse~task, data = punto1)

#punto 2 (U de man withney o algo asi (equivalente a t de dos muestras))


#punto 3
#importamos los datos
punto3 = read_excel("serpiente.xlsx")
#realizamos la prueba
kruskal.test(distancia~ temperatura, data = punto3)

#punto 4  (prueba de rangos de wilcoxon)
