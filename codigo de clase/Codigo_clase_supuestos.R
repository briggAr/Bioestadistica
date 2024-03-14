library(car)
library(ggplot2)
#The Null Hypothesis(H0): The average length of sepal of the iris flower is significantly equal among the three species.
#The Alternative Hypothesis (H1): At least one Species is different from the overall mean length of sepal of the iris flower.

ggplot(iris)+
aes(x = Sepal.Length, y = Petal.Length, colour = Species) +
  geom_point() +
  scale_color_hue()

plot(iris$Sepal.Length,type="l")

# correr un Analsis de varianza 

iris.aov<-aov(log(Sepal.Length)~Species,data=iris)

# Propbar Normalidad 
# exploramos graficamente conhistograma

hist(iris$Sepal.Length)
plot(density(iris$Sepal.Length))


# Q-Q Plot for variable MPG
qqnorm(log(iris$Sepal.Length))
qqline(log(iris$Sepal.Length))

# Probar la normalidad a traves un test estadisitico
# La prueba de shapiro-wilk
##H0: the data follow a normal distribution
##H1: the data do not follow a normal distribution

shapiro.test(log(iris$Sepal.Length))


# 2. Probar la homogeneidad de varainzas 

ggplot(iris) +
  aes(x = Species, y = Sepal.Length) +
  geom_point()+ 
  theme_classic()

library(HH)
hov(Sepal.Length~Species, data=iris)
hovPlot(Sepal.Length~Species,data=iris)

# La prueba de levene 
# H0: La varianza es igual entre los tratamientos
# H1: la varian es diferente entre los tratamientos

leveneTest(iris.aov)

# la prueba de bartlett 
bartlett.test(Sepal.Length~Species, data=iris)

# Figner-Killeen Test of Homogeneity of Variances
fligner.test(Sepal.Length~Species, data=iris)

# 3. Independencia 

a<-residuals(iris.aov)
b<-seq(1,150)
plot(b,a)

# La prueba de durbin watson 
# H0: hay indendencia 
# H1: que no hay independecia

iris.dw = durbinWatsonTest(iris.aov)
iris.dw

plot(iris.aov)

# 1. Homogeneity of variances
plot(iri, which = 3)

# 2. Normality
plot(iris.anova, which = 2)

