##### Regresion lineal ###

## correlacion lineal 

## cargamos bases y packages 

library(MASS)

library(tidyverse)

data("Cars93")

#analisis visual 


ggplot(data = Cars93, aes(x = Weight, y = Horsepower)) + 
  geom_point(colour = "red4") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#analisis de normalidad 

qqnorm(Cars93$Weight, main = "Weight", col = "darkred")
qqline(Cars93$Weight)
qqnorm(Cars93$Horsepower, main = "Horsepower", col = "blue")
qqline(Cars93$Horsepower)

shapiro.test(Cars93$Weight)

shapiro.test(Cars93$Horsepower)

#transformacion logaritmica (si es posible)

# Representación gráfica

par(mfrow = c(1, 2))
hist(log10(Cars93$Horsepower), breaks = 10, main = "", xlab = "Log10(Horsepower)",
     border = "blue")
qqnorm(log10(Cars93$Horsepower), main = "", col = "blue")
qqline(log10(Cars93$Horsepower))


shapiro.test(log10(Cars93$Horsepower))

#analisis de Homocedasticidad 

ggplot(data = Cars93, aes(x = Weight, y = Horsepower)) + 
  geom_point(colour = "red4") +
  geom_segment(aes(x = 1690, y = 70, xend = 3100, yend = 300),linetype="dashed") +
  geom_segment(aes(x = 1690, y = 45, xend = 4100, yend = 100),linetype="dashed") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## calculo de correlacion 

cor.test(x = Cars93$Weight,
         y = log10(Cars93$Horsepower), 
         alternative = "two.sided",
         conf.level  = 0.95,
         method      = "pearson")

cor.test(x = Cars93$Weight,
         y = log10(Cars93$Horsepower),
         alternative = "two.sided",
         conf.level  = 0.95,
         method      = "spearman")

#calculo de tamaño de efecto 

R2_pearson <- cor(x = Cars93$Weight,
                  y = log10(Cars93$Horsepower),
                  method = "pearson")
R2_pearson <- R2_pearson^2
R2_pearson


R2_spearman <- cor(x = Cars93$Weight,
                   y = log10(Cars93$Horsepower),
                   method = "spearman")
R2_spearman <- R2_spearman^2
R2_spearman


### Matiz de correlacion 

# cargamos datos 

data(iris)
#Se seleccionan únicamente las variables numéricas
datos <- iris[,c(1,2,3,4)]
head(datos)


#aplicamos un ggally 

library(GGally)
ggpairs(iris, lower = list(continuous = "smooth"), 
        diag = list(continuous = "bar"), axisLabels = "none")


# realizamos la matriz de corelacion 

library(corrplot)

corrplot(corr = cor(x = datos, method = "pearson"), method = "number",
         tl.cex = 0.7,number.cex = 0.8, cl.pos = "n")

## Regresion lineal 

#Creamos datos 

equipos <- c("Texas","Boston","Detroit","Kansas","St.","New_S.","New_Y.",
             "Milwaukee","Colorado","Houston","Baltimore","Los_An.","Chicago",
             "Cincinnati","Los_P.","Philadelphia","Chicago","Cleveland","Arizona",
             "Toronto","Minnesota","Florida","Pittsburgh","Oakland","Tampa",
             "Atlanta","Washington","San.F","San.I","Seattle")
numero_bateos <- c(5659,  5710, 5563, 5672, 5532, 5600, 5518, 5447, 5544, 5598,
                   5585, 5436, 5549, 5612, 5513, 5579, 5502, 5509, 5421, 5559,
                   5487, 5508, 5421, 5452, 5436, 5528, 5441, 5486, 5417, 5421)
runs <- c(855, 875, 787, 730, 762, 718, 867, 721, 735, 615, 708, 644, 654, 735,
          667, 713, 654, 704, 731, 743, 619, 625, 610, 645, 707, 641, 624, 570,
          593, 556)
datos <- data.frame(equipos,numero_bateos,runs)
head(datos)

#represrntacion grafica 

library(ggplot2)
ggplot(data = datos, mapping = aes(x = numero_bateos, y = runs)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Diagrama de dispersión', x  =  'número  de bateos') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# creacion de modelo 

modelo_lineal <- lm(runs ~ numero_bateos, datos)

summary(modelo_lineal)

confint(modelo_lineal)

ggplot(data = datos, mapping = aes(x = numero_bateos, y = runs)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Diagrama de dispersión', x  =  'número  de bateos') +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#analisis del modelo 

# creamos variables del modelo 

datos$prediccion <- modelo_lineal$fitted.values
datos$residuos   <- modelo_lineal$residuals
head(datos)

#representacion grafica del modelo como predictor 

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# evaluacion de normalidad de residuos 

ggplot(data = datos, aes(x = residuos)) +
  geom_histogram(aes(y = ..density..)) +
  labs(title = "histograma de los residuos") +
  theme_light()

qqnorm(modelo_lineal$residuals)
qqline(modelo_lineal$residuals)

shapiro.test(modelo_lineal$residuals)

#analsiis de Homocedasticidad

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  geom_smooth(se = FALSE, color = "firebrick") +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Test de Breush-Pagan

library(lmtest)
bptest(modelo_lineal)
