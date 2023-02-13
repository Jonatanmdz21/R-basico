####Pruebas parametricas ###

### T-test 

##homocedasticidad

library(car)

#fligner

fligner.test(y ~x,datos)

#levene

leveneTest(y ~ x,datos, center = "median")


# muestras no pareadas (tabla ancha )

t.test(x = datos$grupos, y = datos$respuesta, 
       alternative = "two.sided", paired = FALSE, conf.level = 0.95) 


# muestras pareadas

t.test(x = datos$grupos, y = datos$respuesta, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)


### ANOVA 


##homocedasticidad

library(car)

#fligner

fligner.test(y ~x,datos)

#levene

leveneTest(y ~ x,datos, center = "median")

## una via (tabla larga)

anova <- aov(sietes$Magnitud ~ sietes$Mes)

summary (anova)

# efecto

library(lsr)

etaSquared(anova)


# comparacion multiple 

TukeyHSD(anova)

plot(TukeyHSD(anova)) 

pairwise.t.test(x = atletas$calorias, g = atletas$deportista, p.adjust.method = "holm",
                pool.sd = TRUE, paired = FALSE, alternative = "two.sided") 


## ANOVA pareada 

datos <- as.matrix((base[-6]) )

modelo_lm <- lm(datos ~ 1)

grupos <- factor(c("grupo_A", "grupo_B", "grupo_C", "grupo_D", "grupo_E")) 


library(car) 

anova_pareado <- Anova(modelo_lm, idata = data.frame(grupos), idesign = ~ grupos, type = "III") 


summary(anova_pareado)

##  comparacion multiple 

pairwise.t.test(x = datos_tabla_larga$respuesta, g = datos_tabla_larga$grupos, 
                p.adjust.method = "holm", paired = TRUE, alternative = "two.sided") 






