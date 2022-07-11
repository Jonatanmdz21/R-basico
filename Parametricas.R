####Pruebas parametricas ###

### T-test 

##homocedasticidad

library(car)

#fligner

fligner.test(y ~x,datos)

#levene

leveneTest(y ~ x,datos, center = "median")


# muestras no pareadas 

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

## una via 

anova <- aov(datos$respuesta ~ datos$grupos)

Summary (anova)

# efecto

etaSquared(anova)


# comparacion multiple 

TukeyHSD(anova)

plot(TukeyHSD(anova)) 

pairwise.t.test(x = datos$respuesta, g = datos$grupos, p.adjust.method = "holm",
                pool.sd = TRUE, paired = FALSE, alternative = "two.sided") 


## ANOVA pareada 

datos <- as.matrix((datos[-1]) )

modelo_lm <- lm(datos ~ 1)

grupos <- factor(c("grupo_A", "grupo_B", "grupo_C", "grupo_D")) 


library(car) 

anova_pareado <- Anova(modelo_lm, idata = data.frame(tienda), idesign = ~ tienda, type = "III") 


summary(anova_pareado)

##  comparacion multiple 

pairwise.t.test(x = datos_tabla_larga$respuesta, g = datos_tabla_larga$grupos, 
                p.adjust.method = "holm", paired = TRUE, alternative = "two.sided") 






