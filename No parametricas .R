### No parametricas x

##prueba de signos 

library(BSDA)

efectividad<-c(0.41,0.68,0.52,0.82,0.45,0.78,0.96,0.91,0.75) #creamos valores 

#corremos la prueba 

SIGN.test(efectividad,md=0.9,alternative = "two.sided",conf.level = 0.95)

SIGN.test(efectividad,md=0.9,alternative = "greater",conf.level = 0.95)

SIGN.test(efectividad,md=0.9,alternative = "less",conf.level = 0.95)

# muestras pareadas 

antes<-c(18,19,11,3,5,3)
despues<-c(10,16,7,4,7,2) #Creamos variables 

SIGN.test(antes,despues,alternative = "t",conf.level = 0.95)

SIGN.test(antes,despues,alternative = "g",conf.level = 0.95)

SIGN.test(antes,despues,alternative = "l",conf.level = 0.95)


##Prueba de rangos de signos de Wilcoxon

library(MASS)

VES<-c(80,78,78,77,76,76,88,89,89,90,95,65,60,60,56,56,50,45) #Creamos variables 

#corremos prueba 

wilcox.test(VES,mu=80,exact=T,alternative = "t",conf.int = 0.95)

wilcox.test(VES,mu=80,exact=F,correct = T, alternative = "t", conf.int = 0.95)

#muestras pareadas 

manana<-c(201,182,191,188,188,174)
noche<-c(203,178,186,183,181,165) #construimos variables 

#corremos prueba 

wilcox.test(manana,noche,paired = T,exact = T, correct = F,  conf.int = 0.95)

wilcox.test(manana,noche,paired = T,exact = F,correct = T,conf.int = 0.95)

# Prueba U de Mann Whitney - Wilcoxon


library(exactRankTests)
actual<-c(68,69,72,78,79,80,84)
nuevo<-c(60,64,68,70,72,73) #construimos variables 

#corremos codigo 

wilcox.exact(actual,nuevo,exact = T, alternative = "t",conf.int = 0.95)

##Kruskall-Wallis

indigenas<-c(78,79,89,79,81,83,95)  
gitanos<-c( 67, 78, 78, 79,80)          
negros<-c(95,99,89,87,90,92)        
otros<-c(78,80,81,78,67,70,71,73)

kruskal.test(list(indigenas,gitanos,negros,otros))

kruskal.test(y~x, data)

# prueba post hoc

posthoc.kruskal.dunn.test(list(indigenas,gitanos,negros,otros), p.adjust.method = "holm")


#pruebas de independencia x2 

#cargamos base y limpiamos NA 

lesiones=lesiones%>%na.omit(race)


#chi cuadrada 

table(lesiones$sex, lesiones$race)


chisq.test(lesiones$sex, lesiones$race)


#tabla de contingencia 


library(gmodels)

CrossTable(lesiones$sex, lesiones$race,expected = TRUE,
           prop.r = TRUE,prop.c = TRUE,fisher = TRUE)

