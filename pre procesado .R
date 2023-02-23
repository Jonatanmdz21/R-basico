##### PRE PROCESADO DE DATOS####

###carga de librerias 

library(tidyverse)
library(nortest)

#analisis de base 

str(base)
summary(base)

##visualizacion de comportamiento de variables 

library(GGally)

ggpairs(datos [1:3])

ggpairs(resultados[-1], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


##test de normalidad 

#Kolmogorov-Smirnov
by(data = datos,INDICES = datos$Table_Name,FUN = function(x){ lillie.test(x$vares)})

#Kolmogorov-Smirnov
by(data = datos ,INDICES = datos$Table_Name,FUN = function(x){ shapiro.test(x$vares)})



##tablas 

table(trimes1$Status, trimes1$preterm)

## descriptivas 

data%>%summarise(n=n(),media= mean(var, na.rm = T),DS=sd(var, na.rm = T),
                 max= max(var, na.rm = T), min=min(var, na.rm = T))

trimes1%>%group_by(Status)%>% summarise(n=n(),media= mean(perimetro_cefalico,na.rm = T)
                                        ,DS=sd(perimetro_cefalico, na.rm = T))

##exportar base 

library(writexl)

write_xlsx(datacor1,"/Users/jonatanmendoza/Documents/bases/mexComNO.xlsx")


## nueva var condicional
ext=ext%>%mutate('Socioemocional'=case_when(SE_ESC<7~'Alterado', SE_ESC>=7~'Normal'))

##cambio de nombre de var 

datos=datos%>% rename(newname=oldname)


##sustitucion de na

data <- mutate_at(data, "WG" , ~replace(., is.na(.),mean(WG, na.rm=T)))

## eliminacion de datos 

sismo=sismo %>%filter(Mother %in%c("Healthy", "preeclampsia", "GDM"))

#renombrar var

rename(mext, IL10='IL-10')->mext

#hola mundo

