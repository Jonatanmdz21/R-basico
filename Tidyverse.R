#####. INTRODUCCION A TIDYVERSE. ####

##descarga y carga de packages


library(tidyverse)


###DPLYR

#arrange 

arrange(gapminder,desc(year)) %>% head(50)

arrange(gapminder, year, desc(pop)) -> gapyear 


gap=arrange(gapminder, year, desc(gdpPercap))

#count 


gapminder %>% count(year, sort = TRUE, name="registros")


#filter 

gapminder %>% filter(year <= 2002) -> no2007

gapminder %>% filter(continent %in% c("Americas","Europe" ), year>=1990, 
                         pop >= 29601212 ) %>% count(continent, sort = TRUE, name="registros")

# == dato exacto 
# != todos menos 
# >  mayor que 
# <  menor que 
# >= mayor o igual 
# <= menor o igual 
#  %in% c("Americas" , "Europe" )

america <- gapminder %>% filter(year == 2007, continent=="Americas") 
america

#group_by

gap1=gapminder %>% 
  group_by(continent) %>%
  summarize(meanLifeExp = signif(mean(lifeExp), digits=10),
            totalPop = sum(as.numeric(pop), minpop= max(pop), minpop=min(pop))
  )


 
#mutate 

gap1=gapminder %>%  mutate(pop_pmill = pop / 1000000, gdp =log (gdpPercap*pop))

# + suma 
# - resta 
# * multi
# / div 
# 2^3 potencia 
# sqrt(10) raiz 
# log() natural   log10() logaritmo base 10   logn() siendo n cualquier numero natural 


#rename 

gapEs= gapminder %>% rename(vida=lifeExp, poblacion=pop, pais=country, contiente=continent)


rename_with(gapEs, toupper) %>% head()

#sample n

# recoge n datos aleatorios desde gapminder.

sample_n(gapminder, 25)

# sample frac 

#recoge % de datos 

sample_frac(gapminder, 0.2) 


#select 

select(gapminder, country, year,pop) %>% head()

#sumarize 

gapminder %>%
  filter(year == 2007) %>% 
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop))
  )

#transmutate

gapminder %>%
  transmute(population = pop /1000000, lifeExp) %>% head()

#stringr

library(stringr)

#Genera un caden de texto con separador indicado en argumento collapse.

str_c(gapminder$continent, collapse = ", este es: ") %>% head(10)

#  str_detect()
# Busca la ocurrencia del patron dentro del texto: str_detect(texto, patron) 

str_detect(gapminder$country, "[zy]")

# str_count()
# Contabiliza el número de ocurrencias del patrón dentro de el texto.

y=str_count(gapminder$country, "[zy]")

y

## TIDYR

library(tidyr)

ame= gapminder%>% select(country, continent, year, pop)%>% filter(continent=="Americas")%>% filter(year>=1985)

#spread 



ameL=spread(ame, year, pop)

ameL

#gather 

ameA= gather(ameL, "Anio", "Poblacion", 3:7)


#ggplot 2
library(ggplot2)
library(ggbeeswarm)

ggplot(gapminder, aes(x=continent, y=log(pop)))+geom_boxplot(alpha=0)+
  geom_violin(cex=0.5,bw=0.9,alpha=0.9,aes(color=continent))+
  geom_point(size=10,shape=74,  aes(y=mean(log(pop))))

                

p1=gapminder%>%filter(year>=1985)%>% filter(continent!="Oceania")%>%
  ggplot(aes(x=continent, y=log(pop)))+
  geom_boxplot(alpha=0)+geom_violin(cex=0.1,bw=0.5,alpha=0.2,
                                    aes(color=continent))+
  geom_point(size=10, shape=74, aes(color=continent,y=mean(log(pop))))

p1

##scatter plot 

p2= gapminder%>%filter(year>=1985)%>% filter(continent!="Oceania")%>%
ggplot(aes(lifeExp, log(pop), col=continent))+
  geom_point(size=0.3)+geom_smooth(method = "lm",se=F)+
  facet_grid(year~continent)+theme_minimal()+
  labs(title = 'Estos son los continentes', subtitle = "Te vas a acordar de mi maldito", 
       y="yo soy y", x='yo soy  x', color="pos estos son \n los colores")

# bar plot 

p3=ggplot(gapminder, aes(x=factor(year),fill=continent))+
  geom_bar(position ="dodge" )+
  scale_y_continuous(limits =c(0,75), breaks=seq(10,50, 5))+
  theme_light()+labs(title = 'Años', y='Conteos', x='Año de medicion',
                     fill='Contiente')
library(gridExtra)

grid.arrange(p2, p1, p3, ncol=2, widths=c(5.5, 2.2))



