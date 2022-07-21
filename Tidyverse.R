#####INTRODUCCION A TIDYVERSE####

##descarga y carga de packages


library(tidyverse)


###DPLYR

#arrange 

arrange(gapminder, year, desc(pop)) %>% head()

gap=arrange(gapminder, year, desc(pop))

#cout 


gapminder %>% count(continent, sort = TRUE, name="registros")


#filter 

gapminder %>% filter(year == 2007) %>% head()

gap=gapminder %>% filter(continent %in% c("Americas","Europe" ), year>=1990)

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

gap1 = gapminder %>% 
  group_by(continent) %>%
  summarize(meanLifeExp = signif(mean(lifeExp), digits=4),
            totalPop = sum(as.numeric(pop), minpop= max(pop))
  )


 
#mutate 

gap1=gapminder %>%  mutate(pop_pmill = pop / 1000000, gdp = gdpPercap*pop)

# + suma 
# - resta 
# * multi
# / div 
# 2^3 potencia 
# sqrt(10) raiz 
# log() natural   log10() logaritmo base 10   logn() siendo n cualquier numero natural 


#rename 

rename(gapminder, life=lifeExp, population=pop) %>% head()

rename_with(gapminder, toupper) %>% head()

#sample n

# recoge n datos aleatorios desde gapminder.
sample_n(gapminder, 5)

# sample frac 

#recoge % de datos 

sample_frac(gapminder, 0.2) %>% head()


#select 

select(gapminder, country, year,pop, gdpPercap) %>% head()

#sumarize 

gapminder %>%
  filter(year == 2007) %>% 
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop))
  )

#trasmutate

gapminder %>%
  transmute(population = pop /1000000, lifeExp) %>% head()

#stringr

#Genera un caden de texto con separador indicado en argumento collapse.

str_c(america$country, collapse = ", ")

#  str_detect()
# Busca la ocurrencia del patron dentro del texto: str_detect(texto, patron) 

str_detect(america$country, "[zy]")

# str_count()
# Contabiliza el número de ocurrencias del patrón dentro de el texto.

str_count(america$country, "[zy]")

## TIDYR

ame= gapminder%>% select(country, continent, year, pop)%>% filter(continent=="Americas")%>% filter(year>=2000)

#spread 

ameL=spread(ame, year, pop)

ameL

#gather 

ameA= gather(ameL, "Anio", "Poblacion", 3:4)





