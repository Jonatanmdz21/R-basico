#####INTRODUCCION A TIDYVERSE####

##descarga y carga de packages

install.packages("tidyverse")


library(tidyverse)


###DPLYR

#arrange 

arrange(gapminder, year, desc(pop)) %>% head()

#cout 


gapminder %>% count(continent, sort = TRUE, name="registros")


#filter 

gapminder %>% filter(year == 2007) %>% head()

america <- gapminder %>% filter(year == 2007, continent=="Americas") 
america

#group_by

gapminder %>% 
  group_by(year) %>%
  summarize(meanLifeExp = signif(mean(lifeExp), digits=4),
            totalPop = sum(as.numeric(pop))
  )


#mutate 

gapminder %>%  mutate(pop = pop / 1000000, gdp = gdpPercap*pop) %>% head()

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

#gather 



#spread 

