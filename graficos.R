#### graficos Mex ###

# librerias 

library(tidyverse)


## importancias de extraccion 

ggplot(
  data = extract,
  aes(x    = reorder(var, Extraction),
      y    = Extraction*100,
      fill = Extraction)
) +
  labs(x = "variable", title = "Variable extraction ", y='extraction percentage') +
  geom_col() +
  scale_fill_viridis_c() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")+
  scale_fill_gradient(low ="red", high = "green" )
 