#Lectura de datos
library(tidyverse)
library (data.table)
library (dplyr)

base <- data.table(read.csv("data/raw/concentradohogar.csv"))

#Ejercicios ####

#Número 1
vivienda <- n_distinct (base$folioviv)

#Numero 2
base %>% distinct(folioviv, foliohog, .keep_all = T) %>% dim()

#Numero 3
if (87826 - dim(base)[1] != 0){
  "No coincide con el tamaño de la muestra"
}

#Numero 4
cortes <- seq(0, 1, by = 0.1)
deciles <- base %>% 
  summarise(valor = quantile(ing_cor, cortes), 
            decil = paste0('d', cortes * 10))
deciles %>% 
  relocate(decil) %>% 
  arrange(desc(valor),decil)

#Numero 5
concentrado <- read_csv('data/raw/concentradohogar.csv',
                        col_types = cols(folioviv = col_number()))
x <- lapply(base, class) %>% unlist(use.names = TRUE)
y <- lapply(concentrado, class) %>% unlist(use.names = TRUE)
cambiaron <- sum(ifelse(x==y, 0,1))

#Numero 6
base %>% select(ingtrab,rentas,transfer, estim_alqu, otros_ing, ing_cor) %>%
  mutate(Ingreso= round(ingtrab+rentas+transfer+estim_alqu+otros_ing,2))%>%
  mutate(Comprobacion= Ingreso==ing_cor)
#Numero 7
base%>% select (folioviv, foliohog, hombres, mujeres, ing_cor, 
                perc_ocupa) %>% tibble() %>% head()
