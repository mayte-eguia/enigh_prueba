#Lectura de datos
library(tidyverse)
library(data.table)
library(dplyr)

concentrado <- read_csv("data/raw/concentradohogar.csv")

#Ejercicios ####
#Número 1
vivienda <- concentrado %>% 
  summarise(total = n_distinct(folioviv))

#Numero 2
concentrado %>%
  distinct(folioviv, foliohog, .keep_all = T) %>% 
  summarise(total = n())
 
#Numero 3
grupos_viv_hog <- concentrado %>%
  distinct(folioviv, foliohog, .keep_all = T) %>% 
  summarise(total = n()) %>% 
  pull(total)

#Numero 4
cortes <- seq(0, 1, by = 0.1)
deciles <- base %>% 
  summarise(valor = quantile(ing_cor, cortes), 
            decil = paste0('d', cortes * 10))

deciles %>% 
  mutate(decil = fct_reorder(decil, valor)) %>% 
  arrange(desc(decil)) %>% 
  relocate(decil)


#Numero 5
concentrado <- read_csv('data/raw/concentradohogar.csv',
                        col_types = cols(folioviv = col_number()))

x <- lapply(concentrado, class) %>% unlist(use.names = TRUE)
# Esto es equivalente a:
sapply(concentrado, class)

y <- lapply(concentrado, class) %>% unlist(use.names = TRUE)

# Un método más tidy sería:
concentrado %>% 
  summarise(across(everything(), class)) %>% 
  pivot_longer(everything(), 
               names_to = "variable",
               values_to = "clase")

cambiaron <- sum(ifelse(x==y, 0,1))

# La respuesta es que está leyendo como numérico y eso 
# le quita los ceros iniciales
# Esto lo soluciona:
concentrado <- read_csv('data/raw/concentradohogar.csv')


#Numero 6
concentrado %>%
    select(folioviv, foliohog, ing_cor,
           ingtrab, rentas, transfer, estim_alqu, otros_ing) %>%
    pivot_longer(cols = ingtrab:otros_ing) %>%
    group_by(folioviv, foliohog, ing_cor) %>%
    summarise(valor = sum(value)) %>%
    filter(as.integer(ing_cor) != as.integer(valor))

#Numero 7
base %>%
  select (folioviv, foliohog, hombres, mujeres, ing_cor, 
                perc_ocupa) %>%
  tibble() %>%
  head()
