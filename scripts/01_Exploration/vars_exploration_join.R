#######################################
# Union variables seleccionadas: Personas
#######################################


# Libraries ---------------------------------------------------------------

library(pacman)
p_load(tidyverse)

# Data --------------------------------------------------------------------

vars_pack1 <- read_csv("../stores/01_var_exploration/Base_Hector.csv")
vars_pack2 <- read_csv("../stores/01_var_exploration/variables_paquete_2.csv")
vars_pack3 <- read_csv("../stores/01_var_exploration/variables_paquete_3.csv")

#vars_pack2
vars_pack2$id <- vars_pack1$id
vars_pack2$Orden <- vars_pack1$Orden

# Join --------------------------------------------------------------------
vars_join <- vars_pack1 %>%
  left_join(vars_pack2) %>%
  left_join(vars_pack3)


# NAs ---------------------------------------------------------------------

#Drop all NA

nas_all <- rowSums(is.na(vars_join)) # NAs siguen una lógica

nas_join <- t(colSums(is.na(vars_join)) *100 / dim(vars_join)[1])

#Inactivos - Ocupados
table(vars_join$Oc) 
table(vars_join$Des)
table(vars_join$Ina)
table(vars_join$Pet)

#Pet

pet_na <- vars_join %>%
  filter(is.na(Pet))

table(vars_join$P6090)


## Replacements

vars_join_full <- vars_join %>%
  replace_na(list(Oc = 0, Des = 0, Ina = 0, Pet = 0)) %>%
  mutate(P6090 = ifelse(P6090 == 9, 2, P6090))









