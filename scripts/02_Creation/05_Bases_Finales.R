########################
# Creación bases completa
########################


# Librerias ---------------------------------------------------------------

library(pacman)
p_load(tidyverse)


# Leer datos --------------------------------------------------------------

train_pack_1 <- read_csv("../stores/01_var_exploration/train_pack_1.csv")
train_pack_2 <- read_csv("../stores/01_var_exploration/train_pack_2.csv")
train_pack_3 <- read_csv("../stores/01_var_exploration/train_pack_3.csv")
train_pack_4 <- read_csv("../stores/01_var_exploration/train_pack_4.csv")

test_pack_1 <- read_csv("../stores/01_var_exploration/test_pack_1.csv")
test_pack_2 <- read_csv("../stores/01_var_exploration/test_pack_2.csv")
test_pack_3 <- read_csv("../stores/01_var_exploration/test_pack_3.csv")
test_pack_4 <- read_csv("../stores/01_var_exploration/test_pack_4.csv")

#Extraer variables de resultado
train_hogares <- read_csv("../stores/raw/train_hogares.csv")
test_hogares <- read_csv("../stores/raw/test_hogares.csv") #Incluir personas por unidad de gasto

# Join -------------------------------------------------------------------

train <- train_pack_1 %>%
  left_join(train_pack_2) %>%
  left_join(train_pack_3) %>%
  left_join(train_pack_4) %>%
  select(-c(Fex_dpto, Fex_c, Li, Nper)) %>%
  drop_na() %>% # 1 missing en pet
  left_join(select(train_hogares, id, Npersug, Ingpcug, Pobre)) %>%
  mutate(lIngpcug = ifelse(Ingpcug >0, log(Ingpcug), 0), #Mejor usar el logaritmo para el ingreso
         P5090 = as.factor(P5090),
         Dominio = as.factor(Dominio),
         Depto = as.factor(Depto)) 
  
test <- test_pack_1 %>%
  left_join(test_pack_2) %>%
  left_join(test_pack_3) %>%
  left_join(test_pack_4) %>%
  select(-c(Fex_dpto, Fex_c, Li, Nper)) %>%
  left_join(select(test_hogares, id, Npersug)) %>%
  mutate(P5090 = as.factor(P5090),
         Dominio = as.factor(Dominio),
         Depto = as.factor(Depto)) %>%
  replace_na(list(edad_pet=0, tasa_ocupados=0, tasa_inactivos=0))
  

# Para incluir dominio, toca sin Bogotá
train_sin_bog <- train %>%
  mutate(Dominio = as.character(Dominio)) %>%
  filter(Dominio != "BOGOTA") %>%
  mutate(Dominio = as.factor(Dominio))


#Exportar

write_csv(train, "../stores/train_final.csv")
write_csv(train_sin_bog, "../stores/train_final_sin_bog.csv")
write_csv(test, "../stores/test_final.csv")

saveRDS(train, "../stores/train_final.rds")
saveRDS(train_sin_bog, "../stores/train_final_sin_bog.rds")
saveRDS(test, "../stores/test_final.rds")

