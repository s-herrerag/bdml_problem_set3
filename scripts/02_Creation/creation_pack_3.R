################################
# Variables nuevas: Paquete 3
################################

# Libraries ---------------------------------------------------------------
library(pacman)
p_load(tidyverse)


# Variables ---------------------------------------------------------------
vars_1_3 <- read_csv("../stores/01_var_exploration/vars_join_v0.csv")
vars_pack_3 <- c("P7495","P7500s2", "P7500s3", "P7505", "P7510s1", "P7510s2", 
                   "P7510s3", "P7510s5", "P7510s6", "P7510s7", "Pet", "Oc", "Des", "Ina")


# NA Pattern  -----------------------------------------------------------------

df_vars <- vars_1_3 %>%
  select(all_of(c("id", "Orden", "Clase", "P6040", "P6050", vars_pack_3)))

nas_vars_interest <- data.frame(colSums(is.na(df_vars)) / nrow(df_vars))

#First family of variables: P7500s1, P7500s2, P7500s3 (depend on P7495!!!)
P7500s1 <- df_vars  %>%
  filter(P7495==2)

#Mirar jefes de hogar
jefes_hogar <- df_vars %>%
  filter(P6050 ==1)
nas_jefes_hogar <- data.frame(colSums(is.na(jefes_hogar)))

# Second family of variables: "P7510s1", "P7510s2", "P7510s3", "P7510s5", "P7510s6", "P7510s7" (depend on P7505)


# Funcion para tratar paquete 3 y retornar variables a nivel hogar -------------------------------------------

treatment_pack_3 <- function(df_vars){
  
  vars_pack_3 <- c("P7495","P7500s2", "P7500s3", "P7505", "P7510s1", "P7510s2", 
                   "P7510s3", "P7510s5", "P7510s6", "P7510s7", "Pet", "Oc", "Des", "Ina")
  
  df_vars <- df_vars %>%
    select(c("id", "Orden", "Clase", "P6040", "P6050", vars_pack_3))
  
  #Replace 2 
  df_vars <- df_vars %>%
    mutate(across(all_of(vars_pack_3), ~ifelse(.==2 | .==9, 0, .)))
  
  # Unidad de gasto para las preguntas
  
  df_unidad_gasto <- df_vars %>%
    filter(!P6050%in%c(6,7,8,9)) %>%
    group_by(id) %>%
    mutate(size_unidad_gasto = n())
  
  ## 1) Crear variables relacionadas con personas fuera de la unidad de gasto
  
  # Servicio doméstico y trabajadores
  servicio <- df_vars %>%
    mutate(servicio_trabajadores = ifelse(P6050%in%c(6,8), 1, 0)) %>%
    group_by(id) %>%
    summarise(servicio_trabajadores = sum(servicio_trabajadores, na.rm = T)) %>%
    select(id, servicio_trabajadores) %>%
    unique()
  
  # Arriendos y pensiones
  df_unidad_gasto <- df_unidad_gasto %>%
    group_by(id) %>%
    mutate(pensiones = ifelse(sum(P7500s2, na.rm = T)>0, 1, 0),
           pension_alimenticia = ifelse(sum(P7500s3, na.rm = T)>0, 1, 0), 
           #2a:
           remesas_internas = sum(P7510s1, na.rm = T)/size_unidad_gasto,
           remesas_externas = sum(P7510s2, na.rm = T)/size_unidad_gasto,
           ayuda_instituciones = sum(P7510s3, na.rm = T)/size_unidad_gasto,
           inversiones = sum(P7510s5, na.rm = T)/size_unidad_gasto,
           cesantias = ifelse(sum(P7510s6, na.rm = T)>0, 1, 0),
           otros_ingresos = ifelse(sum(P7510s7, na.rm = T)>0, 1, 0)
    )
  
  # Características del trabajo en el hogar
  
  caracteristicas_hogares <- df_unidad_gasto %>%
    select(c(id, P6040, Pet, Oc, Des, Ina)) %>%
    replace_na(list(Pet = 0, Oc = 0, Des = 0, Ina = 0)) %>%
    group_by(id) %>%
    summarise(prop_pet = mean(Pet),
              edad_pet = mean(P6040[Pet == 1], na.rm = T),
              tasa_ocupados = mean(Oc[Pet == 1], na.rm = T),
              tasa_inactivos = mean(Ina[Pet == 1], na.rm = T)) %>%
    unique()
  
  # Seleccionar variables útiles y dejarlas al nivel de hogar
  
  variables_hogar_finales <- df_unidad_gasto %>%
    select(id, Clase, pensiones, pension_alimenticia, remesas_internas,
           remesas_externas, ayuda_instituciones, inversiones, cesantias, otros_ingresos) %>%
    left_join(caracteristicas_hogares) %>%
    left_join(servicio) %>%
    unique()
  
  ########### RETURN ###########
  variables_hogar_finales
}


# Aplicacion a train y test -----------------------------------------------

train_hogares <- read_csv("/Users/santiagoherreragarcia/GitHub/BDML/bdml_problem_set_3/stores/raw/train_hogares.csv")
train_personas <- read_csv("/Users/santiagoherreragarcia/GitHub/BDML/bdml_problem_set_3/stores/raw/train_personas.csv")

test_hogares <- read_csv("/Users/santiagoherreragarcia/GitHub/BDML/bdml_problem_set_3/stores/raw/test_hogares.csv")
test_personas <- read_csv("/Users/santiagoherreragarcia/GitHub/BDML/bdml_problem_set_3/stores/raw/test_personas.csv")

# Funciones a personas

train_personas_pack_3 <- treatment_pack_3(train_personas)
test_personas_pack_3 <- treatment_pack_3(test_personas)


# Exportar ----------------------------------------------------------------

write_csv(train_personas_pack_3, "../stores/train_pack_3.csv")
write_csv(test_personas_pack_3, "../stores/test_pack_3.csv")






  