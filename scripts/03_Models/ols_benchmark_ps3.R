##############################################################
#       Big Data y Machine Learning                          #
#       Taller 3: OLS Benchmark.                             #
##############################################################


# Libraries ---------------------------------------------------------------
require(pacman)
p_load(tidymodels, tidyverse)

# Cargar datos y eliminar columnas que no son predictores ------------------------------------------------------------

train <- readRDS("../stores/train_final.rds")
train_sin_bog <- readRDS("../stores/train_final_sin_bog.rds")
test <- readRDS("../stores/test_final.rds")

train_reg <- train %>%
  select(-c(Lp, Ingpcug, Pobre, Dominio, id, train, Cabecera))
train_sin_bog_reg <- train_sin_bog %>%
  select(-c(Lp, Ingpcug, Pobre, id, train, Cabecera))

# 1) OLS: Con Bogotá ------------------------------------------------------------------

#Recipe
initial_rec <- 
  recipe(lIngpcug ~ ., data = train_reg)

#Modelo
initial_model <- linear_reg() %>%
  step_dummy(all_factor_predictors()) %>%
  step_zv(all_predictors())

#Workflow
initial_wflow <- 
  workflow() %>% 
  add_recipe(initial_rec) %>%
  add_model(initial_model)

#Fit
initial_fit <- initial_wflow %>%
  fit(train_reg)

#Predict
initial_predictions <- predict(initial_fit, test) %>%
  bind_cols(test$id) %>%
  bind_cols(test$Lp) %>%
  rename(c("id"="...2", "Lp" = "...3")) %>%
  mutate(Ingpcug=exp(.pred)) %>%
  mutate(pobre = ifelse(Ingpcug<=Lp,1,0)) %>%
  select(-c(.pred, Ingpcug, Lp)) %>%
  replace_na(list(pobre = 0))

## Exportar
write_csv(initial_predictions, "../stores/02_submissions/03_regression_OLS_completa.csv")


# 2) OLS: Sin Bogotá ------------------------------------------------------------------

#Recipe
initial_rec <- 
  recipe(lIngpcug ~ ., data = train_sin_bog_reg)

#Modelo
initial_model <- linear_reg() %>%
  step_dummy(all_factor_predictors()) %>%
  step_zv(all_predictors())

#Workflow
initial_wflow <- 
  workflow() %>% 
  add_recipe(initial_rec) %>%
  add_model(initial_model)

#Fit
initial_fit <- initial_wflow %>%
  fit(train_sin_bog_reg)

#Predict
initial_predictions <- predict(initial_fit, test) %>%
  bind_cols(test$id) %>%
  bind_cols(test$Lp) %>%
  rename(c("id"="...2", "Lp" = "...3")) %>%
  mutate(Ingpcug=exp(.pred)) %>%
  mutate(pobre = ifelse(Ingpcug<=Lp,1,0)) %>%
  select(-c(.pred, Ingpcug, Lp)) %>%
  replace_na(list(pobre = 0))

## Exportar
write_csv(initial_predictions, "../stores/02_submissions/03_regression_OLS_completa_sin_bog.csv")






