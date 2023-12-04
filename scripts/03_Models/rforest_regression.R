#######################
# Boosting
#######################


# Cargar datos y eliminar columnas que no son predictores ------------------------------------------------------------
require(pacman)
p_load("tidymodels", "glmnet", "doParallel", "xgboost", 
       "lightgbm", 
       "bonsai", "pROC", "ranger")

require(pacman)
p_load(tidymodels, tidyverse)
source("03_Models/ROC_function.R")

# Cargar datos y eliminar columnas que no son predictores ------------------------------------------------------------

train <- readRDS("../stores/train_final.rds")
train_sin_bog <- readRDS("../stores/train_final_sin_bog.rds")
test <- readRDS("../stores/test_final.rds")

#Seleccion basada en Lasso
coefs_lasso <- read_csv("../stores/01_var_exploration/coefs_lasso.csv")

#Seleccionar variables
remove_lasso <- coefs_lasso %>%
  filter(estimate == 0) %>%
  select(term)
remove_lasso <- remove_lasso$term

#Organizar
train_reg <- train_sin_bog %>%
  select(-c(Lp, Ingpcug, Pobre, id, train, Clase))

# Folds -------------------------------------------------------------------
set.seed(123)
kfolds <- vfold_cv(train, v = 5)

#Model
rf_reg<- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger") %>%
  set_mode("regression")       # Cambiar a modo de regresión

random_forest_grid_reg <- grid_regular(mtry(range = c(20, 35)),
                                       min_n(),
                                       trees(range = c(250, 500)),
                                       levels = c(5,5,2))

#Receta, con todas las variables

rec_forests_reg <-
  recipe(lIngpcug ~ ., data = train_reg) %>% 
  step_dummy(all_of(c("Dominio")), -all_outcomes()) %>% 
  step_interact(terms = ~ P5130:starts_with("Depto")) %>%
  step_interact(terms = ~ Educ_avg:starts_with("Depto")) %>%
  step_interact(terms = ~ tasa_ocupados:starts_with("Depto")) %>%
  step_interact(terms = ~ tasa_inactivos:starts_with("Depto")) %>%
  step_interact(terms = ~ P5000:starts_with("Depto")) %>%
  step_interact(terms = ~ edad_pet:starts_with("Depto")) %>%
  step_rm(any_of(remove_lasso)) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes()) %>%
  step_pca(all_numeric_predictors(), threshold = 0.8)

#Workflow

workflow_random_forest_reg <- workflow() %>%
  add_recipe(rec_forests_reg) %>%
  add_model(rf_reg)

#Tune grid

tune_rf_reg <- tune_grid(
  workflow_random_forest_reg, 
  resamples = kfolds,
  grid = random_forest_grid_reg,
  metrics = metric_set(rmse),
  control=control_grid(verbose=TRUE))


metrics_boost <- tune_rf_reg %>%
  collect_metrics()

best_rfreg <- select_best(tune_rf_reg, metric = "mae")
rfreg_final <- finalize_workflow(workflow_random_forest_reg, best_rfreg)
rfreg_final_fit <- fit(rfreg_final, data = train_reg)



# Faster ------------------------------------------------------------------

#Preprocess train
train_reg_baked <- prep(rec_forests_reg, train_reg) %>%
  bake(new_data = train_reg)

#Preprocess test
test_reg_baked <- prep(rec_forests_reg, train_reg) %>%
  bake(new_data = test)


model_rf <- ranger(lIngpcug ~., data = train_reg_baked, mtry = 15, 
                   num.trees = 500, min.node.size = 5, splitrule = "variance")

myControl <- trainControl(
   method = "cv", number = 5,repeats = 5, verboseIter = TRUE)

model_rf <- train(lIngpcug ~., data = train_reg_baked, method='ranger', 
                  trControl = myControl, tuneGrid = test_grid_reg)


rf_predictions <- predict(model_rf, test_reg_baked) 

rf_predictions_df <- rf_predictions$predictions %>%
  bind_cols(test$id) %>%
  bind_cols(test$Lp) %>%
  rename(c("id"="...2", "Lp" = "...3")) %>%
  mutate(Ingpcug=exp(...1)) %>%
  mutate(pobre = ifelse(Ingpcug<=Lp,1,0)) %>%
  select(-c(...1, Ingpcug, Lp)) 

summary(rf_predictions_df)

write_csv(rf_predictions_df, "../stores/02_submissions/regression_randomforest.csv")
