#######################
# Boosting
#######################


# Cargar datos y eliminar columnas que no son predictores ------------------------------------------------------------
require(pacman)
p_load("tidymodels", "glmnet", "doParallel", "xgboost", 
       "lightgbm", 
       "bonsai", "pROC")

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

# Regression -------------------------------------------------------------------

# BOOSTING  --------------------------------------------------------------------

rec_boost_select <-
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

#Model

boost <- boost_tree(
  tree_depth = tune(),
  trees = tune(),
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("lightgbm", nthreads = 8) %>%
  set_mode("regression")

tune_grid_boost <- grid_regular(
  tree_depth(range = c(5,20)),
  mtry(range = c(5, 20)),
  trees(range = c(1000, 2000)),
  learn_rate(), 
  levels = c(5,5,2,3)
)

#Workflow

workflow_boost <- workflow() %>%
  add_recipe(rec_boost_select) %>%
  add_model(boost)

#Tune grid
registerDoParallel()
tune_boost <- workflow_boost %>% 
  tune_grid(resamples = kfolds,
            grid = tune_grid_boost,
            metrics = metric_set(rmse),
            control=control_grid(verbose=TRUE))
stopImplicitCluster()
stopImplicitCluster()

metrics_boost <- tune_boost %>%
  collect_metrics()


#ggsave("views/boost_trees.png", plot_trees, dpi=300)
#ggsave("views/boost_learnrate.png", plot_learn_rate, dpi=300)
#ggsave("views/boost_min_n.png", plot_min_n_boost, dpi=300)

#Best Fit, Training 

best_boost <- select_best(tune_boost, metric = "rmse")
boost_final <- finalize_workflow(workflow_boost, best_boost)
boost_final_fit <- fit(boost_final, data = train_reg)

#Predict
boost_predictions <- predict(boost_final_fit, test) %>%
  bind_cols(test$id) %>%
  bind_cols(test$Lp) %>%
  rename(c("id"="...2", "Lp" = "...3")) %>%
  mutate(Ingpcug=exp(.pred)) %>%
  mutate(pobre = ifelse(Ingpcug<=Lp,1,0)) %>%
  select(-c(.pred, Ingpcug, Lp)) #%>%
  #replace_na(list(pobre = 0))


## Exportar

write_csv(boost_predictions, "../stores/02_submissions/regression_boost.csv")


# Clasificacion -----------------------------------------------------------

train_clas <- train_sin_bog %>%
  select(-c(Lp, Ingpcug, lIngpcug, train, Clase))

#Validation set para train sin Bogotá
set.seed(123)

validation_set <- train_sin_bog %>% 
  sample_frac(0.15)

validation_train <- train_clas %>% 
  anti_join(validation_set, by = "id") %>%
  select(-id)

validation_train <- validation_train %>%
  mutate(Pobre = ifelse(Pobre ==1, "si", "no")) %>%
  mutate(Pobre = factor(Pobre))
validation_train$Pobre <- relevel(validation_train$Pobre, ref = "si")

validation_set <- validation_set %>%
  mutate(Pobre = ifelse(Pobre ==1, "si", "no")) %>%
  mutate(Pobre = factor(Pobre))
validation_set$Pobre <- relevel(validation_set$Pobre, ref = "si")

kfolds <- vfold_cv(validation_train, v = 5)

#Modelo

rec_boost_select <-
  recipe(Pobre ~ ., data = validation_train) %>% 
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

#Model

boost <- boost_tree(
  tree_depth = tune(),
  trees = tune(),
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("lightgbm", nthreads = 8) %>%
  set_mode("classification")

tune_grid_boost <- grid_regular(
  tree_depth(range = c(5,20)),
  mtry(range = c(5, 20)),
  trees(range = c(1000, 2000)),
  learn_rate(), 
  levels = c(3,3,2,3)
)
#Workflow

workflow_boost <- workflow() %>%
  add_recipe(rec_boost_select) %>%
  add_model(boost)

#Tune grid
registerDoParallel()
tune_boost <- workflow_boost %>% 
  tune_grid(resamples = kfolds,
            grid = tune_grid_boost,
            metrics = metric_set(f_meas),
            control=control_grid(verbose=TRUE))

metrics_boost <- tune_boost %>%
  collect_metrics()
stopImplicitCluster()
stopImplicitCluster()

#ggsave("views/boost_trees.png", plot_trees, dpi=300)
#ggsave("views/boost_learnrate.png", plot_learn_rate, dpi=300)
#ggsave("views/boost_min_n.png", plot_min_n_boost, dpi=300)

#Best Fit, Training 

best_boost <- select_best(tune_boost, metric = "f_meas")
boost_final <- finalize_workflow(workflow_boost, best_boost)
boost_final_fit <- fit(boost_final, data = validation_train)


#Predict en validation
predictions_real_df <- predict(boost_final_fit, validation_set, type = "prob")$.pred_si %>%
  bind_cols(validation_set$Pobre) %>%
  rename(c("real" = "...2", "predictions"="...1"))

roc_thresh_boost <- ROC_function(predictions_real_df)

#Predict
boost_predictions_class <- predict(boost_final_fit, test, type = "prob")$.pred_si %>%
  bind_cols(test$id) %>%
  rename(c("id"="...2")) %>%
  mutate(pobre = ifelse(...1>=roc_thresh_boost$threshold, 1, 0)) %>%
  select(-c(...1)) %>%
  replace_na(list(pobre = 0))

## Exportar
write_csv(boost_predictions_class, "../stores/02_submissions/classification_boost.csv")


