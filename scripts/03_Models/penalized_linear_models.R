##############################################################
#       Big Data y Machine Learning                          #
#       Taller 2: Lasso/Ridge/Elastic Net.                   #
##############################################################

# Cargar datos y eliminar columnas que no son predictores ------------------------------------------------------------
require(pacman)
p_load("tidymodels", "glmnet", "doParallel")

require(pacman)
p_load(tidymodels, tidyverse)

# Cargar datos y eliminar columnas que no son predictores ------------------------------------------------------------

train <- readRDS("../stores/train_final.rds")
train_sin_bog <- readRDS("../stores/train_final_sin_bog.rds")
test <- readRDS("../stores/test_final.rds")

train_reg <- train %>%
  select(-c(Lp, Ingpcug, Pobre, Dominio, id, train, Clase))

# Folds -------------------------------------------------------------------

kfolds <- vfold_cv(train, v = 5)

# Lasso -------------------------------------------------------------------

#Model
lasso <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lambda_grid_lasso <- grid_regular(penalty(), levels = 50)

#Recipe
lasso_rec <-
  recipe(lIngpcug ~ ., data = train_reg) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ P5130:starts_with("Depto")) %>%
  step_interact(terms = ~ Educ_avg:starts_with("Depto")) %>%
  step_interact(terms = ~ tasa_ocupados:starts_with("Depto")) %>%
  step_interact(terms = ~ tasa_inactivos:starts_with("Depto")) %>%
  step_interact(terms = ~ P5000:starts_with("Depto")) %>%
  step_interact(terms = ~ edad_pet:starts_with("Depto")) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
lasso_wflow <- 
  workflow() %>% 
  add_recipe(lasso_rec) %>% 
  add_model(lasso) 

#Tune grid

tune_result <- lasso_wflow %>% 
  tune_grid(resamples = kfolds,
            grid = lambda_grid_lasso,
            metrics = metric_set(rmse),
            control=control_grid(verbose=TRUE))

results_tuning_lasso <- tune_result %>%
  collect_metrics()

# Plot
plot_lasso_penalty<- results_tuning_lasso %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(linewidth = 1) +
  ylab("RMSE") +
  xlab("Penalidad (lambda)") + 
  theme_bw() +
  theme(legend.position = "none") 

#Best fit 
tune_best <- tune_result %>% 
  select_best(metric = "rmse")

best_lasso <- linear_reg(penalty = tune_best$penalty, mixture = 1) %>%
  set_engine("glmnet")

best_lasso_wflow <- 
  workflow() %>% 
  add_recipe(lasso_rec) %>% 
  add_model(best_lasso) 

best_lasso_fit <- 
  best_lasso_wflow %>% 
  fit(train_reg)


## Es interesante ver las variables que quedaron seleccionadas
coefs_lasso <- best_lasso_fit %>%
  pull_workflow_fit() %>%
  tidy()

#Predict
lasso_predictions <- predict(best_lasso_fit, test) %>%
  bind_cols(test$id) %>%
  bind_cols(test$Lp) %>%
  rename(c("id"="...2", "Lp" = "...3")) %>%
  mutate(Ingpcug=exp(.pred)) %>%
  mutate(pobre = ifelse(Ingpcug<=Lp,1,0)) %>%
  select(-c(.pred, Ingpcug, Lp)) %>%
  replace_na(list(pobre = 0))

## Exportar
write_csv(lasso_predictions, "../stores/02_submissions/regression_lasso.csv")


# Ridge -------------------------------------------------------------------

#Model
ridge <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet")

lambda_grid_ridge <- grid_regular(penalty(), levels = 50)

#Recipe
ridge_rec <-
  recipe(lIngpcug ~ ., data = train_reg) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
ridge_wflow <- 
  workflow() %>% 
  add_recipe(ridge_rec) %>% 
  add_model(ridge) 

#Tune grid

tune_result_ridge <- ridge_wflow %>% 
  tune_grid(resamples = kfolds,
            grid = lambda_grid_ridge,
            metrics = metric_set(rmse),
            control=control_grid(verbose=TRUE))

results_tuning_ridge <- tune_result_ridge %>%
  collect_metrics()

# Plot
plot_ridge_penalty<- results_tuning_ridge %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(linewidth = 1) +
  ylab("RMSE") +
  xlab("Penalidad (lambda)") + 
  theme_bw() +
  theme(legend.position = "none") 

#Best fit 
tune_best <- tune_result %>% 
  select_best(metric = "rmse")

best_ridge <- linear_reg(penalty = tune_best$penalty, mixture = 1) %>%
  set_engine("glmnet")

best_ridge_wflow <- 
  workflow() %>% 
  add_recipe(ridge_rec) %>% 
  add_model(best_ridge) 

best_ridge_fit <- 
  best_ridge_wflow %>% 
  fit(train_reg)


## Es interesante ver las variables que quedaron seleccionadas
coefs_ridge <- best_ridge_fit %>%
  pull_workflow_fit() %>%
  tidy()

#Predict
ridge_predictions <- predict(best_ridge_fit, test) %>%
  bind_cols(test$id) %>%
  bind_cols(test$Lp) %>%
  rename(c("id"="...2", "Lp" = "...3")) %>%
  mutate(Ingpcug=exp(.pred)) %>%
  mutate(pobre = ifelse(Ingpcug<=Lp,1,0)) %>%
  select(-c(.pred, Ingpcug, Lp)) %>%
  replace_na(list(pobre = 0))

## Exportar
write_csv(ridge_predictions, "../stores/02_submissions/regression_ridge.csv")

# Elastic Net -------------------------------------------------------------

#Model
elasticnet <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

lambda_grid_elasticnet <- grid_regular(penalty(), mixture(), levels = c(30, 30))

#Recipe
elasticnet_rec <-
  recipe(lIngpcug ~ ., data = train_reg) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
elasticnet_wflow <- 
  workflow() %>% 
  add_recipe(elasticnet_rec) %>% 
  add_model(elasticnet) 

#Tune grid
registerDoParallel()
tune_result <- elasticnet_wflow %>% 
  tune_grid(resamples = kfolds,
            grid = lambda_grid_elasticnet,
            metrics = metric_set(rmse),
            control=control_grid(verbose=TRUE))
stopImplicitCluster()
results_tuning_elasticnet <- tune_result %>%
  collect_metrics()

#Best fit 
tune_best <- tune_result %>% 
  select_best(metric = "rmse")

best_elasticnet <- linear_reg(penalty = tune_best$penalty, mixture = tune_best$mixture) %>%
  set_engine("glmnet")

best_elasticnet_wflow <- 
  workflow() %>% 
  add_recipe(elasticnet_rec) %>% 
  add_model(best_elasticnet) 

best_elasticnet_fit <- 
  best_elasticnet_wflow %>% 
  fit(train_reg)

## Es interesante ver las variables que quedaron seleccionadas
coefs_elasticnet <- best_elasticnet_fit %>%
  pull_workflow_fit() %>%
  tidy()

elasticnet_predictions <- predict(best_elasticnet_fit, test) %>%
  bind_cols(test$id) %>%
  bind_cols(test$Lp) %>%
  rename(c("id"="...2", "Lp" = "...3")) %>%
  mutate(Ingpcug=exp(.pred)) %>%
  mutate(pobre = ifelse(Ingpcug<=Lp,1,0)) %>%
  select(-c(.pred, Ingpcug, Lp)) %>%
  replace_na(list(pobre = 0))

## Exportar
write_csv(elasticnet_predictions, "../stores/02_submissions/regression_elasticnet.csv")



