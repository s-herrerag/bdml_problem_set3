##############################################################
#       Big Data y Machine Learning                          #
#       Taller 2: Lasso/Ridge/Elastic Net. Clasificación     #
##############################################################

# Cargar datos y eliminar columnas que no son predictores ------------------------------------------------------------
require(pacman)
p_load("tidymodels", "glmnet", "doParallel", "pROC", "brulee", "discrim", "sparsediscrim")

require(pacman)
p_load(tidymodels, tidyverse)

# Cargar datos y eliminar columnas que no son predictores ------------------------------------------------------------

train <- readRDS("../stores/train_final.rds")
train_sin_bog <- readRDS("../stores/train_final_sin_bog.rds")
test <- readRDS("../stores/test_final.rds")

train_clas <- train_sin_bog %>%
  select(-c(Lp, Ingpcug, lIngpcug, Depto, train, Clase))

#Validation set para train sin Bogotá
set.seed(123)

validation_set <- train_sin_bog %>% 
  sample_frac(0.15)

validation_train <- train_clas %>% 
  anti_join(validation_set, by = "id") %>%
  select(-id)

# Folds -------------------------------------------------------------------

kfolds <- vfold_cv(validation_train, v = 5)

# Lasso -------------------------------------------------------------------

#Model
lasso <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lambda_grid_lasso <- grid_regular(penalty(), levels = 50)

#Recipe
lasso_rec <-
  recipe(Pobre ~ ., data = validation_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ P5130:starts_with("Dominio")) %>%
  step_interact(terms = ~ Educ_avg:starts_with("Dominio")) %>%
  step_interact(terms = ~ tasa_ocupados:starts_with("Dominio")) %>%
  step_interact(terms = ~ tasa_inactivos:starts_with("Dominio")) %>%
  step_interact(terms = ~ P5000:starts_with("Dominio")) %>%
  step_interact(terms = ~ edad_pet:starts_with("Dominio")) %>%
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
  fit(validation_train)


## Es interesante ver las variables que quedaron seleccionadas
coefs_lasso <- best_lasso_fit %>%
  extract_fit_parsnip() %>%
  tidy()

write_csv(coefs_lasso, "../stores/01_var_exploration/coefs_lasso.csv")

# Curva ROC para escoger punto de corte
source("03_Models/ROC_function.R")

#Predict en validation
predictions_real_df_lasso <- predict(best_lasso_fit, validation_set) %>%
  bind_cols(validation_set$Pobre) %>%
  #mutate() %>%
  rename(c("real" = "...2", "predictions"=".pred")) %>%
  mutate(real = as.factor(real))

roc_thresh_lasso <- ROC_function(predictions_real_df_lasso)


# Matriz de confusión 
#confusionMatrix(data = predict_test$class_ROC, reference = predict_test$Pobre)

#Predict
lasso_predictions <- predict(best_lasso_fit, test) %>%
  bind_cols(test$id) %>%
  rename(c("id"="...2")) %>%
  mutate(pobre = ifelse(.pred>=roc_thresh_lasso$threshold, 1, 0)) %>%
  select(-c(.pred)) 

## Exportar
write_csv(lasso_predictions, "../stores/02_submissions/classification_lasso.csv")


# Ridge -------------------------------------------------------------------

#Model
ridge <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet")

lambda_grid_ridge <- grid_regular(penalty(), levels = 50)

#Recipe
ridge_rec <-
  recipe(Pobre ~ ., data = validation_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ P5130:starts_with("Dominio")) %>%
  step_interact(terms = ~ Educ_avg:starts_with("Dominio")) %>%
  step_interact(terms = ~ tasa_ocupados:starts_with("Dominio")) %>%
  step_interact(terms = ~ tasa_inactivos:starts_with("Dominio")) %>%
  step_interact(terms = ~ P5000:starts_with("Dominio")) %>%
  step_interact(terms = ~ edad_pet:starts_with("Dominio")) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
ridge_wflow <- 
  workflow() %>% 
  add_recipe(ridge_rec) %>% 
  add_model(ridge) 

#Tune grid

tune_result <- ridge_wflow %>% 
  tune_grid(resamples = kfolds,
            grid = lambda_grid_ridge,
            metrics = metric_set(rmse),
            control=control_grid(verbose=TRUE))

results_tuning_ridge <- tune_result %>%
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

best_ridge <- linear_reg(penalty = tune_best$penalty, mixture = 0) %>%
  set_engine("glmnet")

best_ridge_wflow <- 
  workflow() %>% 
  add_recipe(ridge_rec) %>% 
  add_model(best_ridge) 

best_ridge_fit <- 
  best_ridge_wflow %>% 
  fit(validation_train)


## Es interesante ver las variables que quedaron seleccionadas
coefs_ridge <- best_ridge_fit %>%
  extract_fit_parsnip() %>%
  tidy()

# Curva ROC para escoger punto de corte
source("03_Models/ROC_function.R")

#Predict en validation
predictions_real_df_ridge <- predict(best_ridge_fit, validation_set) %>%
  bind_cols(validation_set$Pobre) %>%
  #mutate() %>%
  rename(c("real" = "...2", "predictions"=".pred")) %>%
  mutate(real = as.factor(real))

roc_thresh_ridge <- ROC_function(predictions_real_df_ridge)


# Matriz de confusión 
#confusionMatrix(data = predict_test$class_ROC, reference = predict_test$Pobre)

#Predict
ridge_predictions <- predict(best_ridge_fit, test) %>%
  bind_cols(test$id) %>%
  rename(c("id"="...2")) %>%
  mutate(pobre = ifelse(.pred>=roc_thresh_ridge$threshold, 1, 0)) %>%
  select(-c(.pred)) 

## Exportar
write_csv(ridge_predictions, "../stores/02_submissions/classification_ridge.csv")


# Elastic Net -------------------------------------------------------------

#Model
elasticnet <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

lambda_grid_elasticnet <- grid_regular(penalty(), mixture(), levels = c(30, 30))

#Recipe
elasticnet_rec <-
  recipe(Pobre ~ ., data = validation_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ P5130:starts_with("Dominio")) %>%
  step_interact(terms = ~ Educ_avg:starts_with("Dominio")) %>%
  step_interact(terms = ~ tasa_ocupados:starts_with("Dominio")) %>%
  step_interact(terms = ~ tasa_inactivos:starts_with("Dominio")) %>%
  step_interact(terms = ~ P5000:starts_with("Dominio")) %>%
  step_interact(terms = ~ edad_pet:starts_with("Dominio")) %>%
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

results_tuning_elasticnet <- tune_result %>%
  collect_metrics()

stopImplicitCluster()

# Plot
plot_elasticnet_penalty<- results_tuning_elasticnet %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(linewidth = 1) +
  ylab("RMSE") +
  xlab("Penalidad (lambda)") + 
  theme_bw() +
  theme(legend.position = "none") 

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
  fit(validation_train)


## Es interesante ver las variables que quedaron seleccionadas
coefs_elasticnet <- best_elasticnet_fit %>%
  extract_fit_parsnip() %>%
  tidy()

# Curva ROC para escoger punto de corte
source("03_Models/ROC_function.R")

#Predict en validation
predictions_real_df_elasticnet <- predict(best_elasticnet_fit, validation_set) %>%
  bind_cols(validation_set$Pobre) %>%
  #mutate() %>%
  rename(c("real" = "...2", "predictions"=".pred")) %>%
  mutate(real = as.factor(real))

roc_thresh_elasticnet <- ROC_function(predictions_real_df_elasticnet)


# Matriz de confusión 
#confusionMatrix(data = predict_test$class_ROC, reference = predict_test$Pobre)

#Predict
elasticnet_predictions <- predict(best_elasticnet_fit, test) %>%
  bind_cols(test$id) %>%
  rename(c("id"="...2")) %>%
  mutate(pobre = ifelse(.pred>=roc_thresh_elasticnet$threshold, 1, 0)) %>%
  select(-c(.pred)) 

## Exportar
write_csv(elasticnet_predictions, "../stores/02_submissions/classification_elasticnet.csv")



# Logit -------------------------------------------------------------

# Lasso -------------------------------------------------------------------

validation_train <- validation_train %>%
  mutate(Pobre = ifelse(Pobre ==1, "si", "no")) %>%
  mutate(Pobre = factor(Pobre))
validation_train$Pobre <- relevel(validation_train$Pobre, ref = "si")

validation_set <- validation_set %>%
  mutate(Pobre = ifelse(Pobre ==1, "si", "no")) %>%
  mutate(Pobre = factor(Pobre))
validation_set$Pobre <- relevel(validation_set$Pobre, ref = "si")

kfolds <- vfold_cv(validation_train, v = 5)

#Model
logit_lasso <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("brulee")

lambda_grid_logit_lasso <- grid_regular(penalty(), levels = 50)

#Recipe
logit_lasso_rec <-
  recipe(Pobre ~ ., data = validation_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  #step_interact(terms = ~ P5130:starts_with("Dominio")) %>%
  #step_interact(terms = ~ Educ_avg:starts_with("Dominio")) %>%
  #step_interact(terms = ~ tasa_ocupados:starts_with("Dominio")) %>%
  #step_interact(terms = ~ tasa_inactivos:starts_with("Dominio")) %>%
  #step_interact(terms = ~ P5000:starts_with("Dominio")) %>%
  #step_interact(terms = ~ edad_pet:starts_with("Dominio")) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
logit_lasso_wflow <- 
  workflow() %>% 
  add_recipe(logit_lasso_rec) %>% 
  add_model(logit_lasso) 


#Tune grid

tune_result <- logit_lasso_wflow %>% 
  tune_grid(resamples = kfolds,
            grid = lambda_grid_logit_lasso,
            metrics = metric_set(f_meas),
            control=control_grid(verbose=TRUE))

results_tuning_logit_lasso <- tune_result %>%
  collect_metrics()

# Plot
plot_logit_lasso_penalty<- results_tuning_logit_lasso %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(linewidth = 1) +
  ylab("F1 Score") +
  xlab("Penalidad (lambda)") + 
  theme_bw() +
  theme(legend.position = "none") 

#Best fit 
tune_best <- tune_result %>% 
  select_best(metric = "f_meas")

best_logit_lasso <- logistic_reg() %>%
  set_engine("glm")

best_logit_lasso_wflow <- 
  workflow() %>% 
  add_recipe(logit_lasso_rec) %>% 
  add_model(best_logit_lasso) 

best_logit_lasso_fit <- 
  best_logit_lasso_wflow %>% 
  fit(validation_train)


# Curva ROC para escoger punto de corte
source("03_Models/ROC_function.R")

#Predict en validation
predictions_real_df_logit_lasso <- predict(best_logit_lasso_fit, validation_set, type = "prob")$si %>%
  bind_cols(validation_set$Pobre) %>%
  #mutate() %>%
  rename(c("real" = "...2", "predictions"=".pred_class")) %>%
  mutate(real = as.factor(real))

roc_thresh_logit_lasso <- ROC_function(predictions_real_df_logit_lasso)


# Matriz de confusión 
#confusionMatrix(data = predict_test$class_ROC, reference = predict_test$Pobre)

#Predict
logit_lasso_predictions <- predict(best_logit_lasso_fit, test) %>%
  bind_cols(test$id) %>%
  rename(c("id"="...2")) %>%
  mutate(pobre = ifelse(.pred>=roc_thresh_logit_lasso$threshold, 1, 0)) %>%
  select(-c(.pred)) 

## Exportar
write_csv(logit_lasso_predictions, "../stores/02_submissions/classification_logit_lasso.csv")


# QDA ---------------------------------------------------------------------

qda <- discrim_quad(regularization_method = "diagonal") %>% 
  set_engine("sparsediscrim")%>% 
  set_mode("classification")

#Recipe
qda_rec <-
  recipe(Pobre ~ ., data = validation_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ P5130:starts_with("Dominio")) %>%
  step_interact(terms = ~ Educ_avg:starts_with("Dominio")) %>%
  step_interact(terms = ~ tasa_ocupados:starts_with("Dominio")) %>%
  step_interact(terms = ~ tasa_inactivos:starts_with("Dominio")) %>%
  step_interact(terms = ~ P5000:starts_with("Dominio")) %>%
  step_interact(terms = ~ edad_pet:starts_with("Dominio")) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
qda_wflow <- 
  workflow() %>% 
  add_recipe(qda_rec) %>% 
  add_model(qda) 

#Fit
qda_fit <- 
  qda_wflow %>% 
  fit(validation_train)

#Predict en validation
predictions_real_df_qda <- predict(qda_fit, validation_set, type = "prob")$.pred_si %>%
  bind_cols(validation_set$Pobre) %>%
  rename(c("real" = "...2", "predictions"="...1"))

roc_thresh_qda <- ROC_function(predictions_real_df_qda)

#Predict
qda_predictions <- predict(qda_fit, test, type = "prob")$.pred_si %>%
  bind_cols(test$id) %>%
  rename(c("id"="...2")) %>%
  mutate(pobre = ifelse(...1>=roc_thresh_qda$threshold, 1, 0)) %>%
  select(-c(...1)) %>%
  replace_na(list(pobre = 0))

## Exportar
write_csv(qda_predictions, "../stores/02_submissions/classification_qda.csv")

# lda ---------------------------------------------------------------------

lda <- discrim_linear(regularization_method = "diagonal") %>% 
  set_engine("sparsediscrim")%>% 
  set_mode("classification")

#Recipe
lda_rec <-
  recipe(Pobre ~ ., data = validation_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ P5130:starts_with("Dominio")) %>%
  step_interact(terms = ~ Educ_avg:starts_with("Dominio")) %>%
  step_interact(terms = ~ tasa_ocupados:starts_with("Dominio")) %>%
  step_interact(terms = ~ tasa_inactivos:starts_with("Dominio")) %>%
  step_interact(terms = ~ P5000:starts_with("Dominio")) %>%
  step_interact(terms = ~ edad_pet:starts_with("Dominio")) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

#Workflow
lda_wflow <- 
  workflow() %>% 
  add_recipe(lda_rec) %>% 
  add_model(lda) 

#Fit
lda_fit <- 
  lda_wflow %>% 
  fit(validation_train)

#Predict en validation
predictions_real_df_lda <- predict(lda_fit, validation_set, type = "prob")$.pred_si %>%
  bind_cols(validation_set$Pobre) %>%
  rename(c("real" = "...2", "predictions"="...1"))

roc_thresh_lda <- ROC_function(predictions_real_df_lda)

#Predict
lda_predictions <- predict(lda_fit, test, type = "prob")$.pred_si %>%
  bind_cols(test$id) %>%
  rename(c("id"="...2")) %>%
  mutate(pobre = ifelse(...1>=roc_thresh_lda$threshold, 1, 0)) %>%
  select(-c(...1)) %>%
  replace_na(list(pobre = 0))

## Exportar
write_csv(lda_predictions, "../stores/02_submissions/classification_lda.csv")
