require("pacman")
p_load("tidyverse", 
       "dplyr", 
       "doParallel",
       "tidymodels",
       "yardstick",
       "rpart")

#Carga de datos, sin Bogotá para evitar ruidos

train <- readRDS("../stores/train_final.rds")
train_sin_bog <- readRDS("../stores/train_final_sin_bog.rds")
test <- readRDS("../stores/test_final.rds")

#Seleccion basada en Lasso
coefs_lasso <- read_csv("../stores/01_var_exploration/coefs_lasso.csv")

#Seleccionar variables Lasso
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


# Regresión ---------------------------------------------------------------

rec_ccp_tree <-
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

model_ccp_tree <- decision_tree(cost_complexity = tune()) %>%
  set_engine("rpart") %>% 
  set_mode("regression")

grid_ccp_tree <- grid_regular(cost_complexity(),
                              levels = 25)

wf_ccp_tree <- workflow() %>%
  add_recipe(rec_ccp_tree) %>%
  add_model(model_ccp_tree)

registerDoParallel(cores = 7) ##################
tune_ccp_tree <- wf_ccp_tree %>% 
  tune_grid(resamples = kfolds,
            grid = grid_ccp_tree,
            metrics = metric_set(rmse),
            control=control_grid(verbose=TRUE))
stopImplicitCluster() ################
stopImplicitCluster() ################


best_tree <- select_best(tune_ccp_tree, metric = "rmse")
tree_final <- finalize_workflow(wf_ccp_tree, best_tree)
tree_final_fit <- fit(tree_final, data = train_reg)

#Predict
tree_predictions <- predict(tree_final_fit, test) %>%
  bind_cols(test$id) %>%
  bind_cols(test$Lp) %>%
  rename(c("id"="...2", "Lp" = "...3")) %>%
  mutate(Ingpcug=exp(.pred)) %>%
  mutate(pobre = ifelse(Ingpcug<=Lp,1,0)) %>%
  select(-c(.pred, Ingpcug, Lp)) 

write_csv(tree_predictions, "../stores/02_submissions/regression_tree.csv")



