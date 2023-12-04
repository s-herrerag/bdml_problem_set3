# Cargamos librerías ------------------------------------------------------

require("pacman")
p_load("tidyverse", 
       "dplyr", 
       "doParallel",
       "tidymodels",
       "yardstick",
       "rpart")


# Directorio --------------------------------------------------------------

file_dir <- this.path::here()
setwd(file_dir)

# Cargamos datos ----------------------------------------------------------

df_train_personas <- read.csv("../stores/raw/train_personas.csv")
df_train_hogares <- read.csv("../stores/raw/train_hogares.csv")
df_test_personas <- read.csv("../stores/raw/test_personas.csv")
df_test_hogares <- read.csv("../stores/raw/test_hogares.csv")


# Unión de Bases ----------------------------------------------------------

test_full <- merge(df_test_personas, df_test_hogares, by = "id", all.x = TRUE, indicator = TRUE)

train_full <- merge(df_train_personas, df_train_hogares, by = "id", all.x = TRUE, indicator = TRUE)

test_full$train <- 0 
train_full$train <- 1 

test_full[, setdiff(names(train_full), names(test_full))] <- NA 

db1 <- rbind(train_full, test_full)

# Subset Variables Seleccionadas, Paquete 2 -------------------------------

db <- as.data.frame(subset(db1, select = c(id, P6050, P6600, P6610, P6620, P6630s1, P6630s2, P6630s3, P6630s4, P6630s6, P6800, P6870, P6920, P7040, P7045, P7050, P6920, P7090, P7110, P7120, P7150, P7160, P7310, P7350, P7422, P7472, Npersug, train)))

# Reemplazo de valores 2 y 9 por 0 en variables dummies ---------------

db <- db %>%
  mutate(P6600 = ifelse(P6600 == 2 | P6600 == 9, 0, P6600))

db <- db %>%
  mutate(P6610 = ifelse(P6610 == 2 | P6610 == 9, 0, P6610))

db <- db %>%
  mutate(P6620 = ifelse(P6620 == 2 | P6620 == 9, 0, P6620))

db <- db %>%
  mutate(P6630s1 = ifelse(P6630s1 == 2 | P6630s1 == 9, 0, P6630s1))

db <- db %>%
  mutate(P6630s2 = ifelse(P6630s2 == 2 | P6630s2 == 9, 0, P6630s2))

db <- db %>%
  mutate(P6630s3 = ifelse(P6630s3 == 2 | P6630s3 == 9, 0, P6630s3))

db <- db %>%
  mutate(P6630s4 = ifelse(P6630s4 == 2 | P6630s4 == 9, 0, P6630s4))

db <- db %>%
  mutate(P6630s6 = ifelse(P6630s6 == 2 | P6630s6 == 9, 0, P6630s6))

db <- db %>%
  mutate(P6920 = ifelse(P6920 == 2 | P6920 ==3, 0, P6920))

db <- db %>%
  mutate(P7040 = ifelse(P7040 == 2 | P7040 == 9, 0, P7040))

db <- db %>%
  mutate(P7090 = ifelse(P7090 == 2 | P7090 == 9, 0, P7090))

db <- db %>%
  mutate(P7110 = ifelse(P7110 == 2 | P7110 == 9, 0, P7110))

db <- db %>%
  mutate(P7120 = ifelse(P7120 == 2 | P7120 == 9, 0, P7120))

db <- db %>%
  mutate(P7150 = ifelse(P7150 == 2 | P7150 == 9, 0, P7150))

db <- db %>%
  mutate(P7160 = ifelse(P7160 == 2 | P7160 == 9, 0, P7160))

db <- db %>%
  mutate(P7310 = ifelse(P7310 == 2 | P7310 == 9, 0, P7310))

db <- db %>%
  mutate(P7422 = ifelse(P7422 == 2 | P7422 == 9, 0, P7422))

db <- db %>%
  mutate(P7472 = ifelse(P7472 == 2 | P7472 == 9, 0, P7472))


# Agrupación de datos de personas por Hogar -------------------------------

db <- db %>%
  select(-train) %>%
  filter(!P6050%in%c(6,7,8,9)) %>%
  mutate(obrero = ifelse(P7050 == 1, 1, 0),
         empleado_gobierno = ifelse(P7050 == 2, 1, 0),
         empleado_domestico = ifelse(P7050 == 3, 1, 0),
         trab_cta_propia = ifelse(P7050 == 4, 1, 0),
         patron = ifelse(P7050 == 5, 1, 0),
         trabaj_fam_sin_remun = ifelse(P7050 == 6, 1, 0),
         trabaj_no_fam_sin_remun = ifelse(P7050 == 7, 1, 0),
         jornalero = ifelse(P7050 == 8, 1, 0),
         previo_obrero = ifelse(P7350 == 1, 1, 0),
         previo_empleado_gobierno = ifelse(P7350 == 2, 1, 0),
         previo_empleado_domestico = ifelse(P7350 == 3, 1, 0),
         previo_trab_cta_propia = ifelse(P7350 == 4, 1, 0),
         previo_patron = ifelse(P7350 == 5, 1, 0),
         previo_trabaj_fam_sin_remun = ifelse(P7350 == 6, 1, 0),
         previo_trabaj_no_fam_sin_remun = ifelse(P7350 == 7, 1, 0),
         previo_jornalero = ifelse(P7350 == 8, 1, 0)) %>%
  replace_na(list(obrero = 0,
                  empleado_gobierno = 0,
                  empleado_domestico = 0,
                  trab_cta_propia = 0,
                  patron = 0,
                  trabaj_fam_sin_remun = 0,
                  trabaj_no_fam_sin_remun = 0,
                  jornalero = 0,
                  previo_obrero = 0,
                  previo_empleado_gobierno = 0,
                  previo_empleado_domestico = 0,
                  previo_trab_cta_propia = 0,
                  previo_patron = 0,
                  previo_trabaj_fam_sin_remun = 0,
                  previo_trabaj_no_fam_sin_remun = 0,
                  previo_jornalero = 0, 
                  P6870 = 0))%>%
  group_by(id) %>%
  summarise(especie_vivienda = sum(P6600, na.rm = T)/n(),
         especie_transporte = sum(P6610, na.rm = T)/n(), 
         especie_otros = sum(P6620, na.rm = T)/n(),
         prima_servicios = sum(P6630s1, na.rm = T)/n(),
         prima_navidad = sum(P6630s2, na.rm = T)/n(),
         prima_vacaciones = sum(P6630s3, na.rm = T)/n(),
         viaticos_bonificaciones = sum(P6630s4, na.rm = T)/n(),
         bonificaciones_anuales = sum(P6630s6, na.rm = T)/n(),
         horas_trabajadas = sum(P6800, na.rm = T),
         tamano_empresa = max(P6870, na.rm = T),
         pensionados = sum(P6920, na.rm = T)/n(),
         segundo_trabajo = sum(P7040, na.rm = T)/n(),
         horas_trabajadas_2 = sum(P7045, na.rm = T),
         obrero = ifelse(sum(obrero)>=1, 1, 0),
         empleado_gobierno = ifelse(sum(empleado_gobierno)>=1, 1, 0),
         empleado_domestico = ifelse(sum(empleado_domestico)>=1, 1, 0),
         trab_cta_propia = ifelse(sum(trab_cta_propia)>=1, 1, 0),
         patron = ifelse(sum(patron)>=1, 1, 0),
         trabaj_fam_sin_remun = ifelse(sum(trabaj_fam_sin_remun)>=1, 1, 0),
         trabaj_no_fam_sin_remun = ifelse(sum(trabaj_no_fam_sin_remun)>=1, 1, 0),
         jornalero = ifelse(sum(jornalero)>=1, 1, 0),
         desea_trabajar_mas = sum(P7090, na.rm = T)/n(),
         diligencias_para_trabajar_mas = sum(P7110, na.rm = T)/n(),
         disponibilidad_otro_trabajo = sum(P7120, na.rm = T)/n(),
         diligencias_cambio_trabajo = sum(P7150, na.rm = T)/n(),
         disponibilidad_cambio_trabajo = sum(P7160, na.rm = T)/n(),
         primer_trabajo = sum(P7310, na.rm = T)/n(),
         previo_obrero = ifelse(sum(previo_obrero)>=1, 1, 0),
         previo_empleado_gobierno = ifelse(sum(previo_empleado_gobierno)>=1, 1, 0),
         previo_empleado_domestico = ifelse(sum(previo_empleado_domestico)>=1, 1, 0),
         previo_trab_cta_propia = ifelse(sum(previo_trab_cta_propia)>=1, 1, 0),
         previo_patron = ifelse(sum(previo_patron)>=1, 1, 0),
         previo_trabaj_fam_sin_remun = ifelse(sum(previo_trabaj_fam_sin_remun)>=1, 1, 0),
         previo_trabaj_no_fam_sin_remun = ifelse(sum(previo_trabaj_no_fam_sin_remun)>=1, 1, 0),
         previo_jornalero = ifelse(sum(previo_jornalero)>=1, 1, 0),
         ingresos_trabajo = sum(P7422, na.rm = T)/n(),
         ingresos_trabajo_2 = sum(P7472, na.rm = T)/n()
  )


# Añadimos la variable train ------------------------------------------

ids_train_test <- db1 %>%
  select(id, train) %>%
  unique()

db <- db %>%
  left_join(ids_train_test)

#db <- subset(db, select = -c(P6600, P6610, P6620, P6630s1, P6630s2, P6630s3, P6630s4, P6630s6, P6800, P6870, P6920, P7040, P7045, P7050, P6920, P7090, P7110, P7120, P7150, P7160, P7310, P7350, P7422, P7472, P6920.1, Npersug))

# Separación bases --------------------------------------------------------

db_train <- subset(db, train == 1)

db_test <- subset(db, train == 0)


# Eliminación variable train ----------------------------------------------

db_train <- subset(db_train, select = -c(train))

db_test <- subset(db_test, select = -c(train))

# Escritura CSV's ---------------------------------------------------------

write_csv(db_train, file.path("../stores", "train_pack_2.csv"))

write_csv(db_test, file.path("../stores", "test_pack_2.csv"))


#################################### TREES ###################

# Cargamos bases de datos finales -----------------------------------------

train_final <- read_rds("../stores/train_final.rds")
test_final <- read_rds("../stores/test_final.rds")


# Vemos balance de clases para pobreza ------------------------------------


table(train_final$Pobre)
table(train_final$Pobre)/nrow(train_final)
#        0         1 
#0.7998109 0.2001891 
#Existe desbalance


# Eliminamos variable logIngreso ------------------------------------------

train_final <- subset(train_final, select = -c(Ingpcug, lIngpcug, Lp))

train_final$Pobre <- as.factor(train_final$Pobre)

# Dividimos datos ---------------------------------------------------------

set.seed(666)

split <- initial_split(train_final, prop = .70)
train_data <- training(split)
test_data  <- testing(split)


# Receta ------------------------------------------------------------------

recipe <- recipe(Pobre ~ . , data = train_data)

# Tune grid ---------------------------------------------------------------

tune_grid_tree <- grid_random(
  tree_depth(range = c(1, 10)),
  min_n(range=c(1, 10)),
  size=100
)

tree_spec <- decision_tree(
  tree_depth = tune(),
  min_n = tune()) %>%
  set_mode("classification")

workflow_1.1 <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(tree_spec)

df_fold <- vfold_cv(train_data, v = 5)

#Accuracy

tune_tree <- tune_grid(
  workflow_1.1,
  resamples = df_fold, 
  grid = tune_grid_tree
)
tune_tree

###Submission 

#F1 Means

#tune_tree1 <- tune_grid(
#  workflow_1.1,
#  resamples = df_fold, 
#  grid = tune_grid_tree,
#  metrics = metric_set(f_meas),
#  control=control_grid(verbose=TRUE)
#)

best_params <- tune_tree %>% select_best(metric = "accuracy")
best_params

best_tree_fit <- finalize_workflow(workflow_1.1, best_params) %>%
  fit(data = train_data)

#Predicción

test_final <- test_final %>%
  mutate(predicciones_tree = predict(best_tree_fit, test_final)$.pred_class)

confusion_matrix <- conf_mat(test_final, truth = Pobre, estimate = predicciones_tree )

accuracy <- accuracy(test_final, truth = Pobre, estimate = predicciones_tree)

#Eliminar variables innecesarias para el submission

test_final1 <- as.data.frame(subset(test_final, select = c(id, predicciones_tree)))
colnames(test_final1)[2]="pobre"

#Escribir archivo para submission

write_csv(test_final1, "modelo1_arbol.csv")

#### Árbol de Regresión y Cost Complexity Prunning ---------------------

#Carga de datos, sin Bogotá para evitar ruidos

train <- readRDS("../stores/train_final.rds")
train_sin_bog <- readRDS("../stores/train_final_sin_bog.rds")
test <- readRDS("../stores/test_final.rds")

#Seleccion basada en Lasso
coefs_lasso <- read_csv("../stores/coefs_lasso.csv")

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
                                     levels = 100)

wf_ccp_tree <- workflow() %>%
  add_recipe(rec_ccp_tree) %>%
  add_model(model_ccp_tree)

registerDoParallel() ##################
tune_ccp_tree <- wf_ccp_tree %>% 
  tune_grid(resamples = kfolds,
            grid = grid_ccp_tree,
            metrics = metric_set(rmse))
stopImplicitCluster() ################
stopImplicitCluster() ################

metrics_ccp <- tune_ccp_tree %>%
  collect_metrics()
  select_best("mae")
  
best_ccp <- select_best(tune_ccp_tree, metric = "rmse")
ccp_final <- finalize_workflow(wf_ccp_tree, best_ccp)
ccp_final_fit <- fit(ccp_final, data = train_reg)

ccp_predictions <- predict(ccp_final_fit, test) %>%
  bind_cols(test$id) %>%
  bind_cols(test$Lp) %>%
  rename(c("id"="...2", "Lp" = "...3")) %>%
  mutate(Ingpcug=exp(.pred)) %>%
  mutate(pobre = ifelse(Ingpcug<=Lp,1,0)) %>%
  select(-c(.pred, Ingpcug, Lp))

write_csv(boost_predictions, "../stores/regression_ccp_tree.csv")