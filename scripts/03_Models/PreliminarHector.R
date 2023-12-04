# Cargar pacman (contiene la función p_load)
library(pacman)

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(
  tidyverse, # Manipular dataframes
  rio, # Import data easily
  plotly, # Gráficos interactivos
  leaflet, # Mapas interactivos
  rgeos, # Calcular centroides de un poligono
  tmaptools, # geocode_OSM()
  sf, # Leer/escribir/manipular datos espaciales
  osmdata, # Get OSM's data
  tidymodels, # para modelos de ML
  stopwords, # para eliminar palabras sin "valor"
  tokenizers,
  SnowballC
)

require(pacman)
p_load(tidymodels, 
       caret, 
       rpart, 
       modeldata, 
       spatialsample,
       tidyverse,
       rio,
       leaflet,
       randomForest,
       rattle, 
       spatialsample, 
       doParallel, 
       ranger, 
       xgboost, 
       lightgbm, 
       bonsai)

rm(list = ls())
setwd("/Users/hectorsegura/Documentos/Big Data & ML/Taller 3 ")

# Importación de las bases ---------------------------------------------

test_personas <- read.csv("test_personas.csv")
test_hogares <- read.csv("test_hogares.csv")

train_personas <- read.csv("train_personas.csv")
train_hogares <- read.csv("train_hogares.csv")

# Junte de las bases ---------------------------------------------------

test_full <- merge(test_personas, test_hogares, by = "id", all.x = TRUE, indicator = TRUE)

train_full <- merge(train_personas, train_hogares, by = "id", all.x = TRUE, indicator = TRUE)

test_full$train <- 0 
train_full$train <- 1 

test_full[, setdiff(names(train_full), names(test_full))] <- NA 

db <- rbind(train_full, test_full)

# Pobre = I(Ingpcug < Lp) con linea de pobreza = 289878.2 & Ingcup: ingreso per cápita de la unidad de gasto 

# Variables ------------------------------------------------------------

# Del módulo de HOGARES ------------------------------------------------

db$vivienda <- factor(db$P5090, labels = c("Propia, pagada", "Propia, pagando", "Arriendo/subarriendo", "Usufructo", "Posesión sin título", "Otra"))

#Nper: número de personas en el hogar 
#Depto: se puede pensar en el pegue de NBIs por departamentos 
#Ingtotug: ingreso total de la unidad de gasto antes de imputación de arriendo 
#Ingtotugarr: ingreso total de la unidad de gasto con imputación de arriendo 

# Del módulo de PERSONAS -----------------------------------------------

#Estrato1:estrato socioeconómico 

db$tiene_salud <- case_when(
  db$P6090 == 1 ~ 1,
  db$P6090 == 2 ~ 0,
  db$P6090 == 9 ~ NA) #Discutir qué hacer con los NAs, son 326 

db$pensionado <- ifelse(db$P6920 == 3, 1, 0)
  
db$cotiza_pension <- case_when(
  db$P6920 == 1 ~ 1,
  db$P6920 == 2 ~ 0,
  db$P6920 == 3 ~ NA) # 3 ya es pensionado, son 6352. Podemos ponerlos también como 1 (?)

db$nivel_educ <- ifelse(db$P6210 == 9, 1, db$P6210) #Hay 93 obs. que son 9 y no está contemplado en el formulario
db$nivel_educ <- factor(db$nivel_educ, labels = c("Ninguno", "Preescolar", "Primaria", "Secundaria", "Media", "Superior"))

#P6020: sexo está en ambas y puede ser relevante para el ingreso
#P6040: años cumplidos que tiene, está en ambas 
#P6050: parentesco con el jefe de hogar, está en ambas 
#P6090: tiene seguridad social en salud, en ambas
#P6100: a cuá regimen está afiliado, en ambas 
#P6210: nivel educativo, en ambas
#P6210s1: grado escolar aprobado, en ambas 
#P6240: en qué ocupó la mayor parte de su tiempo, en ambas
#Oficio: está en ambas 
#P6500 - P6590 tienen que ver con los ingresos - subsidios recibidos por la persona
  #P6545: recibió primas, en ambas 
  #P6580: recibió bonificaciones, en ambas
  #P6585s1-s4 auxilio alimentación, transporte, familiar, educativo. Está en ambas 
  #Estas NO están en test: P6500 (cuánto ganó)

#P6590: además del salario en dinero, recibió alimentos como parte de pago por su trabajo, en ambas. Interesante. Muchos Na = 174430 en test y 426983 en train

Base_Hector <- select(db, "id", "Orden", "Clase.x", "train", "Estrato1", "P6020", "P6040", "P6050", "P6090", "P6100", "P6210", "nivel_educ", "P6210s1", "P6240", "Oficio", 
                      "P6426", "P6430", "P6510", "P6510s1", "P6545", "P6545s1", "P6580", "P6580s1", "P6585s1", "P6585s1a1", "P6585s2", "P6585s2a1", "P6585s3", "P6585s3a1", "P6585s4", "P6585s4a1", "P6590", 
                      "Pet", "Oc", "Des", "Ina", "Nper", "Npersug")

write_csv(Base_Hector, file = "stores/01_var_exploration/Base_Hector.csv")

# Exploración NAs ---------------------------------------------------------

nas_all <- as.data.frame(colSums(is.na(Base_Hector)))

p_nas_all <- as.data.frame(colSums(is.na(Base_Hector)) *100 / dim(Base_Hector)[1])

Patron_NAs <- cbind(nas_all, p_nas_all)

#No entiendo por qué hay un patrón de 78.6% de Nas. No coincide con nada. Hay gente ocupada (en la PET) con NA, gente que no es inactiva con NA...
  #Puede ser gente que simplemente que declaró no haber recibido nada el mes pasado o que era inactiva, desocupada el mes pasado a la encuesta 
  #Parecen Nas inducidos 

# 17.57% de Nas corresponden a las personas fuera de la PET 
# 54.32% de Nas corresponden a las personas no OCUPADAS 

#Hago la revisón con las variables en paquetes de preguntas a ver si tienen cosas que no coincidan 
  # Revisé todas las variables en grupos que tiene 78.6% de Nas; no hay nadie que reporte un valor y que responda NO. 

Base_Hector %>% 
  filter(P6510 == 2 & P6510s1 != 0 ) %>% 
  summarise(count = n()) 

Base_Hector <- Base_Hector %>%
  replace_na(list(Oc = 0, Des = 0, Ina = 0, Pet = 0))

Base_Hector %>% 
  filter(is.na(P6585s4) & P6585s4a1 != 0) %>% 
  summarise(count = n()) 

Base_Hector %>% 
  filter(P6545 == 1 & P6545s1 == 98) %>% 
  summarise(count = n()) 

Base_Hector <- Base_Hector %>% #Hay personas que declaran monto 0 que dicen sí recibir el rubro 
  mutate(
    P6545 = ifelse(P6545s1 == 0 & train == 1, 2, P6545),
    P6580 = ifelse(P6580s1 == 0 & train == 1, 2, P6580),
    P6585s1 = ifelse(P6585s1a1 == 0 & train == 1, 2, P6585s1), 
    P6585s2 = ifelse(P6585s2a1 == 0 & train == 1, 2, P6585s2),
    P6585s3 = ifelse(P6585s3a1 == 0 & train == 1, 2, P6585s3),
    P6585s4 = ifelse(P6585s4a1 == 0 & train == 1, 2, P6585s4))

Base_Hector <- Base_Hector %>%
  mutate(
    P6545 = ifelse(P6545 == 2 | P6545 == 9, 0, P6545),
    P6580 = ifelse(P6580 == 2 | P6545 == 9, 0, P6580),
    P6585s1 = ifelse(P6585s1 == 2 | P6545 == 9, 0, P6585s1), 
    P6585s2 = ifelse(P6585s2 == 2 | P6545 == 9, 0, P6585s2),
    P6585s3 = ifelse(P6585s3 == 2 | P6545 == 9, 0, P6585s3),
    P6585s4 = ifelse(P6585s4 == 2 | P6545 == 9, 0, P6585s4),
    DiffNper = Nper - Npersug)

# Reemplazo de NAs --------------------------------------------------------

Base_Hector <- Base_Hector %>%
  group_by(id) %>%
  mutate(
    Nprimas = sum(P6545, na.rm = TRUE),
    Nbonos  = sum(P6580, na.rm = TRUE),
    NauxAli = sum(P6585s1, na.rm = TRUE),
    NauxTra = sum(P6585s2, na.rm = TRUE),
    NauxFam = sum(P6585s3, na.rm = TRUE),
    NauxEdu = sum(P6585s4, na.rm = TRUE),) %>%
  ungroup()

Base_Hector <- Base_Hector %>%
  group_by(id) %>%
  mutate(
    NprimasPP = sum(P6545, na.rm = TRUE)/Npersug,
    NbonosPP  = sum(P6580, na.rm = TRUE)/Npersug,
    NauxAliPP = sum(P6585s1, na.rm = TRUE)/Npersug,
    NauxTraPP = sum(P6585s2, na.rm = TRUE)/Npersug,
    NauxFamPP = sum(P6585s3, na.rm = TRUE)/Npersug,
    NauxEduPP = sum(P6585s4, na.rm = TRUE)/Npersug,
    Educ_avg = sum(nivel_educ, na.rm = TRUE)/Npersug) %>%
  ungroup()

#Educación del jefe de hogar

Educ_jefe <- Base_Hector %>% 
  filter(P6050 == 1) %>% 
  select("id", "nivel_educ") %>% 
  rename(Educ_jefe = nivel_educ)

names(Base_Hector)[names(Base_Hector) == "Clase.x"] <- "Cabecera"
names(Base_Hector)[names(Base_Hector) == "Estrato1"] <- "Estrato" #No está en test

BaseHogar_Hector <- Base_Hector %>% 
  select("id", "train", "Cabecera", "Nprimas", "Nbonos", "NauxAli", "NauxTra", "NauxFam", "NauxEdu", "NprimasPP", "NbonosPP", "NauxAliPP", "NauxTraPP", "NauxFamPP", "NauxEduPP", "Educ_avg") %>% 
  mutate(Cabecera = ifelse(Cabecera == 2, 0, Cabecera)) %>% 
  unique()

BaseHogar_Hector <- right_join(BaseHogar_Hector, Educ_jefe, by = "id")

BaseHector_train <- BaseHogar_Hector %>% 
  filter(train == 1)

write_csv(BaseHector_train, file = "BaseHector_train.csv")

BaseHector_test <- BaseHogar_Hector %>% 
  filter(train == 0)

write_csv(BaseHector_test, file = "BaseHector_test.csv")


# Bases para modelos  -----------------------------------------------------

train_final <- readRDS("train_final.rds")
test_final <- readRDS("test_final.rds") %>% 
  replace_na(list(edad_pet=0, tasa_ocupados=0, tasa_inactivos=0))
train_sin_bog <- readRDS("train_final_sin_bog.rds")

train_class <- train_final_sin_bog %>%
  select(-c(Lp, Ingpcug, lIngpcug, Depto, train, Clase))

#Validation set para train sin Bogotá
set.seed(123)

validation_set <- train_final_sin_bog %>% 
  mutate(Pobre = factor(Pobre)) %>% 
  sample_frac(0.15)

validation_train <- train_class %>% 
  anti_join(validation_set, by = "id") %>%
  mutate(Pobre = factor(Pobre)) %>% 
  select(-id) 

validation_train <- validation_train %>%
  mutate(Pobre = ifelse(Pobre ==1, "si", "no")) %>%
  mutate(Pobre = factor(Pobre))
validation_train$Pobre <- relevel(validation_train$Pobre, ref = "si")

validation_set <- validation_set %>%
  mutate(Pobre = ifelse(Pobre ==1, "si", "no")) %>%
  mutate(Pobre = factor(Pobre))
validation_set$Pobre <- relevel(validation_set$Pobre, ref = "si")


# Random forest  ----------------------------------------------------------

set.seed(123)
#Model
random_forest_class <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = tune(),
) %>%
  set_engine("ranger") %>%
  set_mode("classification")

random_forest_grid <- grid_random(mtry(range = c(5, 20)),
                                   min_n(),
                                   trees(range = c(250, 500)),
                                   levels = c(5,5,2))

#Receta, con todas las variables

rec_forests <-
  recipe(Pobre ~ ., data = validation_train, -geometry) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  #step_interact(terms = ~ P5130:starts_with("Dominio")) %>%
  #step_interact(terms = ~ Educ_avg:starts_with("Dominio")) %>%
  #step_interact(terms = ~ tasa_ocupados:starts_with("Dominio")) %>%
  #step_interact(terms = ~ tasa_inactivos:starts_with("Dominio")) %>%
  #step_interact(terms = ~ P5000:starts_with("Dominio")) %>%
  #step_interact(terms = ~ edad_pet:starts_with("Dominio")) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes()) %>% 
  step_zv(all_predictors())

#Workflow

workflow_random_forest <- workflow() %>%
  add_recipe(rec_forests) %>%
  add_model(random_forest_class)

kfolds <- vfold_cv(validation_train, v = 5)

#Tune grid
registerDoParallel()
tune_random_forest <- workflow_random_forest %>% 
  tune_grid(resamples = kfolds,
            grid = random_forest_grid,
            metrics = metric_set(f_meas),
            control=control_grid(verbose=TRUE))
stopImplicitCluster()

metrics_rf <- tune_random_forest %>%
  collect_metrics()

#Best Fit, Training and K-validation
best_random_forest <- select_best(tune_random_forest, metric = "f_meas")

#Predict
random_forest_final <- finalize_workflow(workflow_random_forest, best_random_forest)
random_forest_final_fit <- fit(random_forest_final, data = validation_train) #Para ROC 

predictions_real_df_rforest <- predict(random_forest_final_fit, validation_set) %>%
  bind_cols(validation_set$Pobre)

roc_thresh_rforest <- ROC_function(predictions_real_df_rforest)


# Predicción con test tras ROC  -------------------------------------------

lda_predictions <- predict(lda_fit, test, type = "prob")$.pred_si %>%
  bind_cols(test$id) %>%
  rename(c("id"="...2")) %>%
  mutate(pobre = ifelse(...1>=roc_thresh_lda$threshold, 1, 0)) %>%
  select(-c(...1)) %>%
  replace_na(list(pobre = 0))

## Exportar

write_csv(random_forest_predictions, "submissions/RandomForest.csv")

#Predict en validation
predictions_real_df_lda <- predict(lda_fit, validation_set, type = "prob")$.pred_si %>%
  bind_cols(validation_set$Pobre) %>%
  rename(c("real" = "...2", "predictions"="...1"))

# Árboles con Ranger ------------------------------------------------------

ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = twoClassSummary,
                    classProbs = TRUE,
                    savePredictions = T)

set.seed(123)

class_ranger_ROC <- train(
  Pobre ~ .,
  data=validation_train,
  metric = "ROC",
  method = "ranger",
  trControl = ctrl,
  tuneGrid=expand.grid(
    mtry = c(10, 25, 35),
    splitrule = "gini",
    min.node.size = c(150, 200, 250, 300)),
  verbose = TRUE 
)

#No metí la importancia en el anterior, entonces lo volveré a correr con los valores de la grilla que me arrojó por "mejores" 

registerDoParallel()
class_ranger_ROC_importance <- train(
  Pobre ~ .,
  data=validation_train,
  metric = "ROC",
  method = "ranger",
  trControl = ctrl,
  importance = "impurity",
  tuneGrid=expand.grid(
    mtry = 35,
    splitrule = "gini",
    min.node.size = 150),
  verbose = TRUE 
)
stopImplicitCluster()


# Importancia  ------------------------------------------------------------

print(varImp(class_ranger_ROC_importance))

importance_vars <- class_ranger_ROC_importance$finalModel$variable.importance %>% 
  sort(decreasing = TRUE) 

top20 <- head(importance_vars, 20)

top_20 <- data.frame(
  Importance = unlist(top20)) %>% 
  rownames_to_column(var = "Variable") %>% 
  replace(list(pensionados = "Pensionados", P5130 = "Estimación arriendo", prop_pet = "Proporción PET"))

write_csv(top_20, "Importance_rf.csv")

ggplot(top_20, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Variable", y = "Importancia", title = "Importancia de las variables más importantes") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))



# Predicciones  -----------------------------------------------------------

predictions_rf <- as.data.frame(predict(class_ranger_ROC, newdata = validation_set, type = "prob")$si)

predictions_rf <- predictions_rf %>% 
  bind_cols(validation_set$Pobre) %>%
  rename(c("predictions" = "predict(class_ranger_ROC, newdata = validation_set, type = \"prob\")$si", "real" = "...2"))

source("/Users/hectorsegura/Documentos/GitHub/Problem set 3/scripts/03_Models/ROC_function.R")

roc_thresh_rf <- ROC_function(predictions_rf)

#Predict
rf_predictions <- predict(class_ranger_ROC, test_final, type = "prob")$si %>%
  bind_cols(test_final$id) %>%
  rename(c("id"="...2")) %>%
  mutate(pobre = ifelse(...1>=roc_thresh_rf$threshold, 1, 0)) %>%
  select(-c(...1)) %>%
  replace_na(list(pobre = 0))

## Exportar
write_csv(rf_predictions, "classification_rf.csv")


# Árboles tidymodels, regresión -------------------------------------------

train_reg <- train_sin_bog %>%
  select(-c(Lp, Ingpcug, Pobre, id, train, Clase))

# Folds -------------------------------------------------------------------
set.seed(123)
kfolds <- vfold_cv(train_final, v = 5)

#Model
rf_reg<- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("randomForest") %>%
  set_mode("regression")       # Cambiar a modo de regresión

random_forest_grid_reg <- grid_regular(mtry(range = c(35, 50)),
                                  min_n(),
                                  trees(range = c(500, 1000)),
                                  levels = c(5,5,2))
                                  
#Receta, con todas las variables

rec_forests_reg <-
  recipe(lIngpcug ~ ., data = train_reg) %>% 
  step_dummy(all_of(c("Dominio")), -all_outcomes()) %>% 
  #step_interact(terms = ~ P5130:starts_with("Depto")) %>%
  #step_interact(terms = ~ Educ_avg:starts_with("Depto")) %>%
  #step_interact(terms = ~ tasa_ocupados:starts_with("Depto")) %>%
  #step_interact(terms = ~ tasa_inactivos:starts_with("Depto")) %>%
  #step_interact(terms = ~ P5000:starts_with("Depto")) %>%
  #step_interact(terms = ~ edad_pet:starts_with("Depto")) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes()) %>%
  step_pca(all_numeric_predictors(), threshold = 0.8)

#Workflow

workflow_random_forest_reg <- workflow() %>%
  add_recipe(rec_forests_reg) %>%
  add_model(rf_reg)

#Tune grid
registerDoParallel()

tune_rf_reg <- tune_grid(
  workflow_random_forest_reg, 
  resamples = kfolds,
  grid = random_forest_grid_reg,
  metrics = metric_set(mae),
  control=control_grid(verbose=TRUE))

stopImplicitCluster()

metrics_rf_reg <- tune_random_forest_reg %>%
  collect_metrics()

#Best Fit, Training and K-validation
best_random_forest_reg <- select_best(tune_random_forest_reg, metric = "rmse")

#Predict
random_forest_final <- finalize_workflow(workflow_random_forest, best_random_forest)
random_forest_final_fit <- fit(random_forest_final, data = validation_train) #Para ROC 

predictions_real_df_rforest <- predict(random_forest_final_fit, validation_set) %>%
  bind_cols(validation_set$Pobre)

roc_thresh_rforest <- ROC_function(predictions_real_df_rforest)

# Predicción con test tras ROC  -------------------------------------------

lda_predictions <- predict(lda_fit, test, type = "prob")$.pred_si %>%
  bind_cols(test$id) %>%
  rename(c("id"="...2")) %>%
  mutate(pobre = ifelse(...1>=roc_thresh_lda$threshold, 1, 0)) %>%
  select(-c(...1)) %>%
  replace_na(list(pobre = 0))

## Exportar

write_csv(random_forest_predictions, "submissions/RandomForest.csv")

#Predict en validation
predictions_real_df_lda <- predict(lda_fit, validation_set, type = "prob")$.pred_si %>%
  bind_cols(validation_set$Pobre) %>%
  rename(c("real" = "...2", "predictions"="...1"))
