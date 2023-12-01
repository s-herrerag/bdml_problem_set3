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

rm(list = ls())
setwd("/Users/hectorsegura/Documentos/GitHub/bdml_problem_set_3")

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

db$nivel_educ <- ifelse(db$P6210 == 9, NA, db$P6210) #Discutir qué hacer con los NAs, son 93
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
  #P6580: recibió bonificaciones, en ambas
  #P6585s1-s4 auxilio alimentación, transporte, familiar, educativo. Está en ambas 
  #Estas NO están en test: P6500 (cuánto ganó)

#P6590: además del salario en dinero, recibió alimentos como parte de pago por su trabajo, en ambas. Interesante. Muchos Na = 174430 en test y 426983 en train

Base_Hector <- select(train_personas, "id", "Orden", "Clase", "Estrato1", "P6020", "P6040", "P6050", "P6090", "P6100", "P6210","P6210s1", "P6240", "Oficio", 
                      "P6426", "P6430", "P6510", "P6545", "P6580", "P6430", "P6545", "P6580", "P6585s1", "P6585s2", "P6585s3", "P6585s4", "P6590", 
                      "Pet", "Oc", "Des", "Ina")

write_csv(Base_Hector, file = "stores/01_var_exploration/Base_Hector.csv")


# Exploración NAs ---------------------------------------------------------



