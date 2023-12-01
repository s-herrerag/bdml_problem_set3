##############################################################
# Variables (3): P7495 to Cclasnr6
# Santiago Herrera García
##############################################################


# Libraries ---------------------------------------------------------------
library(pacman)
p_load(tidyverse, janitor)


# Data --------------------------------------------------------------------
train_hogares <- read_csv("../stores/raw/train_hogares.csv")
train_personas <- read_csv("../stores/raw/train_personas.csv")
test_hogares <- read_csv("../stores/raw/test_hogares.csv")
test_personas <-read_csv("../stores/raw/test_personas.csv")

#Build full train to analyze variables
full_train <- left_join(train_hogares, train_personas)
full_test <- left_join(test_hogares, test_personas)

# Variables ---------------------------------------------------------------
vars_interest <- c("P7495", "P7500s1", "P7500s1a1",  
"P7500s2", "P7500s2a1", "P7500s3", "P7500s3a1", "P7505", "P7510s1",   
"P7510s1a1", "P7510s2", "P7510s2a1", "P7510s3", "P7510s3a1", "P7510s5",    
"P7510s5a1", "P7510s6", "P7510s6a1", "P7510s7", "P7510s7a1", "Pet",  
"Oc", "Des", "Ina", "Impa", "Isa", "Ie", "Imdi", "Iof1", "Iof2", 
"Iof3h", "Iof3i", "Iof6", "Cclasnr2", "Cclasnr3", "Cclasnr4", "Cclasnr5", "Cclasnr6")

test_colnames <- colnames(full_test)

#Select
train_interest <- train_personas %>%
  select(all_of(c("id", "Orden", vars_interest)))

test_interest <- test_personas %>%
  select(any_of(c("id", "Orden", vars_interest)))

# Variable analysis -------------------------------------------------------

#### Missing values
nas_train <- t(colSums(is.na(train_interest))*100/dim(train_personas)[1])
nas_test <- t(colSums(is.na(test_interest))*100/dim(train_personas)[1])

#### All are in persons:

# P7495 (%7 NA in test) - Yes: Fill with no, KNN or .
"P7495"%in%test_colnames #Yes
table(train_personas$P7495)
table(test_personas$P7495)

# P7500s1 - No (same info P7495)
"P7500s1"%in%test_colnames #No

# P7500s1a1 - No
"P7500s1"%in%test_colnames #No

#P7500s2 - Could be, 92% NAs, but fill with next in train or no in test
"P7500s2"%in%test_colnames #Yes
table(train_personas$P7500s2)
table(test_personas$P7500s2)

#P7500s2a1 - Not in test
"P7500s2a1"%in%test_colnames #No

#P7500s3
"P7500s3"%in%test_colnames #Fill with s3a1 in train, with no in test
table(train_personas$P7500s3)
table(test_personas$P7500s3)

#P7505 - Yes!: Important
"P7505"%in%test_colnames
table(train_personas$P7505)

#P7510s1 - Yes!
"P7510s1"%in%test_colnames
table(train_personas$P7510s1)

## Many vars with the same pattern:intriguing the 17% NA

##pet*Oc, etc. NAs are information in those variables. Always interact with pet. 
## All no after ina. 

# Select useful vars ------------------------------------------------------

useful_vars <- c("P7495", "P7500s1","P7500s2", "P7500s3", "P7505", "P7510s1", "P7510s2", 
                 "P7510s3", "P7510s5", "P7510s6", "P7510s7", "Pet", "Oc", "Des", "Ina")

useful_train_pack3 <- full_train %>%
  select(all_of(c("id", "Orden", useful_vars)))
  
write_csv(useful_train_pack3, "../stores/01_var_exploration/variables_paquete_3.csv")
  
  
  