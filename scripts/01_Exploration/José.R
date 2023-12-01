require("pacman")
p_load("tidyverse", "stargazer", "xtable")

file_dir <- this.path::here()
setwd(file_dir)
train_hogares_José <- read_csv("../../stores/raw/train_hogares.csv")
train_personas_José <- read_csv(("../../stores/raw/train_personas.csv"))

colnames(train_hogares)
colnames(train_personas)

nans_hogares <- sapply(train_hogares, function(x) sum(is.na(x)))
nans_personas <- sapply(train_personas, function(x) sum(is.na(x)))
#nans_all <- rbind(nans, nans_test)


#print(xtable(nans_all, type = "latex"), file = "/stores/nans_table.tex")

df_for_descriptive_statistics <- as.data.frame(subset(id, Orden, train_personas_José, select = c(P6590s1, P6600s1, P6610s1, P6620s1, P6630s1a1, P6630s2a1, P6630s3a1, P6630s4a1, P6630s6a1, P6750, P6760, P550, P6800, P6870, P6920, P7040, P7045, P7070, P7310, P7422, P7422s1, P7472, P7472s1)))

write_csv(df_for_descriptive_statistics, file.path("../../stores", "variables_parquete_2.csv"))

descriptive_statistics <- stargazer(df_for_descriptive_statistics,
                                    type = "text", min.max = TRUE, mean.sd = TRUE,
                                    nobs = TRUE, median = TRUE, iqr = FALSE,
                                    digits = 1, align = T,
                                    title = "Summary Statistics for the Final Training Sample",
                                    covariate.labels = c("EspecieAlimentos", "EspecieVivienda", "EspecieTransporte", "EspecieOtros", "IngPrimaServ", "IngPrimaNav", "IngPrimaVac", "ViatPermBonifAnuales", "BonifAnuales", "Honorarios", "MesesHonorarios", "GananCosechAnual", "HorasSemTrabaj", "TamañoEmpresa", "CotizPension", "SegundoTrabajo", "SegundoTrabajoHoras", "SegundoTrabajoIngMens", "PrimerTrabajo", "Ocupado", "IngLab", "Ocupado?", "Inglab?"),
                                    out="C:/Users/PC/Documents/Uniandes/Economía/Big Data y Machine Learning para Economía Aplicada/Taller 3/descrp_stat.txt"
)