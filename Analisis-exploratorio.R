library(readr)
library(readxl)
library(dplyr)
library(eq5d)
library(ggplot2)
library(tidyr)

# Datasets.
basal <- read_csv("COVIDPersistent-basal.csv")
df_combined3 <- read_csv("COVIDPersistente-Seguimiento-1año.csv")
df_combined4 <- read_csv("COVIDPersistente-Seguimiento-2año.csv")

names(basal) <- gsub("actuals", "actual", names(basal))

# Cogemos los datos demogràficos.
socio_dem <- basal%>%select(email, sexe, Edat, starts_with("situacio_l"),
                            treball_sanitari, comorbiditats___0,
                            tractament___0, alguna_situacio___4,
                            alguna_situacio___6, alguna_situacio___7,
                            starts_with("eq5d"))

# Categorizamos la edad.
edat_breaks <- c(18, 35, 50, 65, Inf)
edat_labels <- c("18-34", "35-49", "50-64", "≥65 years")

socio_dem <- socio_dem %>%
  mutate(edad_cat = cut(Edat, breaks = edat_breaks, 
                        labels = edat_labels, right = FALSE))

# Categorizamos la calidad de vida.
vas_breaks <- c(-Inf, 40, 53, 76, 80, Inf)
vas_labels <- c("Poor", "Fair", "Good", "Very Good", "Excellent")

socio_dem <- socio_dem %>%
  mutate(vas_cat = cut(eq5d_vas2_spa_spa, breaks = vas_breaks, 
                        labels = vas_labels, right = TRUE))

# Calculamos la calidad de vida.
quality <- socio_dem%>%select(email, starts_with("eq5d"))
quality <- quality%>%select(-eq5d_vas2_spa_spa)

euroqol <- data_frame(MO=quality$eq5d_mb_3l_spa_spa,
                      SC=quality$eq5d_sc_3l_spa_spa,
                      UA=quality$eq5d_ua_3l_spa_spa,
                      PD=quality$eq5d_pd_3l_spa_spa,
                      AD=quality$eq5d_ad_3l_spa_spa)

quality <- quality %>% na.omit()
euroqol <- euroqol %>% na.omit()

quality$eq5d_3l <- eq5d(scores=euroqol, country="Spain",
                version="3L", type="VAS")

quality <- quality%>%select(email, eq5d_3l)

# Cogemos los datos demográficos.
socio_dem <- socio_dem %>%
  select(-ends_with("altres"), -starts_with("eq5d"))

socio_dem <- merge(socio_dem, quality, by = "email", all.x = TRUE)

# Separamos por sexo.
socio_dem_h <- filter(socio_dem, sexe == 2)
socio_dem_m <- filter(socio_dem, sexe == 1)

# Hacemos tablas de frecuencias.
variables_categoricas <- socio_dem %>% select(-email, -Edat, -eq5d_3l)

contar_frecuencias <- function(df, variable, total) {
  freqs <- table(df[[variable]])
  freqs_df <- as.data.frame(freqs)
  colnames(freqs_df) <- c(variable, "n")
  freqs_df$percent <- round((freqs_df$n / nrow(total)) * 100, 2)
  freqs_df$combined <- paste(freqs_df$n, " (", freqs_df$percent, ")", sep = "")
  freqs_df
}

variables <- colnames(variables_categoricas)

tablas_frecuencia <- lapply(variables, function(var) {
  contar_frecuencias(variables_categoricas, var, socio_dem)
})

names(tablas_frecuencia) <- variables

# Mostrar todas las tablas de frecuencia.
for (var in variables) {
  cat("\nFrecuencias para la variable:", var, "\n")
  print(tablas_frecuencia[[var]][, c(var, "combined")])
}

# Miramos los quantils y la media.
summary(socio_dem$Edat)
summary(socio_dem$eq5d_3l)

# Lo mismo para cada sexo.
variables_categoricas_h <- socio_dem_h %>% select(-email, -Edat, -eq5d_3l)

variables_h <- colnames(variables_categoricas_h)

tablas_frecuencia_h <- lapply(variables_h, function(var) {
  contar_frecuencias(variables_categoricas_h, var, socio_dem_h)
})

names(tablas_frecuencia_h) <- variables_h

for (var in variables_h) {
  cat("\nFrecuencias para la variable:", var, "\n")
  print(tablas_frecuencia_h[[var]][, c(var, "combined")])
}

summary(socio_dem_h$Edat)
summary(socio_dem_h$eq5d_3l)

variables_categoricas_m <- socio_dem_m %>% select(-email, -Edat, -eq5d_3l)

variables_m <- colnames(variables_categoricas_m)

tablas_frecuencia_m <- lapply(variables_m, function(var) {
  contar_frecuencias(variables_categoricas_m, var, socio_dem_m)
})

names(tablas_frecuencia_m) <- variables_m

for (var in variables_m) {
  cat("\nFrecuencias para la variable:", var, "\n")
  print(tablas_frecuencia_m[[var]][, c(var, "combined")])
}

summary(socio_dem_m$Edat)
summary(socio_dem_m$eq5d_3l)

# Ahora hacemos lo mismo para el año de seguimiento.
df_combined3 <- merge(df_combined3, socio_dem, 
                      by.x = "codi_seguiment", by.y = "email")

socio_dem_1any <- df_combined3%>%select(codi_seguiment, sexe, Edat, starts_with("situacio_l"),
                                        treball_sanitari, comorbiditats___0,
                                        tractament___0, alguna_situacio___4,
                                        alguna_situacio___6, alguna_situacio___7,
                                        starts_with("eq5d"), edad_cat, vas_cat)

socio_dem_1any <- socio_dem_1any %>%
  mutate(vas_cat_1any = cut(eq5d_vas2_spa_spa, breaks = vas_breaks, 
                       labels = vas_labels, right = TRUE))

quality_1any <- socio_dem_1any%>%select(codi_seguiment, starts_with("eq5d"))
quality_1any <- quality_1any%>%select(-eq5d_vas2_spa_spa, -eq5d_3l)

euroqol_1any <- data_frame(MO=quality_1any$eq5d_mb_3l_spa_spa,
                           SC=quality_1any$eq5d_sc_3l_spa_spa,
                           UA=quality_1any$eq5d_ua_3l_spa_spa,
                           PD=quality_1any$eq5d_pd_3l_spa_spa,
                           AD=quality_1any$eq5d_ad_3l_spa_spa)

quality_1any <- quality_1any %>% na.omit()
euroqol_1any <- euroqol_1any %>% na.omit()

quality_1any$eq5d_3l <- eq5d(scores=euroqol_1any, country="Spain",
                             version="3L", type="VAS")

quality_1any <- quality_1any%>%select(codi_seguiment, eq5d_3l)

socio_dem_1any <- socio_dem_1any %>%
  select(-ends_with("altres"), -starts_with("eq5d"))

socio_dem_1any <- merge(socio_dem_1any, quality_1any, by = "codi_seguiment", all.x = TRUE)

socio_dem_1any_h <- filter(socio_dem_1any, sexe == 2)
socio_dem_1any_m <- filter(socio_dem_1any, sexe == 1)

variables_categoricas_1any <- socio_dem_1any %>% select(-codi_seguiment, 
                                                        -Edat, -eq5d_3l)

variables_1any <- colnames(variables_categoricas_1any)

tablas_frecuencia_1any <- lapply(variables_1any, function(var) {
  contar_frecuencias(variables_categoricas_1any, var, socio_dem_1any)
})

names(tablas_frecuencia_1any) <- variables_1any

for (var in variables) {
  cat("\nFrecuencias para la variable:", var, "\n")
  print(tablas_frecuencia_1any[[var]][, c(var, "combined")])
}

summary(socio_dem_1any$Edat)
summary(socio_dem_1any$eq5d_3l)


variables_categoricas_1any_h <- socio_dem_1any_h %>% select(-codi_seguiment, 
                                                        -Edat, -eq5d_3l)

variables_1any_h <- colnames(variables_categoricas_1any_h)

tablas_frecuencia_1any_h <- lapply(variables_1any_h, function(var) {
  contar_frecuencias(variables_categoricas_1any_h, var, socio_dem_1any_h)
})

names(tablas_frecuencia_1any_h) <- variables_1any_h

for (var in variables) {
  cat("\nFrecuencias para la variable:", var, "\n")
  print(tablas_frecuencia_1any_h[[var]][, c(var, "combined")])
}

summary(socio_dem_1any_h$Edat)
summary(socio_dem_1any_h$eq5d_3l)

variables_categoricas_1any_m <- socio_dem_1any_m %>% select(-codi_seguiment, 
                                                            -Edat, -eq5d_3l)

variables_1any_m <- colnames(variables_categoricas_1any_m)

tablas_frecuencia_1any_m <- lapply(variables_1any_m, function(var) {
  contar_frecuencias(variables_categoricas_1any_m, var, socio_dem_1any_m)
})

names(tablas_frecuencia_1any_m) <- variables_1any_m

for (var in variables) {
  cat("\nFrecuencias para la variable:", var, "\n")
  print(tablas_frecuencia_1any_m[[var]][, c(var, "combined")])
}

summary(socio_dem_1any_m$Edat)
summary(socio_dem_1any_m$eq5d_3l)

# Lo mismo para el segundo año de seguimiento.
df_combined4 <- merge(df_combined4, socio_dem, 
                      by.x = "codi_seguiment", by.y = "email")

socio_dem_2any <- df_combined4%>%select(codi_seguiment, sexe, Edat, starts_with("situacio_l"),
                                        treball_sanitari, comorbiditats___0,
                                        tractament___0, alguna_situacio___4,
                                        alguna_situacio___6, alguna_situacio___7,
                                        starts_with("eq5d"), edad_cat, vas_cat)

socio_dem_2any <- socio_dem_2any %>%
  mutate(vas_cat_2any = cut(eq5d_vas2_spa_spa, breaks = vas_breaks, 
                            labels = vas_labels, right = TRUE))

quality_2any <- socio_dem_2any%>%select(codi_seguiment, starts_with("eq5d"))
quality_2any <- quality_2any%>%select(-eq5d_vas2_spa_spa, -eq5d_3l)

euroqol_2any <- data_frame(MO=quality_2any$eq5d_mb_3l_spa_spa,
                           SC=quality_2any$eq5d_sc_3l_spa_spa,
                           UA=quality_2any$eq5d_ua_3l_spa_spa,
                           PD=quality_2any$eq5d_pd_3l_spa_spa,
                           AD=quality_2any$eq5d_ad_3l_spa_spa)

quality_2any <- quality_2any %>% na.omit()
euroqol_2any <- euroqol_2any %>% na.omit()

quality_2any$eq5d_3l <- eq5d(scores=euroqol_2any, country="Spain",
                             version="3L", type="VAS")

quality_2any <- quality_2any%>%select(codi_seguiment, eq5d_3l)

socio_dem_2any <- socio_dem_2any %>%
  select(-ends_with("altres"), -starts_with("eq5d"))

socio_dem_2any <- merge(socio_dem_2any, quality_2any, by = "codi_seguiment", all.x = TRUE)

socio_dem_2any_h <- filter(socio_dem_2any, sexe == 2)
socio_dem_2any_m <- filter(socio_dem_2any, sexe == 1)

variables_categoricas_2any <- socio_dem_2any %>% select(-codi_seguiment, 
                                                        -Edat, -eq5d_3l)

variables_2any <- colnames(variables_categoricas_2any)

tablas_frecuencia_2any <- lapply(variables_2any, function(var) {
  contar_frecuencias(variables_categoricas_2any, var, socio_dem_2any)
})

names(tablas_frecuencia_2any) <- variables_2any

for (var in variables) {
  cat("\nFrecuencias para la variable:", var, "\n")
  print(tablas_frecuencia_2any[[var]][, c(var, "combined")])
}

summary(socio_dem_2any$Edat)
summary(socio_dem_2any$eq5d_3l)


variables_categoricas_2any_h <- socio_dem_2any_h %>% select(-codi_seguiment, 
                                                            -Edat, -eq5d_3l)

variables_2any_h <- colnames(variables_categoricas_2any_h)

tablas_frecuencia_2any_h <- lapply(variables_2any_h, function(var) {
  contar_frecuencias(variables_categoricas_2any_h, var, socio_dem_2any_h)
})

names(tablas_frecuencia_2any_h) <- variables_2any_h

for (var in variables) {
  cat("\nFrecuencias para la variable:", var, "\n")
  print(tablas_frecuencia_2any_h[[var]][, c(var, "combined")])
}

summary(socio_dem_2any_h$Edat)
summary(socio_dem_2any_h$eq5d_3l)

variables_categoricas_2any_m <- socio_dem_2any_m %>% select(-codi_seguiment, 
                                                            -Edat, -eq5d_3l)

variables_2any_m <- colnames(variables_categoricas_2any_m)

tablas_frecuencia_2any_m <- lapply(variables_2any_m, function(var) {
  contar_frecuencias(variables_categoricas_2any_m, var, socio_dem_2any_m)
})

names(tablas_frecuencia_2any_m) <- variables_2any_m

for (var in variables) {
  cat("\nFrecuencias para la variable:", var, "\n")
  print(tablas_frecuencia_2any_m[[var]][, c(var, "combined")])
}

summary(socio_dem_2any_m$Edat)
summary(socio_dem_2any_m$eq5d_3l)

# Miramos si las variables con significante para el sexo.
library(stats)
library(broom)

# Transformar variables categóricas en factores.
socio_dem2 <- socio_dem %>%
  mutate(across(c(starts_with("situacio_laboral___"), 
                  treball_sanitari, comorbiditats___0, tractament___0,
                  alguna_situacio___4, alguna_situacio___6, alguna_situacio___7,
                  edad_cat, vas_cat), as.factor))

socio_dem2 <- socio_dem2 %>% filter(socio_dem2$sexe == 1 | socio_dem2$sexe == 2)

# Realizar para variables continuas.
continuous_vars <- c("Edat", "eq5d_3l")
continuous_p_values <- sapply(continuous_vars, function(var) {
  t.test(socio_dem2[[var]] ~ socio_dem2$sexe)$p.value
})

# Realizar para variables categóricas.
categorical_vars <- c("situacio_laboral___1", "situacio_laboral___2", "situacio_laboral___3",
                      "situacio_laboral___4", "situacio_laboral___5", "situacio_laboral___6",
                      "situacio_laboral___7", "situacio_laboral___8", "treball_sanitari",
                      "comorbiditats___0", "tractament___0", "alguna_situacio___4",
                      "alguna_situacio___6", "alguna_situacio___7", "edad_cat", "vas_cat")

categorical_p_values <- sapply(categorical_vars, function(var) {
  chisq.test(table(socio_dem2[[var]], socio_dem2$sexe))$p.value
})

# Combinar resultados.
p_values <- data.frame(
  Variable = c(continuous_vars, categorical_vars),
  P_Value = c(continuous_p_values, categorical_p_values)
)

print(p_values)

# Lo mismo para un año.
socio_dem_1any2 <- socio_dem_1any %>%
  mutate(across(c(starts_with("situacio_laboral___"), 
                  treball_sanitari, comorbiditats___0, tractament___0,
                  alguna_situacio___4, alguna_situacio___6, alguna_situacio___7,
                  edad_cat, vas_cat), as.factor))

socio_dem_1any2 <- socio_dem_1any2 %>% filter(socio_dem_1any2$sexe == 1 | socio_dem_1any2$sexe == 2)

continuous_p_values <- sapply(continuous_vars, function(var) {
  t.test(socio_dem_1any2[[var]] ~ socio_dem_1any2$sexe)$p.value
})

categorical_p_values <- sapply(categorical_vars, function(var) {
  chisq.test(table(socio_dem_1any2[[var]], socio_dem_1any2$sexe))$p.value
})

p_values <- data.frame(
  Variable = c(continuous_vars, categorical_vars),
  P_Value = c(continuous_p_values, categorical_p_values)
)

print(p_values)

# Lo mismo para dos años.
socio_dem_2any2 <- socio_dem_2any %>%
  mutate(across(c(starts_with("situacio_laboral___"), 
                  treball_sanitari, comorbiditats___0, tractament___0,
                  alguna_situacio___4, alguna_situacio___6, alguna_situacio___7,
                  edad_cat, vas_cat), as.factor))

socio_dem_2any2 <- socio_dem_2any2 %>% filter(socio_dem_2any2$sexe == 1 | socio_dem_2any2$sexe == 2)

continuous_p_values <- sapply(continuous_vars, function(var) {
  t.test(socio_dem_2any2[[var]] ~ socio_dem_2any2$sexe)$p.value
})

categorical_p_values <- sapply(categorical_vars, function(var) {
  chisq.test(table(socio_dem_2any2[[var]], socio_dem_2any2$sexe))$p.value
})

p_values <- data.frame(
  Variable = c(continuous_vars, categorical_vars),
  P_Value = c(continuous_p_values, categorical_p_values)
)

print(p_values)

# Sumamos los sintomas por tiempo y sexo.
sint_basal <- basal%>%select(129:479)
sint_inici <-  sint_basal %>% select(ends_with("__1"))
sint_inici <- as.data.frame(sapply(sint_inici, as.numeric))
sint_inici$total_sintomas <- rowSums(sint_inici, na.rm = TRUE)
sint_22 <-  sint_basal %>% select(ends_with("__2"))
sint_22 <- as.data.frame(sapply(sint_22, as.numeric))
sint_22$total_sintomas <- rowSums(sint_22, na.rm = TRUE)
sint_actual <-  sint_basal %>% select(ends_with("__3"))
sint_actual <- as.data.frame(sapply(sint_actual, as.numeric))
sint_actual$total_sintomas <- rowSums(sint_actual, na.rm = TRUE)
sint_1any <- df_combined3%>%select(13:129)
sint_1any <- ifelse(is.na(sint_1any) | sint_1any <= 0, 0, 1)
sint_1any <- as.data.frame(sint_1any)
sint_1any$total_sintomas <- rowSums(sint_1any, na.rm = TRUE)
sint_2any <- df_combined4%>%select(13:129)
sint_2any <- ifelse(is.na(sint_2any) | sint_2any <= 0, 0, 1)
sint_2any <- as.data.frame(sint_2any)
sint_2any$total_sintomas <- rowSums(sint_2any, na.rm = TRUE)

summary(sint_inici$total_sintomas)
summary(sint_22$total_sintomas)
summary(sint_actual$total_sintomas)

sint_basal_m <- basal%>%select(sexe, 129:479)
sint_basal_m <- filter(sint_basal_m, sexe == 1)
sint_basal_m <- sint_basal_m%>%select(-sexe)
sint_inici_m <-  sint_basal_m %>% select(ends_with("__1"))
sint_inici_m <- as.data.frame(sapply(sint_inici_m, as.numeric))
sint_inici_m$total_sintomas <- rowSums(sint_inici_m, na.rm = TRUE)
sint_22_m <-  sint_basal_m %>% select(ends_with("__2"))
sint_22_m <- as.data.frame(sapply(sint_22_m, as.numeric))
sint_22_m$total_sintomas <- rowSums(sint_22_m, na.rm = TRUE)
sint_actual_m <-  sint_basal_m %>% select(ends_with("__3"))
sint_actual_m <- as.data.frame(sapply(sint_actual_m, as.numeric))
sint_actual_m$total_sintomas <- rowSums(sint_actual_m, na.rm = TRUE)

summary(sint_inici_m$total_sintomas)
summary(sint_22_m$total_sintomas)
summary(sint_actual_m$total_sintomas)

sint_basal_h <- filter(basal, sexe == 2)
sint_inici_h <-  sint_basal_h %>% select(ends_with("__1"))
sint_inici_h <- as.data.frame(sapply(sint_inici_h, as.numeric))
sint_inici_h$total_sintomas <- rowSums(sint_inici_h, na.rm = TRUE)
sint_22_h <-  sint_basal_h %>% select(ends_with("__2"))
sint_22_h <- as.data.frame(sapply(sint_22_h, as.numeric))
sint_22_h$total_sintomas <- rowSums(sint_22_h, na.rm = TRUE)
sint_actual_h <-  sint_basal_h %>% select(ends_with("__3"))
sint_actual_h <- as.data.frame(sapply(sint_actual_h, as.numeric))
sint_actual_h$total_sintomas <- rowSums(sint_actual_h, na.rm = TRUE)

summary(sint_inici_h$total_sintomas)
summary(sint_22_h$total_sintomas)
summary(sint_actual_h$total_sintomas)

sint_1any <- df_combined3%>%select(12:128)
sint_1any <- sint_1any %>%
  mutate_all(~ ifelse(is.na(.), 0, ifelse(. > 0, 1, 0)))
sint_1any$total_sintomas <- rowSums(sint_1any, na.rm = TRUE)

sint_1any_h <- df_combined3[df_combined3$codi_seguiment %in% socio_dem_1any_h$codi_seguiment, ]
sint_1any_h <- sint_1any_h%>%select(12:128)
sint_1any_h <- sint_1any_h %>%
  mutate_all(~ ifelse(is.na(.), 0, ifelse(. > 0, 1, 0)))
sint_1any_h$total_sintomas <- rowSums(sint_1any_h, na.rm = TRUE)

sint_1any_m <- df_combined3[df_combined3$codi_seguiment %in% socio_dem_1any_m$codi_seguiment, ]
sint_1any_m <- sint_1any_m%>%select(12:128)
sint_1any_m <- sint_1any_m %>%
  mutate_all(~ ifelse(is.na(.), 0, ifelse(. > 0, 1, 0)))
sint_1any_m$total_sintomas <- rowSums(sint_1any_m, na.rm = TRUE)

sint_2any <- df_combined4%>%select(12:128)
sint_2any <- sint_2any %>%
  mutate_all(~ ifelse(is.na(.), 0, ifelse(. > 0, 1, 0)))
sint_2any$total_sintomas <- rowSums(sint_2any, na.rm = TRUE)

sint_2any_h <- df_combined4[df_combined4$codi_seguiment %in% socio_dem_2any_h$codi_seguiment, ]
sint_2any_h <- sint_2any_h%>%select(12:128)
sint_2any_h <- sint_2any_h %>%
  mutate_all(~ ifelse(is.na(.), 0, ifelse(. > 0, 1, 0)))
sint_2any_h$total_sintomas <- rowSums(sint_2any_h, na.rm = TRUE)

sint_2any_m <- df_combined4[df_combined4$codi_seguiment %in% socio_dem_2any_m$codi_seguiment, ]
sint_2any_m <- sint_2any_m%>%select(12:128)
sint_2any_m <- sint_2any_m %>%
  mutate_all(~ ifelse(is.na(.), 0, ifelse(. > 0, 1, 0)))
sint_2any_m$total_sintomas <- rowSums(sint_2any_m, na.rm = TRUE)

summary(sint_1any$total_sintomas)
summary(sint_1any_h$total_sintomas)
summary(sint_1any_m$total_sintomas)
summary(sint_2any$total_sintomas)
summary(sint_2any_h$total_sintomas)
summary(sint_2any_m$total_sintomas)

# Crear variables para las agrupaciones de sintomas.
agrupaciones_sintomas <- list(
  simptomes_respiratoris = c("simp_tos_prod", "simp_tos_seca", 
                             "simp_falta_aire", "simp_f_a_mod", 
                             "simp_f_a_petit", "simp_esput", 
                             "simp_insp_incomp", "simp_resp_anorm", 
                             "simp_llavis_blaus", "simp_baix_o2",
                             "simp_f_a_repos"),
  
  simptomes_cardio = c("simp_palpi", "simp_dolor_torasic", 
                       "simp_d_t_opre", "simp_d_t_cremor", 
                       "simp_d_t_altre", "simp_taquicar", 
                       "simp_bradicar", "simp_hipotens", 
                       "simp_hta"),
  
  simptomes_NRL = c("simp_mal_cap", "simp_mareig",
                    "simp_formig", "simp_deb_musc",
                    "simp_tremolor", "simp_convulsions", 
                    "simp_hipoestesia", "simp_rampes", 
                    "simp_fascicu", "simp_descoordi",
                    "simp_dif_motrici", "simp_insomni_conci", 
                    "simp_insomni_manten"),
  
  simptomes_neurocognitius = c("simp_dif_concen", "simp_oblits", 
                               "simp_recordar", "simp_desorientacio",
                               "simp_anomia", "simp_alexia"),
  
  simptomes_disautonomics = c("simp_suor", "simp_orina_molt",
                              "simp_olor_cos"),
  
  olfacte_gust = c("simp_perdua_gust", "simp_perdua_olf", 
                   "simp_cacosmia", "simp_fantosmia"),
  
  alteracions_olfacte = c("simp_perdua_olf", "simp_cacosmia", 
                          "simp_fantosmia"),
  
  simptomes_ORL = c("simp_perd_audicio", "simp_exces_audicio",
                    "simp_sec_gola", "simp_gust_sang_gola", 
                    "simp_acufens", "simp_mal_orella", 
                    "simp_sec_nas", "simp_hemo_nas", 
                    "simp_raspera", "simp_disfonia",
                    "simp_afonia", "simp_llagues"),
  
  simptomes_vies_altes = c("simp_picor_nas", "simp_esternuts", 
                           "simp_mucositat", "simp_congestio",
                           "simp_mal_gola", "simp_picor_gola"),
  
  simptomes_generals = c("simp_disterm", "simp_t_1", 
                         "simp_t_2", "simp_t_3", 
                         "simp_t_4", "simp_calfreds", 
                         "simp_dism_pes", "simp_inapetencia", 
                         "simp_d_musc", "simp_herpes", 
                         "simp_cansament", "simp_malestar"),
  
  simptomes_reuma = c("simp_infla_arti", "simp_d_clatell", 
                      "simp_d_costelles", "simp_d_esquena", 
                      "simp_d_cost_dret", "simp_d_cost_esq", 
                      "simp_d_arti", "simp_d_lesions", 
                      "simp_d_punxades"),
  
  simptomes_dermatologics = c("simp_cau_cabell", "simp_pell_seca", 
                              "simp_pell_picor", "simp_pell_erupcio", 
                              "simp_eritema", "simp_cutis_marmo"),
  
  simptomes_oftalmologics = c("simp_ulls_secs", "simp_ulls_dolor", 
                              "simp_conjuntivitis", "simp_ulls_vermell", 
                              "simp_visio_borros", "simp_diplopia", 
                              "simp_fotofobia"),
  
  simptomes_digestius = c("simp_d_abdo", "simp_mal_estomac", 
                          "simp_nausea", "simp_vomits", 
                          "simp_femta_moc", "simp_femta_sang", 
                          "simp_diarrea", "simp_femta_liq",
                          "simp_budells", "simp_gasos"),
  
  simptomes_ginecològics = c("simp_menst_dolor", "simp_molest_vaginals"),
  
  alteracions_menstruals = c("simp_menst_alterada", "simp_menst_durada", 
                             "simp_menst_vol"),
  
  simptomes_urologics = c("simp_molest_genitals", "simp_orina_cremor"),
  
  simptomes_esfera_sexual = c("simp_sex_menys", "simp_sex_impoten")
)

# Crear columnas para cada agrupacion.
for (agrupacion in names(agrupaciones_sintomas)) {
  columnas <- agrupaciones_sintomas[[agrupacion]]
  df_combined3[[paste0(agrupacion, "_1any")]] <- as.integer(rowSums(!is.na(df_combined3[, columnas]) & df_combined3[, columnas] != 0) > 0)
}

for (agrupacion in names(agrupaciones_sintomas)) {
  columnas <- agrupaciones_sintomas[[agrupacion]]
  df_combined4[[paste0(agrupacion, "_2any")]] <- as.integer(rowSums(!is.na(df_combined4[, columnas]) & df_combined4[, columnas] != 0) > 0)
}

# Sintomas por tiempo.
library(tibble)

sintomas_basal <- basal%>%select(890:943)
sintomas_inici <-  sintomas_basal %>% select(ends_with("inici"))
sintomas_tot_inici <- colSums(sintomas_inici, na.rm = TRUE)
porcentaje_inici <- sintomas_tot_inici/nrow(sintomas_inici) * 100
names(porcentaje_inici) <- gsub("_inici", "", colnames(sintomas_inici))
porcentaje_inici <- data.frame(porcentaje_inici)
porcentaje_inici <- porcentaje_inici %>%
  rownames_to_column(var = "sintoma")
names(porcentaje_inici) <- c("sintoma", "porcentaje")
porcentaje_inici <- porcentaje_inici %>%
  mutate(intervalo = "inici")

sintomas_22 <-  sintomas_basal %>% select(ends_with("22_60d"))
sintomas_tot_22 <- colSums(sintomas_22, na.rm = TRUE)
porcentaje_22 <- sintomas_tot_22/nrow(sintomas_22) * 100
names(porcentaje_22) <- gsub("_22_60d", "", colnames(sintomas_22))
porcentaje_22 <- data.frame(porcentaje_22)
porcentaje_22 <- porcentaje_22 %>%
  rownames_to_column(var = "sintoma")
names(porcentaje_22) <- c("sintoma", "porcentaje")
porcentaje_22 <- porcentaje_22 %>%
  mutate(intervalo = "22_60d")

sintomas_actual <-  sintomas_basal %>% select(ends_with("actual"))
sintomas_tot_actual <- colSums(sintomas_actual, na.rm = TRUE)
porcentaje_actual <- sintomas_tot_actual/nrow(sintomas_actual) * 100
names(porcentaje_actual) <- gsub("_actual", "", colnames(sintomas_actual))
porcentaje_actual <- data.frame(porcentaje_actual)
porcentaje_actual <- porcentaje_actual %>%
  rownames_to_column(var = "sintoma")
names(porcentaje_actual) <- c("sintoma", "porcentaje")
porcentaje_actual <- porcentaje_actual %>%
  mutate(intervalo = "actual")

sintomas_1any <- df_combined3 %>% select(ends_with("1any"))
sintomas_tot_1any <- colSums(sintomas_1any)
porcentaje_1any <- sintomas_tot_1any/nrow(sintomas_1any) * 100
names(porcentaje_1any) <- gsub("_1any", "", colnames(sintomas_1any))
porcentaje_1any <- data.frame(porcentaje_1any)
porcentaje_1any <- porcentaje_1any %>%
  rownames_to_column(var = "sintoma")
names(porcentaje_1any) <- c("sintoma", "porcentaje")
porcentaje_1any <- porcentaje_1any %>%
  mutate(intervalo = "1any")

sintomas_2any <- df_combined4 %>% select(ends_with("2any"))
sintomas_tot_2any <- colSums(sintomas_2any)
porcentaje_2any <- sintomas_tot_2any/nrow(sintomas_2any) * 100
names(porcentaje_2any) <- gsub("_2any", "", colnames(sintomas_2any))
porcentaje_2any <- data.frame(porcentaje_2any)
porcentaje_2any <- porcentaje_2any %>%
  rownames_to_column(var = "sintoma")
names(porcentaje_2any) <- c("sintoma", "porcentaje")
porcentaje_2any <- porcentaje_2any %>%
  mutate(intervalo = "2any")

# Agrupamos todos los sintomas.
sintomas_total_perc <- bind_rows(porcentaje_inici, porcentaje_22,
                                 porcentaje_actual, porcentaje_1any,
                                 porcentaje_2any)

orden_intervalos <- c("inici", "22_60d", "actual", "1any", "2any")
sintomas_total_perc$intervalo <- factor(sintomas_total_perc$intervalo, 
                                        levels = orden_intervalos)

# Vector de traducción
sintomas <- sintomas_total_perc$sintoma
sintomas_ingles <- c("Dermatological", "Ophthalmic", "Gynecological", 
                     "Menstrual alterations", "Urological", "Sexual-related issues", 
                     "Digestive", "Upper Respiratory Way", "Smell alterations", 
                     "ENT (others)", "Respiratory", "Cardiac", 
                     "Rheumatological", "General", "Neurological", 
                     "Neurocognitive", "Dysautonomic", "Taste & smell")

# Reemplazar nombres
sintomas_total_perc$sintoma <- sintomas_ingles[match(sintomas_total_perc$sintoma, 
                                                     sintomas)]


ggplot(sintomas_total_perc, aes(x = intervalo, y = porcentaje, fill = sintoma)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of symptoms at different time intervals", 
       x = "Time interval",
       y = "Percentage",
       fill = "Symptom") +
  scale_fill_viridis_d() + 
  theme_minimal() 

# Guardar el data frame en un archivo CSV.
write.csv(df_combined3, "COVIDPersistente-Seguimiento-1año-clean.csv", row.names = FALSE)
write.csv(df_combined4, "COVIDPersistente-Seguimiento-2año-clean.csv", row.names = FALSE)
write.csv(basal, "COVIDPersistent-basal-clean.csv", row.names = FALSE)
write.csv(socio_dem, "COVIDPersistent-sociodem.csv", row.names = FALSE)