library(devtools)
install_github("zcebeci/fcvalid")
library(fcvalid)
library(VIM)
library(readr)
library(dplyr)
library(eq5d)
library(ggplot2)
library(tidyr)
library(readxl)
library(plyr)
library(e1071)
library(fpc)
library(e1071)
library(ggmosaic)
library(circlize)
library(viridis)

# Datasets.
basal <- read_csv("COVIDPersistent-basal-clean.csv")
df_combined3 <- read_csv("COVIDPersistente-Seguimiento-1año-clean.csv")
df_combined4 <- read_csv("COVIDPersistente-Seguimiento-2año-clean.csv")
socio_dem <- read_csv("COVIDPersistent-sociodem.csv")

# Cogemos las variables que necesitamos.
data_s <- basal%>%select(email, sexe, Edat, 890:943)
data_combined3 <- df_combined3%>%select(codi_seguiment, ends_with("1any"), eq5d_mb_3l_spa_spa,
                                        eq5d_sc_3l_spa_spa, eq5d_ua_3l_spa_spa,
                                        eq5d_pd_3l_spa_spa, eq5d_ad_3l_spa_spa,
                                        eq5d_vas2_spa_spa)
data_combined4 <- df_combined4%>%select(codi_seguiment, ends_with("2any"), eq5d_mb_3l_spa_spa,
                                        eq5d_sc_3l_spa_spa, eq5d_ua_3l_spa_spa,
                                        eq5d_pd_3l_spa_spa, eq5d_ad_3l_spa_spa,
                                        eq5d_vas2_spa_spa)
data_s <- data_s %>% mutate_all(~ ifelse(is.na(.), 0, .))

# Juntamos todas bases de datos.
df_merged <- merge(data_s, data_combined3, by.x = "email", by.y = "codi_seguiment", all = TRUE)
df_merged <- merge(df_merged, data_combined4, by.x = "email", by.y = "codi_seguiment", all = TRUE)
column_order <- names(df_merged)[order(names(df_merged))]
df_merged <- df_merged[column_order]
df_merged <- df_merged%>%select(email, sexe, Edat, ends_with("inici"), ends_with("22_60d"), 
                                ends_with("actual"), ends_with("1any"), ends_with("2any"),
                                starts_with("eq5d"))

# Miramos los pacientes que han contestado algun seguimiento.
pacientes <- df_merged%>%select(email, ends_with("1any"), ends_with("2any"))
names_na <- which(rowSums(!is.na(pacientes[, -1])) > 0)
pacientes_na <- pacientes[names_na, ]
df_na <- df_merged[df_merged$email %in% pacientes_na$email, ]

# Variables para imputar.
var_imputed <- c(names(df_merged %>% select(ends_with("1any"))), 
                 names(df_merged %>% select(ends_with("2any"))), 
                 names(df_merged %>% select(starts_with("eq5d"))))
caracteristicas <- c("sexe", "Edat", 
                     names(df_merged %>% select(ends_with("inici"))), 
                     names(df_merged %>% select(ends_with("22_60d"))),
                     names(df_merged %>% select(ends_with("actual"))))

# Imputacion.
df_imputado <- kNN(df_merged, variable = var_imputed, dist_var = caracteristicas)
df_imputado_na <- kNN(df_na, variable = var_imputed, dist_var = caracteristicas)

df_imputado <- df_imputado%>%select(1:105)
df_imputado_na <- df_imputado_na%>%select(1:105)

# Categorizamos la calidad de vida.
vas_breaks <- c(-Inf, 40, 53, 76, 80, Inf)
vas_labels <- c("Poor", "Fair", "Good", "Very Good", "Excellent")

df_imputado <- df_imputado %>%
  mutate(vas_cat_2any = cut(eq5d_vas2_spa_spa.y, breaks = vas_breaks, 
                            labels = vas_labels, right = TRUE))

# Calculamos el EQ indice.
calidad_previa <- socio_dem%>%select(email, vas_cat, eq5d_3l)
calidad_2any <- df_imputado%>%select(email, ends_with(".y"))

euroqol_2any <- data_frame(MO=calidad_2any$eq5d_mb_3l_spa_spa.y,
                           SC=calidad_2any$eq5d_sc_3l_spa_spa.y,
                           UA=calidad_2any$eq5d_ua_3l_spa_spa.y,
                           PD=calidad_2any$eq5d_pd_3l_spa_spa.y,
                           AD=calidad_2any$eq5d_ad_3l_spa_spa.y)

calidad_2any$eq5d_3l_2any <- eq5d(scores=euroqol_2any, country="Spain",
                                  version="3L", type="VAS")

calidad_2any <- calidad_2any%>%select(email, eq5d_3l_2any)

df_imputado_na <- df_imputado_na %>%
  mutate(vas_cat_2any = cut(eq5d_vas2_spa_spa.y, breaks = vas_breaks, 
                            labels = vas_labels, right = TRUE))

calidad_2any2 <- df_imputado_na%>%select(email, ends_with(".y"))

euroqol_2any2 <- data_frame(MO=calidad_2any2$eq5d_mb_3l_spa_spa.y,
                           SC=calidad_2any2$eq5d_sc_3l_spa_spa.y,
                           UA=calidad_2any2$eq5d_ua_3l_spa_spa.y,
                           PD=calidad_2any2$eq5d_pd_3l_spa_spa.y,
                           AD=calidad_2any2$eq5d_ad_3l_spa_spa.y)

calidad_2any2$eq5d_3l_2any2 <- eq5d(scores=euroqol_2any2, country="Spain",
                                  version="3L", type="VAS")

calidad_2any2 <- calidad_2any2%>%select(email, eq5d_3l_2any2)

# Guardamos el VAS para despues.
calidad_correlacion <- df_imputado%>%select(email, eq5d_vas2_spa_spa.y)

# Escogemos las variables que necesitamos.
df_imputado <- df_imputado%>%select(email, sexe, Edat, ends_with("inici"), 
                                    ends_with("22_60d"), ends_with("actual"), 
                                    ends_with("1any"), ends_with("2any"))
df_imputado_na <- df_imputado_na%>%select(email, sexe, Edat, ends_with("inici"), 
                                          ends_with("22_60d"), ends_with("actual"), 
                                          ends_with("1any"), ends_with("2any"))

vas <- df_imputado%>%select(email, vas_cat_2any)
vas2 <- df_imputado_na%>%select(email, vas_cat_2any)

df_imputado <- df_imputado%>%select(-starts_with("vas"))
df_imputado_na <- df_imputado_na%>%select(-starts_with("vas"))

# Reducimos columnas, haciendo filas por cada intervalo de sintomas.
names <- c("email", "sexe", "Edat", 
           gsub("_inici", "", gsub("simptomes_", "", 
                                   names(df_merged%>%select(ends_with("_inici"))))))

data_c <- data.frame(t(rep(NA, length(names))))
data_d <- data.frame(t(rep(NA, length(names))))

names(data_c) <- names
for (i in 1:nrow(df_imputado)){
  for (j in 1:5){
    register = ifelse(j == 1, "inici", ifelse(j == 2, "22_60d",
                                              ifelse(j == 3,"actual", 
                                                     ifelse(j == 4, "1any", "2any"))))
    temp = df_imputado%>%select(email, sexe, Edat, ends_with(paste0("_",register)))
    names(temp) = names
    data_c = rbind(data_c, temp[i,])
  }
}

names(data_d) <- names
for (i in 1:nrow(df_imputado_na)){
  for (j in 1:5){
    register = ifelse(j == 1, "inici", ifelse(j == 2, "22_60d",
                                              ifelse(j == 3,"actual", 
                                                     ifelse(j == 4, "1any", "2any"))))
    temp = df_imputado_na%>%select(email, sexe, Edat, ends_with(paste0("_",register)))
    names(temp) = names
    data_d = rbind(data_d, temp[i,])
  }
}

# Añadimos un registro donde nos dirá que intervalo es de cada paciente.
data_c <- data_c[-1,]
data_c$Registro <- rep(c(1,2,3,4,5), times = nrow(df_imputado))
data_d <- data_d[-1,]
data_d$Registro <- rep(c(1,2,3,4,5), times = nrow(df_imputado_na))

# Funcion para conseguir lo meberships para los criterios.
getMemberships <- function(model, data, m){
  cc <- model$centers
  dm <- sapply(seq_len(nrow(data)),
               function(i) apply(cc, 1, function(v) parDist(rbind(data[i,], v), method = "euclidean", threads = 10)))
  
  ms <- t(apply(dm, 2,
                function(x) {
                  tmp <- 1/((x/sum(x))^(2/(m-1)))  # formula above
                  tmp/sum(tmp)  # normalization
                }))
  
  return(ms)
}

# Funcion para calculatar los k y m que validen el cluster fuzzy.
getIndexesFuzzyClustering<- function(model, data, k, i, m, threshold = 0.75, test = F){
  
  if(!test){
    memberships <- model$membership
  }else{
    memberships <- getMemberships(model, data, m)
  }
  # Calculamos los diferentes criterios de k y m.
  fukuyama <- fs(x = data, u = memberships, v = model$centers, m = m)
  part.coeff <- pc(u = memberships, m = m)
  part.ent <- pe(u = memberships, m = m)
  xie <- fs(x = data, u = memberships, v = model$centers, m = m)
  calinski <- calinhara(as.data.frame(data),model$cluster,k)
  certainty <- sum(apply(memberships, 1, function(x) any(x>threshold)))/nrow(memberships)
  meanMaxProb <- mean(apply(memberships, 1, function(x) max(x)))
  
  return(list(iteration = i,
              k = k,
              m = m,
              Fukuyama = fukuyama,
              Xie = xie,
              Part.coeff = part.coeff,
              Part.ent = part.ent,
              Calinski = calinski,
              Certainty = certainty,
              MeanMaxProb = meanMaxProb))
}

# Todos los pacientes.
# Metodo pcamix para reducir la dimensionalidad.
pcamix = PCAmixdata::PCAmix(X.quanti = data_c%>%select(Edat),
                            X.quali = data_c%>%select(-email, -Edat, -Registro)%>%
                              mutate_if(is.numeric, as.character),
                            ndim = ncol(data_c%>%select(-email, -Registro)),
                            rename.level = T,
                            graph = F)

karlisCritPCAMix = 1+2*sqrt((ncol(data_c)-1-2)/(nrow(data_c)-1))
pcamixTransf_filt = as.data.frame(
  pcamix$ind$coord[, 1:length(which(pcamix$eig[,1] >= karlisCritPCAMix))])
metricsPCA_filt = data.frame(t(rep(NA, 10)))
names(metricsPCA_filt) = c("iteration", "k", "m", "Fukuyama", "Xie", 
                           "Part.coeff", "Part.ent", "Calinski", 
                           "Certainty", "MeanMaxProb")

# Bucle de 100 para validar el metodo.
for (i in 1:100){
  for (k in 2:8){
    for (m in c(1.1, 1.2, 1.4, 1.8)){
      set.seed(i)
      modelPCA_filt <- cmeans(pcamixTransf_filt, k, m = m)
      metricsPCA_filt <- rbind(metricsPCA_filt,
                               as.data.frame(getIndexesFuzzyClustering(
                                 modelPCA_filt, pcamixTransf_filt, 
                                 k = k, i = i, m = m)))
    }
  }
}

metricsPCA_filt = metricsPCA_filt[-1,]

# Media de todos los datos obtenidos para diferentes k y m.
data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}

# Los datos conseguidos en un data frame.
metrics = metricsPCA_filt%>%tidyr::gather(key = "Metric", value = "Value", -k, -iteration, -m)
metrics = data_summary(metrics, varname = "Value", groupnames = c("k", "Metric", "m"))
names(metrics)[4] = "Value"
metrics$Value = signif(metrics$Value, 3)
metrics$Metric[metrics$Metric == "Calinski"] = "Calinski-Harabasz"
metrics$Metric[metrics$Metric == "Part.coeff"] = "Partition coefficient"
metrics$Metric[metrics$Metric == "Xie"] = "Xie-Beni"
metrics$Metric[metrics$Metric == "Part.ent"] = "Partition entropy"
metrics$Metric[metrics$Metric == "MeanMaxProb"] = "Maximum certainty"
metrics$Metric[metrics$Metric == "Certainty"] = "Certainty > 0.75"

metrics$Metric = factor(metrics$Metric, levels = c("Calinski-Harabasz", "Certainty > 0.75",
                                                   "Fukuyama", "Maximum certainty", 
                                                   "Partition coefficient", "Partition entropy", 
                                                   "Xie-Beni"))

# Pacientes que han realizado algun seguimiento.
pcamix2 = PCAmixdata::PCAmix(X.quanti = data_d%>%select(Edat),
                             X.quali = data_d%>%select(-email, -Edat, -Registro)%>%
                               mutate_if(is.numeric, as.character),
                             ndim = ncol(data_d%>%select(-email, -Registro)),
                             rename.level = T,
                             graph = F)

karlisCritPCAMix2 = 1+2*sqrt((ncol(data_d)-1-2)/(nrow(data_d)-1))
pcamixTransf_filt2 = as.data.frame(
  pcamix2$ind$coord[, 1:length(which(pcamix2$eig[,1]>=karlisCritPCAMix2))])
metricsPCA_filt2 = data.frame(t(rep(NA, 10)))
names(metricsPCA_filt2) = c("iteration", "k", "m", "Fukuyama", "Xie",
                            "Part.coeff", "Part.ent", "Calinski", 
                            "Certainty", "MeanMaxProb")

for (i in 1:100){
  for (k in 2:8){
    for (m in c(1.1, 1.2, 1.4, 1.8)){
      set.seed(i)
      modelPCA_filt2 <- cmeans(pcamixTransf_filt2, k, m = m)
      metricsPCA_filt2 <- rbind(metricsPCA_filt2,
                                as.data.frame(getIndexesFuzzyClustering(
                                  modelPCA_filt2, pcamixTransf_filt2, 
                                  k = k, i = i, m = m)))
    }
  }
}

metricsPCA_filt2 = metricsPCA_filt2[-1,]

metrics2 = metricsPCA_filt2%>%tidyr::gather(key = "Metric", value = "Value", -k, -iteration, -m)
metrics2 = data_summary(metrics2, varname = "Value", groupnames = c("k", "Metric", "m"))
names(metrics2)[4] = "Value"
metrics2$Value = signif(metrics2$Value, 3)
metrics2$Metric[metrics2$Metric == "Calinski"] = "Calinski-Harabasz"
metrics2$Metric[metrics2$Metric == "Part.coeff"] = "Partition coefficient"
metrics2$Metric[metrics2$Metric == "Xie"] = "Xie-Beni"
metrics2$Metric[metrics2$Metric == "Part.ent"] = "Partition entropy"
metrics2$Metric[metrics2$Metric == "MeanMaxProb"] = "Maximum certainty"
metrics2$Metric[metrics2$Metric == "Certainty"] = "Certainty > 0.75"

metrics2$Metric = factor(metrics2$Metric, levels = c("Calinski-Harabasz", "Certainty > 0.75",
                                                     "Fukuyama", "Maximum certainty", 
                                                     "Partition coefficient", "Partition entropy",
                                                     "Xie-Beni"))
# Vemos los datos conseguidos.
print(metrics)
print(metrics2)

# Todos los pacientes
# Realizamos el fuzzy con el mejor criterio.
set.seed(13)
model <- cmeans(pcamixTransf_filt, 5, m = 1.1)

# Conseguimos el perido y el cluster de cada paciente.
temp <- data.frame(ID = data_c$email, register = data_c$Registro, cluster = model$cluster)

# Realizamos la matriz de OE y de Exclusividad.
sintomas <- colnames(data_c)[4:21]
sintomas_ingles <- c("Menstrual alterations", "Smell alterations", "Taste & smell", "Cardiac", 
                     "Dermatological", "Digestive", "Dysautonomic", "Sexual-related issues", "General", 
                     "Gynecological", "Neurocognitive", "Neurological", "Ophthalmic", "ENT (others)", 
                     "Respiratory", "Rheumatological", "Urological", "Upper Respiratory Way")

data_f <- merge(data_c, temp, by.x = c("email", "Registro"),
                by.y = c("ID", "register"))
matriz_OE <- matrix(0, ncol = max(data_f$cluster), nrow = length(sintomas))
matriz_exclusividad <- matrix(0, ncol = max(data_f$cluster), nrow = length(sintomas))

# Calculamos los valores de cada matriz.
for (j in 1:length(sintomas)) { 
  sintoma <- sintomas[j]
  
  for (i in 1:max(data_f$cluster)) {  
    cluster_data <- data_f[data_f$cluster == i, ]  
    total_pacientes_cluster <- nrow(cluster_data)
    
    frecuencia_observada <- sum(cluster_data[[sintoma]])  
    frecuencia_esperada <- total_pacientes_cluster * (sum(data_f[[sintoma]]) / nrow(data_f))
    
    # Calcular OE
    oe <- frecuencia_observada / frecuencia_esperada
    
    # Calcular exclusividad
    exclusividad <- (frecuencia_observada / sum(data_f[[sintoma]]))
    
    matriz_OE[j, i] <- oe
    matriz_exclusividad[j, i] <- exclusividad
  }
}

rownames(matriz_OE) <- sintomas_ingles
colnames(matriz_OE) <- 1:max(data_f$cluster)
rownames(matriz_exclusividad) <- sintomas_ingles
colnames(matriz_exclusividad) <- 1:max(data_f$cluster)

# Función para hacer un heatmap.
heatmap_plot <- function(matriz, titulo) {
  datos <- reshape2::melt(as.matrix(matriz))
  ggplot(data = datos, aes(x = Var2, y = Var1)) +
    geom_tile(aes(fill = value)) +
    geom_text(aes(label = round(value, 2)), color = "black") +
    scale_fill_gradient(low = "white", high = "red") +
    labs(title = titulo, x = "Cluster", y = "Symptoms") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

heatmap_OE <- heatmap_plot(matriz_OE, "OE Matrix")
heatmap_exclusividad <- heatmap_plot(matriz_exclusividad, "Exclusivity Matrix")

# Vemos las matrices conseguidas.
print(heatmap_OE)
print(heatmap_exclusividad)

# Vamos hacer comparativa entre el OE y la Exclusividad
datos <- data.frame(Sintomas = heatmap_OE$data[,1], Clusters = heatmap_OE$data[,2], 
                    OE = heatmap_OE$data[,3], Exclusividad = heatmap_exclusividad$data[,3])
datos_melted <- reshape2::melt(datos, id.vars = c("Sintomas", "Clusters"), 
                               measure.vars = c("OE", "Exclusividad"), 
                               variable.name = "Variable")

# Diagrama por cada cluster.
ggplot(datos_melted, aes(x = Sintomas, y = value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Clusters, nrow = 1) +
  labs(title = "OE vs Exclusivity for Symptoms and Cluster", x = "Symptoms", 
       y = "Value", fill = "Variable") +  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Lo mismo para pacientes que han realizado el seguimiento alguna vez.
set.seed(13)
model2 <- cmeans(pcamixTransf_filt2, 5, m = 1.2)
temp2 <- data.frame(ID = data_d$email, register = data_d$Registro, cluster = model2$cluster)

Symptoms <- colnames(data_d)[4:21]
data_g <- merge(data_d, temp2, by.x = c("email", "Registro"),
                by.y = c("ID", "register"))
matriz_OE2 <- matrix(0, ncol = max(data_g$cluster), nrow = length(Symptoms))
matriz_exclusividad2 <- matrix(0, ncol = max(data_g$cluster), nrow = length(Symptoms))

for (j in 1:length(Symptoms)) { 
  Symptom <- Symptoms[j]
  
  for (i in 1:max(data_g$cluster)) {  
    cluster_data <- data_g[data_g$cluster == i, ]  
    total_pacientes_cluster <- nrow(cluster_data)
    
    frecuencia_observada <- sum(cluster_data[[Symptom]])  
    frecuencia_esperada <- total_pacientes_cluster * (sum(data_g[[Symptom]]) / nrow(data_g))
    
    # Calcular OE
    oe <- frecuencia_observada / frecuencia_esperada
    
    # Calcular exclusividad
    exclusividad <- (frecuencia_observada / sum(data_g[[Symptom]]))
    
    matriz_OE2[j, i] <- oe
    matriz_exclusividad2[j, i] <- exclusividad
  }
}

rownames(matriz_OE2) <- sintomas_ingles  # Nombres de los síntomas
colnames(matriz_OE2) <- 1:max(data_g$cluster)  # Nombres de los clusters
rownames(matriz_exclusividad2) <- sintomas_ingles
colnames(matriz_exclusividad2) <- 1:max(data_g$cluster)

heatmap_OE2 <- heatmap_plot(matriz_OE2, "OE Matrix")
heatmap_exclusividad2 <- heatmap_plot(matriz_exclusividad2, "Exclusivity Matrix")

print(heatmap_OE2)
print(heatmap_exclusividad2)

datos2 <- data.frame(Sintomas = heatmap_OE2$data[,1], Clusters = heatmap_OE2$data[,2], 
                     OE = heatmap_OE2$data[,3], Exclusividad = heatmap_exclusividad2$data[,3])
datos_melted2 <- reshape2::melt(datos2, id.vars = c("Sintomas", "Clusters"), 
                                measure.vars = c("OE", "Exclusividad"), 
                                variable.name = "Variable")

ggplot(datos_melted2, aes(x = Sintomas, y = value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Clusters, nrow = 1) +
  labs(title = "OE vs Exclusivity for Symptoms and Cluster", x = "Symptoms", 
       y = "Value", fill = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Todos los pacientes.
# Realizamos la prevalencia.
# Nombramos los clusters y los registros conseguidos.
temp = temp%>%mutate(register = factor(ifelse(register == 1, "Days 0-20", 
                                              ifelse(register == 2, "Days 21-60",
                                                     ifelse(register == 3, "\U2265 3 months",
                                                            ifelse(register==4, "1 year", 
                                                                   "2 year")))), 
                                       levels = c("Days 0-20", "Days 21-60", "\U2265 3 months",
                                                  "1 year", "2 year")),
                     cluster = factor(ifelse(cluster == 1, "Menstrual &\n sexual alterations", 
                                             ifelse(cluster == 2, "Taste & smell",
                                                    ifelse(cluster == 3, "Heterogeneous",
                                                           ifelse(cluster == 4, "Multisystemic", 
                                                                  "Multisystemic -\npredominatly\ngenerals symptoms"))))))


temp$register = as.character(temp$register)
temp$register = factor(temp$register, levels = c("Days 0-20", "Days 21-60",
                                                 "\U2265 3 months", "1 year", "2 year"))

# Agrupar por registro y cluster, calculamos la cantidad y el porcentaje.
temp <- temp[order(temp$register, temp$cluster), ]
grouped_data <- split(temp, interaction(temp$register, temp$cluster))
N <- sapply(grouped_data, nrow)
distinct_data <- lapply(grouped_data, function(x) x[!duplicated(x$register), ])
Perc <- round(100 * N / nrow(df_imputado), 2)
Label <- paste0(N, " (", Perc, "%)")

# Lo agrupamos.
for (i in seq_along(distinct_data)) {
  distinct_data[[i]]$N <- N[i]
  distinct_data[[i]]$Perc <- Perc[i]
  distinct_data[[i]]$Label <- Label[i]
}
result <- do.call(rbind, distinct_data)

# Prevalencia.
prevalencia <- ggplot(data = temp) +
  geom_mosaic(aes(x = product(cluster, register), fill = cluster)) +
  labs(y = "Cluster", x = "Register")+
  theme_minimal()+
  theme(legend.position = "top", legend.title = element_blank())+
  scale_fill_viridis_d()

# Añadimos la cantidad y el porcentaje
df2 <- ggplot_build(prevalencia)$data[[1]]
df2 <- merge(df2, result, by.x = c("x__register", "x__fill__cluster"),
             by.y = c("register", "cluster"), all.x = TRUE)

prevalencia <- prevalencia + geom_label(data = df2, 
                                        aes(x = (xmin+xmax)/2, 
                                            y = (ymin+ymax)/2, label = Label))

prevalencia

# Trayectorias.
temp_cp = temp

# Cogemos principio y final.
temp_cp$FirstObservation = !duplicated(temp_cp$ID)
temp_cp$LastObservation = rev(!duplicated(rev(temp_cp$ID)))
temp_cp$Origin = ifelse(temp_cp$FirstObservation,
                        "From", ifelse(temp_cp$LastObservation, "To", NA))
temp_cp = temp_cp%>%filter(!is.na(Origin))
colsClusters = viridis::viridis(5)
names(colsClusters) = unique(temp$cluster)
grid.col = structure(rep(colsClusters,2), 
                     names = c(paste0("To_", names(colsClusters)),
                               paste0("From_",names(colsClusters))))

# Base de datos con las trayectorias de cada pacientes.
temp_cp = temp_cp%>%select(ID, Origin, cluster)%>%
  reshape(idvar = "ID", timevar = "Origin", direction = "wide")%>%
  mutate(cluster.To = as.character(ifelse(is.na(cluster.To), 
                                          as.character(cluster.From), 
                                          as.character(cluster.To))))

# Caracterizacion por sexo y edad.
socio_dem_clust <- merge(socio_dem, temp_cp, by.x = "email", by.y = "ID")
socio_dem_clust <-  socio_dem_clust%>%select(sexe, edad_cat, cluster.From,
                                             cluster.To)

caracterizacion_edad <- with(socio_dem_clust, table(cluster.From, cluster.To, edad_cat))
caracterizacion_sexo <- with(socio_dem_clust, table(cluster.From, cluster.To, sexe))

print(caracterizacion_edad)
print(caracterizacion_sexo)

# Caracterizacion de la calidad de vida.
calidad_clust <- merge(calidad_2any, vas, by="email")
calidad_clust <- merge(calidad_clust, calidad_previa, by="email")
calidad_clust <- merge(calidad_clust, temp_cp, by.x = "email", by.y = "ID")
calidad_clust <- na.omit(calidad_clust)

# Calcular la variación.
calidad_clusta <- calidad_clust %>%
  mutate(change_in_vas = as.numeric(factor(vas_cat_2any)) - as.numeric(factor(vas_cat)))
calidad_clusta <- calidad_clusta %>%
  mutate(change_in_eq5d = eq5d_3l_2any - eq5d_3l)
summary_data <- aggregate(cbind(change_in_vas, change_in_eq5d) ~ cluster.From + cluster.To, 
                          calidad_clusta, function(x) c(mean = mean(x, na.rm = TRUE), 
                                                        sd = sd(x, na.rm = TRUE)))
summary_data <- do.call(data.frame, summary_data)

# Diagrama de la variacion.
ggplot(summary_data, aes(x = cluster.From, y = change_in_eq5d.mean, fill = cluster.To)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Change in EQ-5D by Cluster Trajectory.",
       x = "Baseline Cluster",
       y = "Mean Change in EQ-5D",
       fill = "Final Cluster") +
  theme_minimal()

ggplot(summary_data, aes(x = cluster.From, y = change_in_vas.mean, fill = cluster.To)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Change in Quality of Life (VAS) by Cluster Trajectory",
       x = "Baseline Cluster",
       y = "Average Change in VAS",
       fill = "Final Cluster") +
  theme_minimal()

# Grafico de las trayectorias.
temp_cp = temp_cp%>%
  with(table(cluster.From, cluster.To))


colnames(temp_cp) = paste0("To_", colnames(temp_cp))
row.names(temp_cp) = paste0("From_", row.names(temp_cp))

# Haemos un diagrama de cuerdas.
par(mar = c(0.1, 0.1, 0.1, 0.1), cex = 2)
chordDiagram(as.data.frame(temp_cp), 
             grid.col = grid.col,
             transparency = 0.3,  
             direction.type = c("arrows", "diffHeight"), 
             diffHeight  = -0.005,
             link.arr.type = "big.arrow", 
             link.sort = T, 
             link.largest.ontop = T,
             direction = 1,
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = max(strwidth(
               unlist(dimnames(temp))))/1.75))

# Añadimos los nombres de las trayectorias.
circos.track(track.index = 1, 
             panel.fun = function(x, y) {
               circos.text(x = CELL_META$xcenter, 
                           y = CELL_META$ylim[1], 
                           labels = strsplit(CELL_META$sector.index, "_")[[1]][2],
                           facing = "clockwise", 
                           niceFacing = TRUE, 
                           adj = c(0, 0.5), 
                           cex = 0.4)
             }, 
             bg.border = NA)

# Matriz de transicion.
temp_cp <- as.data.frame(temp_cp)
sum_by_cluster <- aggregate(Freq ~ cluster.From, data = temp_cp, FUN = sum)
temp_cp <- merge(temp_cp, sum_by_cluster, by = "cluster.From", suffixes = c("", "_sum"))
temp_cp$Porcentaje <- temp_cp$Freq / temp_cp$Freq_sum * 100
temp_cp <- subset(temp_cp, select = -c(Freq_sum))

ggplot(temp_cp, aes(x = cluster.To, y = cluster.From, fill = Porcentaje, 
                    label = paste(Freq, " (", round(Porcentaje, 2), "%)"))) +
  geom_tile(color = "white") +
  geom_text(color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue", name = "Percentage (%)") +
  labs(title = "Transition Matrix", x = "Cluster To", y = "Cluster From", 
       fill = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Lo mismo para los pacientes que han hecho algun seguimiento.
temp2 = temp2%>%mutate(register = factor(ifelse(register == 1, "Days 0-20", 
                                                ifelse(register == 2, "Days 21-60",
                                                       ifelse(register == 3, "\U2265 3 months",
                                                              ifelse(register==4, "1 year", 
                                                                     "2 year")))), 
                                         levels = c("Days 0-20", "Days 21-60", "\U2265 3 months", 
                                                    "1 year", "2 year")),
                       cluster = factor(ifelse(cluster == 1, "Taste & smell", 
                                               ifelse(cluster == 2, "Multisystemic",
                                                      ifelse(cluster == 3, 
                                                             "Multisystemic -\npredominatly\ngenerals symptoms",
                                                             ifelse(cluster == 4, 
                                                                    "Menstrual &\n sexual alterations", 
                                                                    "Heterogeneous"))))))

temp2$register = as.character(temp2$register)
temp2$register = factor(temp2$register, levels = c("Days 0-20", "Days 21-60",
                                                   "\U2265 3 months", "1 year", "2 year"))

temp2 <- temp2[order(temp2$register, temp2$cluster), ]
grouped_data <- split(temp2, interaction(temp2$register, temp2$cluster))
N <- sapply(grouped_data, nrow)
distinct_data <- lapply(grouped_data, function(x) x[!duplicated(x$register), ])
Perc <- round(100 * N / nrow(df_imputado_na), 2)
Label <- paste0(N, " (", Perc, "%)")

for (i in seq_along(distinct_data)) {
  distinct_data[[i]]$N <- N[i]
  distinct_data[[i]]$Perc <- Perc[i]
  distinct_data[[i]]$Label <- Label[i]
}
result <- do.call(rbind, distinct_data)

prevalencia2 <- ggplot(data = temp2) +
  geom_mosaic(aes(x = product(cluster, register), fill = cluster)) +
  labs(y = "Cluster", x = "Register")+
  theme_minimal()+
  theme(legend.position = "top", legend.title = element_blank())+
  scale_fill_viridis_d()

df3 <- ggplot_build(prevalencia2)$data[[1]]
df3 <- merge(df3, result, by.x = c("x__register", "x__fill__cluster"),
             by.y = c("register", "cluster"), all.x = TRUE)

prevalencia2 <- prevalencia2 + geom_label(data = df3, 
                                          aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label = Label))

prevalencia2

temp_cp2 = temp2
temp_cp2$FirstObservation = !duplicated(temp_cp2$ID)
temp_cp2$LastObservation = rev(!duplicated(rev(temp_cp2$ID)))
temp_cp2$Origin = ifelse(temp_cp2$FirstObservation,
                         "From", ifelse(temp_cp2$LastObservation, "To", NA))
temp_cp2 = temp_cp2%>%filter(!is.na(Origin))

colsClusters = viridis::viridis(5)
names(colsClusters) = unique(temp$cluster)
grid.col = structure(rep(colsClusters,2), 
                     names = c(paste0("To_", names(colsClusters)),
                               paste0("From_",names(colsClusters))))

temp_cp2 = temp_cp2%>%select(ID, Origin, cluster)%>%
  reshape(idvar = "ID", timevar = "Origin", direction = "wide")%>%
  mutate(cluster.To = as.character(ifelse(is.na(cluster.To), 
                                          as.character(cluster.From), 
                                          as.character(cluster.To))))

socio_dem_clust2 <- merge(socio_dem, temp_cp2, by.x = "email", by.y = "ID")
socio_dem_clust2 <-  socio_dem_clust2%>%select(sexe, edad_cat, cluster.From,
                                               cluster.To)

caracterizacion_edad2 <- with(socio_dem_clust2, table(cluster.From, cluster.To, edad_cat))
caracterizacion_sexo2 <- with(socio_dem_clust2, table(cluster.From, cluster.To, sexe))

print(caracterizacion_edad2)
print(caracterizacion_sexo2)

calidad_clust2 <- merge(calidad_2any2, vas2, by="email")
calidad_clust2 <- merge(calidad_clust2, calidad_previa, by="email")
calidad_clust2 <- merge(calidad_clust2, temp_cp2, by.x = "email", by.y = "ID")
calidad_clust2 <- na.omit(calidad_clust2)

# Calcular la variación en calidad de vida
calidad_clustb <- calidad_clust2 %>%
  mutate(change_in_vas = as.numeric(factor(vas_cat_2any)) - as.numeric(factor(vas_cat)))
calidad_clustb <- calidad_clustb %>%
  mutate(change_in_eq5d = eq5d_3l_2any2 - eq5d_3l)
summary_data <- aggregate(cbind(change_in_vas, change_in_eq5d) ~ cluster.From + cluster.To, calidad_clustb, function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))

# Separar las medias y desviaciones estándar en columnas separadas
summary_data <- do.call(data.frame, summary_data)
summary_data$mean_change_vas <- summary_data$change_in_vas.mean
summary_data$sd_change_vas <- summary_data$change_in_vas.sd
summary_data$mean_change_eq5d <- summary_data$change_in_eq5d.mean
summary_data$sd_change_eq5d <- summary_data$change_in_eq5d.sd

ggplot(summary_data, aes(x = cluster.From, y = mean_change_eq5d, fill = cluster.To)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Change in EQ-5D by Cluster Trajectory.",
       x = "Baseline Cluster",
       y = "Mean Change in EQ-5D",
       fill = "Final Cluster") +
  theme_minimal()

ggplot(summary_data, aes(x = cluster.From, y = mean_change_vas, fill = cluster.To)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Change in Quality of Life (VAS) by Cluster Trajectory",
       x = "Baseline Cluster",
       y = "Average Change in VAS",
       fill = "Final Cluster") +
  theme_minimal()

temp_cp2 = temp_cp2%>%
  with(table(cluster.From, cluster.To))

colnames(temp_cp2) = paste0("To_", colnames(temp_cp2))
row.names(temp_cp2) = paste0("From_", row.names(temp_cp2))

par(mar = c(0.1, 0.1, 0.1, 0.1), cex = 2)
chordDiagram(as.data.frame(temp_cp2), 
             grid.col = grid.col,
             transparency = 0.3,  
             direction.type = c("arrows", "diffHeight"), 
             diffHeight  = -0.005,
             link.arr.type = "big.arrow", 
             link.sort = T, 
             link.largest.ontop = T,
             direction = 1,
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(temp2))))/1.75))

circos.track(track.index = 1, 
             panel.fun = function(x, y) {
               circos.text(x = CELL_META$xcenter, 
                           y = CELL_META$ylim[1], 
                           labels = strsplit(CELL_META$sector.index, "_")[[1]][2],
                           facing = "clockwise", 
                           niceFacing = TRUE, 
                           adj = c(0, 0.5), 
                           cex = 0.4)
             }, 
             bg.border = NA) 

temp_cp2 <- as.data.frame(temp_cp2)
sum_by_cluster <- aggregate(Freq ~ cluster.From, data = temp_cp2, FUN = sum)
temp_cp2 <- merge(temp_cp2, sum_by_cluster, by = "cluster.From", suffixes = c("", "_sum"))
temp_cp2$Porcentaje <- temp_cp2$Freq / temp_cp2$Freq_sum * 100
temp_cp2 <- subset(temp_cp2, select = -c(Freq_sum))

ggplot(temp_cp2, aes(x = cluster.To, y = cluster.From, 
                     fill = Porcentaje, 
                     label=paste(Freq, " (", round(Porcentaje, 2), "%)"))) +
  geom_tile(color = "white") +
  geom_text(color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue", name = "Percentage (%)") +
  labs(title = "Transition Matrix", x = "Cluster To", y = "Cluster From", fill = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Correlacion de los pacientes que han hecho todo el seguimiento.
calidad_corr_prev <- basal%>%select(email, starts_with("eq5d"))
calidad_corr_2any <- df_merged%>%select(email, ends_with(".y"))

calidad_Corr <- merge(calidad_corr_2any, calidad_corr_prev, by = "email")
calidad_Corr <- na.omit(calidad_Corr)

calidad_Corr <- calidad_Corr%>%select(email, eq5d_vas2_spa_spa.y, eq5d_vas2_spa_spa)
calidad_Corr <- calidad_Corr %>%
  dplyr::rename(vas_2any = eq5d_vas2_spa_spa.y) %>%
  dplyr::rename(vas_initial = eq5d_vas2_spa_spa)


# Calculamos la calidad de vida del segundo año de seguimiento.
socio_dem_2any <- df_combined4%>%select(codi_seguiment, sexe, Edat, starts_with("situacio_l"),
                                        treball_sanitari, comorbiditats___0,
                                        tractament___0, alguna_situacio___4,
                                        alguna_situacio___6, alguna_situacio___7,
                                        starts_with("eq5d"), edad_cat, vas_cat)

quality_2any <- socio_dem_2any%>%select(codi_seguiment, starts_with("eq5d"))
quality <- socio_dem_2any%>%select(codi_seguiment, eq5d_3l)
quality_2any <- quality_2any%>%select(-eq5d_vas2_spa_spa, -eq5d_3l)
quality_2any <- quality_2any %>% na.omit()

euroqol_2any <- data_frame(MO=quality_2any$eq5d_mb_3l_spa_spa,
                           SC=quality_2any$eq5d_sc_3l_spa_spa,
                           UA=quality_2any$eq5d_ua_3l_spa_spa,
                           PD=quality_2any$eq5d_pd_3l_spa_spa,
                           AD=quality_2any$eq5d_ad_3l_spa_spa)

quality_2any$eq5d_3l_2any <- eq5d(scores=euroqol_2any, country="Spain",
                             version="3L", type="VAS")

quality_2any <- quality_2any%>%select(codi_seguiment, eq5d_3l_2any)

calidad_Corr <- merge(calidad_Corr, quality, by.x = "email", by.y = "codi_seguiment")
calidad_Corr <- merge(calidad_Corr, quality_2any, by.x = "email", by.y = "codi_seguiment")
calidad_Corr <- calidad_Corr %>%
  dplyr::rename(eq5d_initial = eq5d_3l) %>%
  dplyr::rename(eq5d_2any = eq5d_3l_2any)

calidad_Corr <- calidad_Corr %>%
  mutate(
    vas_scaled = scale(vas_2any),
    eq5d_3l_scaled = scale(eq5d_2any)
  )

# Calcular la correlación de Pearson
corr <- cor(calidad_Corr$vas_2any, 
            calidad_Corr$eq5d_2any, method = "pearson")
print(paste("Correlación de Pearson:", corr))

# Realizar una regresión lineal
model_corr2 <- lm(eq5d_3l_scaled ~ vas_scaled, data = calidad_Corr)
model_summary <- summary(model_corr2)

# Extraer los coeficientes y p-valores del modelo
coefficients <- model_summary$coefficients
print(coefficients)

# Extraer el R² del modelo
r_squared <- model_summary$r.squared
print(paste("R² del modelo:", r_squared))

# Visualización de la relación con un gráfico de dispersión y la línea de regresión
ggplot(calidad_Corr, aes(x = vas_scaled, y = eq5d_3l_scaled)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression between VAS and EQ5D (normalized)",
       x = "VAS (normalized)",
       y = "EQ5D (normalized)") +
  theme_minimal()

# Visualización de los residuos del modelo
ggplot(model_corr2, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Verificación de la normalidad de los residuos
ggplot(model_corr2, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(col = "blue") +
  labs(title = "QQ-Plot of the Residuals")

# Correlacion con valores imputados.
calidad_correlacion <- merge(calidad_correlacion, calidad_2any, by ="email")

calidad_correlacion <- calidad_correlacion %>%
  dplyr::rename(vas_2any = eq5d_vas2_spa_spa.y)

calidad_correlacion <- calidad_correlacion %>%
  filter(!is.na(vas_2any) & !is.na(eq5d_3l_2any))

calidad_correlacion <- calidad_correlacion %>%
  mutate(
    vas_scaled = scale(vas_2any),
    eq5d_3l_scaled = scale(eq5d_3l_2any)
  )

# Calcular la correlación de Pearson
correlation <- cor(calidad_correlacion$vas_scaled, 
                   calidad_correlacion$eq5d_3l_scaled, method = "pearson")
print(paste("Correlación de Pearson:", correlation))

# Realizar una regresión lineal
model_corr <- lm(eq5d_3l_scaled ~ vas_scaled, data = calidad_correlacion)
model_summary2 <- summary(model_corr)

# Extraer los coeficientes y p-valores del modelo
coefficients <- model_summary2$coefficients
print(coefficients)

# Extraer el R² del modelo
r_squared <- model_summary2$r.squared
print(paste("R² del modelo:", r_squared))

# Visualización de la relación con un gráfico de dispersión y la línea de regresión
ggplot(calidad_correlacion, aes(x = vas_scaled, y = eq5d_3l_scaled)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression between VAS and EQ5D (normalized)",
       x = "VAS (normalized)",
       y = "EQ5D (normalized)") +
  theme_minimal()

# Visualización de los residuos del modelo
ggplot(model_corr, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Verificación de la normalidad de los residuos
ggplot(model_corr, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(col = "blue") +
  labs(title = "QQ-Plot of the Residuals")
