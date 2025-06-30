rm(list = ls())
setwd("C:/Users/vazqu/OneDrive/Documents/ACTUARIA")
em <- read.csv("CE_FINAL_FINAL.CSV")

# Se omiten valores de prima emitidas negativos y se calcula el ratio 
install.packages("dplyr")
library(dplyr)
em$PRIMA_DEVENGADA <- as.numeric(em$PRIMA_DEVENGADA)
em <- em %>% filter(PRIMA_EMITIDA >0, !is.na(PRIMA_DEVENGADA)) 
em <- em %>% mutate(ratio = PRIMA_DEVENGADA/PRIMA_EMITIDA)

# Instalación de mas paqueterias
pkgs <- c("factoextra", "NbClust", "cluster", "fpc", "dendextend")
install.packages(pkgs)

library(factoextra); library(NbClust); library(fpc); library(cluster)

## Arreglo sobre la variable País
install.packages("stringi")
library(stringi)
em$PAIS <- stri_trans_general(em$PAIS, "Latin-ASCII")
em$PAIS <- as.factor(em$PAIS)
levels(em$PAIS)
em <- em %>% mutate(PAIS = case_when(
  PAIS == "Corea Del Sur" ~ "Corea del Sur",
  PAIS == "Emiratos Arabes Unidos" ~ "Emiratos Arabes Unido",
  PAIS == "E.U.A." ~ "EUA",
  PAIS == "Estados Unidos de America" ~ "EUA", 
  PAIS == "Pais No Especificado" ~ "Pais no Especificado",
  TRUE ~ PAIS
))
em$PAIS <- as.factor(em$PAIS)
levels(em$PAIS)
# Extracción de variables para cluster (País y Suma Asegurada)
str(em)
emnum <- em %>% select(PAIS, SUMA_ASEGURADA)

# Estandarización de Suma Asegurada
emnum$SUMA_ASEGURADA <- as.numeric(scale(emnum$SUMA_ASEGURADA))

#### Aplicación de Prueba del Codo
options(scipen = 999)

install.packages("clustMixType")
library(clustMixType)

costos <- numeric()
set.seed(1234)
for (k in 1:10) {
  modelo <- kproto(emnum[, c("PAIS", "SUMA_ASEGURADA")], k)
  costos[k] <- modelo$tot.withinss
}

elbow_data <- data.frame(k = 1:10, costo = costos)
# Grafica de prueba del codo
ggplot(elbow_data, aes(x = k, y = costo)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Prueba del Codo para K-Prototypes",
       x = "Número de Clusters (k)",
       y = "Costo Total (Suma de Distancias Intra-cluster)") +
  scale_x_continuous(breaks = seq(min(elbow_data$k), max(elbow_data$k), by = 1)) +
  theme_minimal()

# El codo nos indica que tal vez el #Clusters más apropiados es 3. 

### Aplicación por método de la silueta
install.packages("cluster")
library(cluster)
diss <- daisy(emnum[, c("PAIS", "SUMA_ASEGURADA")], metric = "gower")
promedios_silueta <- numeric()
set.seed(1234)
for (i in 2:6) {
  modelo1 <- kproto(emnum[, c("PAIS", "SUMA_ASEGURADA")], i)
  clusters <- modelo1$cluster
  sil <- silhouette(clusters, diss)
  promedios_silueta[i] <- mean(sil[, 3])
}

silueta_df <- data.frame(
  k = 2:6,
  promedio = promedios_silueta[2:6]
)

ggplot(silueta_df, aes(x = k, y = promedio)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Promedio de Índice de Silueta para K-Prototypes",
       x = "Número de Clusters (k)",
       y = "Índice de Silueta Promedio") +
  theme_minimal()

# A pesar de ser debil el promedio del Indice de Silueta, coincide con la Prueba del Codo

set.seed(123)
resultado <- kproto(emnum[, c("PAIS", "SUMA_ASEGURADA")], k = 3)
emnum$cluster <- resultado$cluster
em$cluster <- resultado$cluster
ggplot(emnum, aes(x = SUMA_ASEGURADA, y = PAIS, color = factor(cluster))) +
  geom_jitter(width = 0.5, alpha = 0.7) +
  labs(title = "Clustering por K-Prototypes",
       color = "Cluster") +
  theme_minimal()

# Cantidad de datos por cluster
table(as.factor(emnum$cluster))


ggplot(em, aes(x = factor(cluster), y = SUMA_ASEGURADA)) +
  geom_boxplot()

table(factor(em$cluster), factor(em$TIPO_CREDITO))
##### Primer Cluster. 

Cluster1 <- em %>% filter(cluster == 1) %>% select(PLAZO, NUMERO_POLIZAS, PRIMA_EMITIDA,
                                                   PRIMA_RETENIDA, PRIMA_DEVENGADA, SUMA_ASEGURADA,
                                                   ratio)

Cluster1[, 1:6] <- scale(Cluster1[, 1:6])
quantile(Cluster1$ratio, 0.99)
Cluster1 <- filter(Cluster1, ratio<1.988305)

### ARBOL CUANTILICO: 
install.packages("grf")
library(grf)
Y <- Cluster1$ratio
X <- as.matrix(Cluster1[, c("PRIMA_RETENIDA","SUMA_ASEGURADA", "PLAZO")])
set.seed(1234)
q.forest <- quantile_forest(
  X, Y,
  quantiles = c(0.25, 0.5, 0.75),
  num.trees = 500
)

qhat25 <- predict(q.forest, X, quantiles = c(0.25))
qhat50 <- predict(q.forest, X, quantiles = c(0.50))
qhat75 <- predict(q.forest, X, quantiles = c(0.75))

pred_25 <- qhat25$predictions
pred_50 <- qhat50$predictions
pred_75 <- qhat75$predictions

# Crear un data.frame con los resultados
results <- data.frame(
  Actual = Y,
  Pred_25 = pred_25, 
  Pred_50 = pred_50, 
  Pred_75 = pred_75
)


quantile(Cluster1, 0.9)

ggplot(Cluster1, aes(x = SUMA_ASEGURADA, y = ratio)) +
  geom_point() +
  geom_smooth(method = "rq", method.args = list(tau = 0.25), se = FALSE, color = "blue")+
  geom_smooth(method = "rq", method.args = list(tau = 0.5), se = FALSE, color = "red")+
  geom_smooth(method = "rq", method.args = list(tau = 0.75), se = FALSE, color = "purple")


#### Segundo Cluster
Cluster2 <- em %>% filter(cluster == 2) %>% select(PLAZO, NUMERO_POLIZAS, PRIMA_EMITIDA,
                                                   PRIMA_RETENIDA, PRIMA_DEVENGADA, SUMA_ASEGURADA,
                                                   ratio) 


Cluster2[, 1:6] <- scale(Cluster2[, 1:6])
# Se van a eliminar los ratios mayores a 0.99
quantile(Cluster2$ratio, 0.99)
Cluster2 <- filter(Cluster2, ratio<8.040567)


### ARBOL CUANTILICO: 
install.packages("grf")
library(grf)
Y2 <- Cluster2$ratio
X2 <- as.matrix(Cluster2[, c("PRIMA_RETENIDA","SUMA_ASEGURADA", "PLAZO")])
set.seed(1234)
q2.forest <- quantile_forest(
  X2, Y2,
  quantiles = c(0.25, 0.5, 0.75),
  num.trees = 500
)

C2qhat25 <- predict(q2.forest, X2, quantiles = c(0.25))
C2qhat50 <- predict(q2.forest, X2, quantiles = c(0.50))
C2qhat75 <- predict(q2.forest, X2, quantiles = c(0.75))

C2pred_25 <- C2qhat25$predictions
C2pred_50 <- C2qhat50$predictions
C2pred_75 <- C2qhat75$predictions

# Crear un data.frame con los resultados
results2 <- data.frame(
  Actual = Y2,
  Pred_25 = C2pred_25, 
  Pred_50 = C2pred_50, 
  Pred_75 = C2pred_75
)

mean(abs(results2$Actual - results2$Pred_25)) 
mean(abs(results2$Actual - results2$Pred_50)) 
mean(abs(results2$Actual - results2$Pred_75)) 

ggplot(Cluster2, aes(x = SUMA_ASEGURADA, y = ratio)) +
  geom_point() +
  geom_smooth(method = "rq", method.args = list(tau = 0.25), se = FALSE, color = "blue")+
  geom_smooth(method = "rq", method.args = list(tau = 0.5), se = FALSE, color = "red")+
  geom_smooth(method = "rq", method.args = list(tau = 0.75), se = FALSE, color = "purple")
 

#### Tercer Cluster
Cluster3 <- em %>% filter(cluster == 3) %>% select(PLAZO, NUMERO_POLIZAS, PRIMA_EMITIDA,
                                                   PRIMA_RETENIDA, PRIMA_DEVENGADA, SUMA_ASEGURADA,
                                                   ratio)

Cluster3[, 1:6] <- scale(Cluster3[, 1:6])
# Se van a eliminar los ratios mayores a 0.99
quantile(Cluster3$ratio, 0.99)
Cluster3 <- filter(Cluster3, ratio<4.097605)


### ARBOL CUANTILICO: 
install.packages("grf")
library(grf)
Y3 <- Cluster3$ratio
X3 <- as.matrix(Cluster3[, c("PRIMA_RETENIDA","SUMA_ASEGURADA", "PLAZO")])
set.seed(1234)
q3.forest <- quantile_forest(
  X3, Y3,
  quantiles = c(0.25, 0.5, 0.75),
  num.trees = 500
)

C3qhat25 <- predict(q3.forest, X3, quantiles = c(0.25))
C3qhat50 <- predict(q3.forest, X3, quantiles = c(0.50))
C3qhat75 <- predict(q3.forest, X3, quantiles = c(0.75))

C3pred_25 <- C3qhat25$predictions
C3pred_50 <- C3qhat50$predictions
C3pred_75 <- C3qhat75$predictions

# Crear un data.frame con los resultados
results3 <- data.frame(
  Actual = Y3,
  Pred_25 = C3pred_25, 
  Pred_50 = C3pred_50, 
  Pred_75 = C3pred_75
)

mean(abs(results3$Actual - results3$Pred_25)) 
mean(abs(results3$Actual - results3$Pred_50)) 
mean(abs(results3$Actual - results3$Pred_75)) 

ggplot(Cluster3, aes(x = SUMA_ASEGURADA, y = ratio)) +
  geom_point() +
  geom_smooth(method = "rq", method.args = list(tau = 0.25), se = FALSE, color = "blue")+
  geom_smooth(method = "rq", method.args = list(tau = 0.5), se = FALSE, color = "red")+
  geom_smooth(method = "rq", method.args = list(tau = 0.75), se = FALSE, color = "purple")

