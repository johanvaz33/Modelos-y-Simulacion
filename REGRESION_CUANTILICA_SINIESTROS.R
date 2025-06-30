#Regresión intercuantil para Siniestros
rm(list= ls())

# Base de datos
setwd("C:/Users/rgard/OneDrive/Escritorio/Facultad/10° Semestre/Modelos y Simulacion/Proyectos/Parte final del proyecto/Final")
SF <- read.csv("SiniestrosF.csv")

# Paqueterías
install.packages("pacman")
require(pacman)
pacman::p_load(dplyr, ggplot2, tidyverse, GGally, car, quantreg, factoextra, clustMixType, cluster)

SF <- SF %>% 
  mutate(ID = row_number())


#Convertimos nuestras variables catgóricas a factor
SF$ENTIDAD <- as.factor(SF$ENTIDAD)
SF$PAIS <- as.factor(SF$PAIS)
SF$TIPO.DE.CREDITO <- as.factor(SF$TIPO.DE.CREDITO)
SF$TIPO.DE.INSOLVENCIA <- as.factor(SF$TIPO.DE.INSOLVENCIA)
SF$MONEDA <- as.factor(SF$MONEDA)
SF$FORMA.DE.VENTA <- as.factor(SF$FORMA.DE.VENTA)
SF$TIPO.DE.POLIZA <- as.factor(SF$TIPO.DE.POLIZA)
SF$GIRO2 <- as.factor(SF$GIRO2)
SF$PAIS <- as.factor(SF$PAIS)


#Arreglo de ciertas escrituras
SF <- SF %>%
  mutate(PAIS = case_when(
    PAIS == "E.U.A." ~ "EUA",
    PAIS == "Estados Unidos de America" ~ "EUA",
    PAIS == "Pais No Especificado" ~ "Pais no Especificado",
    PAIS == "" ~ "",
    PAIS == "" ~ "",
    TRUE ~ PAIS  # conserva los demás valores
  ))

SF <- SF %>%
  mutate(TIPO.DE.CREDITO = str_trim(TIPO.DE.CREDITO),  # quita espacios en blanco
         TIPO.DE.CREDITO = str_to_lower(TIPO.DE.CREDITO),  # todo en minúsculas
         TIPO.DE.CREDITO = case_when(
           TIPO.DE.CREDITO %in% c("exportacion", "exportación") ~ "Exportacion",
           TIPO.DE.CREDITO == "interno" ~ "Interno",
           TIPO.DE.CREDITO == "mixto" ~ "Mixto",
           str_detect(TIPO.DE.CREDITO, "no disponible") ~ "No disponible",
           TRUE ~ str_to_title(TIPO.DE.CREDITO)  # pone primera letra en mayúscula para otros
         ))
# ------------------- Clusters de la base de datos (monto pagado y país)
# Método del codo
options(scipens = 999)
SF <- SF[SF$MONTO.DEL.SINIESTRO != 0, ]
clusterdb <- SF %>%
  select(ID, MONTO.PAGADO, PAIS) %>%
  mutate(across(where(is.character), as.factor))

clusterdb$PAIS <- as.factor(clusterdb$PAIS)
cost <- numeric()
set.seed(123)

for (k in 1:10) {
  modelo <- kproto(clusterdb[, -1], k = k)
  cost[k] <- modelo$tot.withinss  # Función objetivo: disimilitud total
}

plot(1:10, cost, type = "b", pch = 19,
     xlab = "Número de clusters k",
     ylab = "Disimilitud intra-cluster total",
     main = "Método del Codo para k-prototypes")


# Aplicando el K prototype a la base de datos
numclus <- kproto(clusterdb[, -1], k = 3)
#clprofiles(numclus, clusterdb, vars = c("MONTO.PAGADO", "PAIS"))
#numclus %>% summary()

resultados <- clusterdb %>%
  select(ID) %>%
  mutate(cluster = numclus$cluster)
SF <- SF %>% left_join(resultados, by = "ID")

SF$ratio <- SF$MONTO.DEL.SINIESTRO / SF$MONTO.PAGADO

table(SF$cluster)
table(SF$PAIS)

SF_cluster1 <- SF %>% filter(cluster == 1)
SF_cluster2 <- SF %>% filter(cluster == 2)
SF_cluster3 <- SF %>% filter(cluster == 3)


#Estandarizacion de variables (para evitar el sesgo entre las mismas) de cada una de las bases
SF_cluster1[c("MONTO.DEL.SINIESTRO", "MONTO.PAGADO", "GASTOS.DE.AJUSTE","SALVAMENTOS", "MONTO.COASEGUROS", "PLAZO..dias.","NUMERO.DE.SINIESTROS","MONTO.DE.DEDUCIBLE")] <- scale(SF_cluster1[c("MONTO.DEL.SINIESTRO", "MONTO.PAGADO", "GASTOS.DE.AJUSTE","SALVAMENTOS", "MONTO.COASEGUROS", "PLAZO..dias.","NUMERO.DE.SINIESTROS","MONTO.DE.DEDUCIBLE")])
SF_cluster2[c("MONTO.DEL.SINIESTRO", "MONTO.PAGADO", "GASTOS.DE.AJUSTE","SALVAMENTOS", "MONTO.COASEGUROS", "PLAZO..dias.","NUMERO.DE.SINIESTROS","MONTO.DE.DEDUCIBLE")] <- scale(SF_cluster2[c("MONTO.DEL.SINIESTRO", "MONTO.PAGADO", "GASTOS.DE.AJUSTE","SALVAMENTOS", "MONTO.COASEGUROS", "PLAZO..dias.","NUMERO.DE.SINIESTROS","MONTO.DE.DEDUCIBLE")])
SF_cluster2[c("MONTO.DEL.SINIESTRO", "MONTO.PAGADO", "GASTOS.DE.AJUSTE","SALVAMENTOS", "MONTO.COASEGUROS", "PLAZO..dias.","NUMERO.DE.SINIESTROS","MONTO.DE.DEDUCIBLE")] <- scale(SF_cluster2[c("MONTO.DEL.SINIESTRO", "MONTO.PAGADO", "GASTOS.DE.AJUSTE","SALVAMENTOS", "MONTO.COASEGUROS", "PLAZO..dias.","NUMERO.DE.SINIESTROS","MONTO.DE.DEDUCIBLE")])


# Creación de los Ratios
SF_cluster1$ratio <- SF_cluster1$MONTO.DEL.SINIESTRO / SF_cluster1$MONTO.PAGADO
SF_cluster2$ratio <- SF_cluster2$MONTO.DEL.SINIESTRO / SF_cluster2$MONTO.PAGADO
SF_cluster3$ratio <- SF_cluster3$MONTO.DEL.SINIESTRO / SF_cluster3$MONTO.PAGADO


# ------------------------------------------- Regresión para el primer cluster

reg_025_1 <- rq(ratio ~ MONTO.PAGADO + MONTO.DEL.SINIESTRO + MONTO.COASEGUROS + PLAZO..dias. , data = SF_cluster1, tau = 0.25)
reg_050_1 <- rq(ratio ~ MONTO.PAGADO + MONTO.DEL.SINIESTRO + MONTO.COASEGUROS + PLAZO..dias. , data = SF_cluster1, tau = 0.5)
reg_075_1 <- rq(ratio ~ MONTO.PAGADO + MONTO.DEL.SINIESTRO + MONTO.COASEGUROS + PLAZO..dias. , data = SF_cluster1, tau = 0.75)

# Verificando el nivel estadístico de información (p value)
#summary(reg_025, se = "boot")
#summary(reg_050, se = "boot")
#summary(reg_075, se = "boot")


# Regresiones de los 3 cuantiles
pred_tau025_1 <- predict(reg_025_1, newdata = SF_cluster1)
pred_tau050_1 <- predict(reg_050_1, newdata = SF_cluster1)
pred_tau075_1 <- predict(reg_075_1, newdata = SF_cluster1)

# Aquí se unen las predicciones para que todo se vea en una tabla
predicciones1 <- data.frame(
  id = 1:nrow(SF_cluster1),
  tau_0.25_1 = pred_tau025_1,
  tau_0.50_1 = pred_tau050_1,
  tau_0.75_1 = pred_tau075_1,
  real_ratio1 = SF_cluster1$ratio
)

#rm(reg_025_1, reg_050_1, reg_075_1, pred_tau025_1,pred_tau050_1, pred_tau075_1, predicciones1)

ggplot(SF_cluster1, aes(x = MONTO.PAGADO, y = ratio)) +
  geom_point() +
  geom_smooth(method = "rq", method.args = list(tau = 0.10), se = FALSE, color = "blue")+
  geom_smooth(method = "rq", method.args = list(tau = 0.5), se = FALSE, color = "red")+
  geom_smooth(method = "rq", method.args = list(tau = 0.9), se = FALSE, color = "purple")

predicciones1$e1 <- mean(abs((predicciones1$real_ratio1 - predicciones1$tau_0.25_1)))
predicciones1$e2 <- mean(abs((predicciones1$real_ratio1 - predicciones1$tau_0.50_1)))
predicciones1$e3 <- mean(abs((predicciones1$real_ratio1 - predicciones1$tau_0.75_1)))

# ------------------------------------------- Regresión para el segundo cluster

reg_025_2 <- rq(ratio ~ MONTO.PAGADO + MONTO.DEL.SINIESTRO + MONTO.COASEGUROS + PLAZO..dias. + PAIS , data = SF_cluster2, tau = 0.25)
reg_050_2 <- rq(ratio ~ MONTO.PAGADO + MONTO.DEL.SINIESTRO + MONTO.COASEGUROS + PLAZO..dias. + PAIS , data = SF_cluster2, tau = 0.50)
reg_075_2 <- rq(ratio ~ MONTO.PAGADO + MONTO.DEL.SINIESTRO + MONTO.COASEGUROS + PLAZO..dias. + PAIS , data = SF_cluster2, tau = 0.75)




# Regresiones de los 3 cuantiles
pred_tau025_2 <- predict(reg_025_2, newdata = SF_cluster2)
pred_tau050_2 <- predict(reg_050_2, newdata = SF_cluster2)
pred_tau075_2 <- predict(reg_075_2, newdata = SF_cluster2)

# Aquí se unen las predicciones para que todo se vea en una tabla
predicciones2 <- data.frame(
  id = 1:nrow(SF_cluster2),
  tau_0.25_2 = pred_tau025_2,
  tau_0.50_2 = pred_tau050_2,
  tau_0.75_2 = pred_tau075_2,
  real_ratio2 = SF_cluster2$ratio
)

quantile(SF_cluster2$ratio, 0.99)
sum(SF_cluster2$ratio > 13.1896 )
SF_cluster2 <- filter(SF_cluster2, ratio<13.1896)
#sum(predicciones2$tau_0.75_2 > predicciones2$real_ratio2)
#rm(reg_025_2, reg_050_2, reg_075_2, pred_tau025_2,pred_tau050_2, pred_tau075_2, predicciones2)

ggplot(SF_cluster2, aes(x = MONTO.PAGADO, y = ratio)) +
  geom_point() +
  geom_smooth(method = "rq", method.args = list(tau = 0.25), se = FALSE, color = "blue")+
  geom_smooth(method = "rq", method.args = list(tau = 0.50), se = FALSE, color = "red")+
  geom_smooth(method = "rq", method.args = list(tau = 0.95), se = FALSE, color = "purple")

predicciones2$e1 <- mean(abs((predicciones2$real_ratio2 - predicciones2$tau_0.25_2)))
predicciones2$e2 <- mean(abs((predicciones2$real_ratio2 - predicciones2$tau_0.50_2)))
predicciones2$e3 <- mean(abs((predicciones2$real_ratio2 - predicciones2$tau_0.75_2)))



# ------------------------------------------- Regresión para el tercer cluster

reg_025_3 <- rq(ratio ~ MONTO.PAGADO + MONTO.DEL.SINIESTRO + MONTO.COASEGUROS + PLAZO..dias. + PAIS , data = SF_cluster3, tau = 0.25)
reg_050_3 <- rq(ratio ~ MONTO.PAGADO + MONTO.DEL.SINIESTRO + MONTO.COASEGUROS + PLAZO..dias. + PAIS, data = SF_cluster3, tau = 0.5)
reg_075_3 <- rq(ratio ~ MONTO.PAGADO + MONTO.DEL.SINIESTRO + MONTO.COASEGUROS + PLAZO..dias. + PAIS, data = SF_cluster3, tau = 0.75)


# Regresiones de los 3 cuantiles
pred_tau025_3 <- predict(reg_025_3, newdata = SF_cluster3)
pred_tau050_3 <- predict(reg_050_3, newdata = SF_cluster3)
pred_tau075_3 <- predict(reg_075_3, newdata = SF_cluster3)

# Aquí se unen las predicciones para que todo se vea en una tabla
predicciones3 <- data.frame(
  id = 1:nrow(SF_cluster3),
  tau_0.25_3 = pred_tau025_3,
  tau_0.50_3 = pred_tau050_3,
  tau_0.75_3 = pred_tau075_3,
  real_ratio3 = SF_cluster3$ratio
)

quantile(SF_cluster3$ratio, 0.75)
mean(predicciones3$tau_0.75_3)

ggplot(SF_cluster3, aes(x = MONTO.PAGADO, y = ratio)) +
  geom_point() +
  geom_smooth(method = "rq", method.args = list(tau = 0.25), se = FALSE, color = "blue")+
  geom_smooth(method = "rq", method.args = list(tau = 0.80), se = FALSE, color = "red")+
  geom_smooth(method = "rq", method.args = list(tau = 0.97), se = FALSE, color = "purple")

quantile(SF_cluster3$ratio, 0.975)
sum(SF_cluster3$ratio > 1.125376)

predicciones3$e1 <- mean(abs((predicciones3$real_ratio3 - predicciones3$tau_0.25_3)))
predicciones3$e2 <- mean(abs((predicciones3$real_ratio3 - predicciones3$tau_0.50_3)))
predicciones3$e3 <- mean(abs((predicciones3$real_ratio3 - predicciones3$tau_0.75_3)))
