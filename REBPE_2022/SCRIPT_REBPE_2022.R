#=========================================================================================================#
# Script para análisis de datos del Registro Estadístico Base de Población del Ecuador                    #
# Autor: Alan Del Rosario                                                                                 #
# Fecha: 18/05/2025                                                                                       #
# centrándose en el cantón Ambato (Código 1801).                                                          #
#=========================================================================================================#

# Librerías
library(tidyverse) # Para manipulación de datos
library(lubridate) # Para trabajar con fechas
library(ggplot2) # Para visualización de datos
library(forcats)
library(emmeans) # Para comparaciones post-hoc

# Cargar datos del cantón Ambato (Código 1801) desde archivo CSV.
datos <- read_csv("C:/Users/USER/Documents/REBPE_2022/Data/Bdd_Datos_Abiertos_REBPE_2022_/rebpe_2022.csv") %>% 
  filter(cant_res == 1801)

# Revisar estructura general
glimpse(datos)

# Transformación de variables categóricas a factor con etiquetas descriptivas----------------------------------------
datos2 <- datos %>%
  mutate(
    sexo = factor(sexo, levels = c(1, 2, 3), labels = c("Hombre", "Mujer", "Indeterminado")),
    grupo_edad = factor(grupo_edad, levels = 1:18, labels = c(
      "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
      "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
      "70-74", "75-79", "80-84", "85+"
    )),
    etaedad = factor(etaedad, levels = 1:5, labels = c(
      "Niñez (0-11)", "Adolescencia (12-17)", "Juventud (18-29)",
      "Adultez (30-64)", "Adulto Mayor (65+)"
    )),
    e_civil = factor(e_civil, levels = c(1, 2, 3, 4, 5, 9), labels = c(
      "Soltero/a", "Casado/a", "Divorciado/a", "Viudo/a", "Unión de hecho", "No registra"
    )),
    es_madre = factor(es_madre, levels = c(1, 2), labels = c("Sí", "No")),
    es_padre = factor(es_padre, levels = c(1, 2), labels = c("Sí", "No")),
    padres_fall = factor(padres_fall, levels = c(1, 2, 3, 4), labels = c(
      "Ninguno", "Huérfano de padre", "Huérfano de madre", "Huérfano de ambos"
    )),
    tiene_disc = factor(tiene_disc, levels = c(1, 2), labels = c("Sí", "No")),
    tipo_disc = factor(tipo_disc, levels = c(1, 2, 3, 4, 5, 6, 9), labels = c(
      "Intelectual", "Física", "Visual", "Auditiva", "Psicosocial", "Lenguaje", "No registra"
    )),
    pensionistas = factor(pensionistas, levels = c(1, 2), labels = c("Sí", "No"))
  )

# Revisar estructura de transformaciones
glimpse(datos2)

# Revisar datos faltantes dentro del conjunto "datos2"
missing_values <- datos2 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_count") %>%
  filter(missing_count > 0)

colSums(is.na(datos2)) # Comprobacion rapida. 

# Tratamiento de casos NA -------------------------------------------------
# Total de observaciones
nrow(datos2) # 386275

# Aplicar tratamiento a NA según naturaleza de cada variable
datos3 <- datos2 %>%
  mutate(
    es_madre = fct_explicit_na(es_madre, na_level = "No Aplica"),
    es_padre = fct_explicit_na(es_padre, na_level = "No Aplica"),
    padres_fall = fct_explicit_na(padres_fall, na_level = "No Aplica"),
    tipo_disc = fct_explicit_na(tipo_disc, na_level = "No Aplica"),
    pensionistas = fct_explicit_na(pensionistas, na_level = "No Aplica"),
    anios_viud = ifelse(is.na(anios_viud), 0, anios_viud),
    edad_viud = ifelse(is.na(edad_viud), 0, edad_viud)
  )

# Verificación rápida de que ya no quedan NA
colSums(is.na(datos3))

# Tablas cruzadas -----------------------------------------------------------------------------------------------------------------------------------------------------
## 1.	Entre las personas con discapacidad del cantón, ¿qué tipo de discapacidad predomina según el sexo?---------------------------------------------------------------
# Analiza: Considerando los resultados obtenidos, ¿qué implicaciones podría tener la predominancia de cierto tipo de discapacidad en un sexo específico para la política pública local de tu cantón?
# Filtrar solo personas con discapacidad registrada
datos_discapacitados <- datos2 %>% filter(tiene_disc == "Sí")

# Tabla cruzada entre sexo y tipo de discapacidad
tabla_sexo_disc <- datos_discapacitados %>%
  group_by(sexo, tipo_disc) %>%
  summarise(total = n(), .groups = "drop") %>%
  pivot_wider(names_from = tipo_disc, values_from = total, values_fill = 0) %>%
  mutate(total = rowSums(across(-sexo))) %>%
  arrange(desc(total))

# Visualización
tabla_sexo_disc %>%
  pivot_longer(cols = -sexo, names_to = "tipo_disc", values_to = "total") %>%
  ggplot(aes(x = sexo, y = total, fill = tipo_disc)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribución de personas con discapacidad por sexo y tipo de discapacidad",
    x = "Sexo",
    y = "Total",
    fill = "Tipo de Discapacidad"
  ) +
  theme_minimal()

## 2.	¿Qué grupo de edad y qué sexo se asocian con una mayor proporción de pensionistas en tu cantón?------------------------------------------------------------------
# Analiza: ¿Qué hipótesis podrías plantear sobre la inserción laboral y los ciclos de vida económica en tu cantón a partir de la distribución de los pensionistas por grupo de edad y sexo?
# Tablas cruzadas entre grupo de edad y sexo
# Filtrar solo registros con información sobre pensionistas
datos_pensionistas <- datos3 %>% filter(!is.na(pensionistas))

# Tabla cruzada entre grupo de edad y sexo
tabla_pensionistas <- datos_pensionistas %>%
  group_by(grupo_edad, sexo, pensionistas) %>%
  summarise(total = n(), .groups = "drop") %>%
  filter(pensionistas == "Sí")

# Visualización de la tabla cruzada para el ejercicio 2
ggplot(tabla_pensionistas, aes(x = grupo_edad, y = total, fill = sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribución de pensionistas por grupo de edad y sexo",
    x = "Grupo de Edad",
    y = "Número de Pensionistas",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 3.	Entre las personas viudas del cantón, ¿hay diferencias en la edad promedio de viudez entre hombres y mujeres, según grupo de edad?-------------------------------
# Analiza: ¿Qué factores sociales o económicos podrían explicar las diferencias en la edad de viudez entre hombres y mujeres en tu cantón?
# Ya existe una variable que indica la edad de viudez, por lo que no es necesario calcularla. Que es "edad_viud", con esta variable se puede hacer un análisis de la edad promedio de viudez entre hombres y mujeres.
# Filtrar registros con edad de viudez registrada
datos_viudos <- datos3 %>% filter(!is.na(edad_viud))

# Calcular edad promedio de viudez por sexo y grupo de edad
tabla_viudez <- datos_viudos %>%
  group_by(grupo_edad, sexo) %>%
  summarise(prom_edad_viud = mean(edad_viud, na.rm = TRUE), .groups = "drop")

# Visualización de la tabla cruzada para el ejercicio 3
ggplot(tabla_viudez, aes(x = grupo_edad, y = prom_edad_viud, color = sexo, group = sexo)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Edad promedio de viudez por sexo y grupo de edad",
    x = "Grupo de Edad",
    y = "Edad Promedio al Enviudar",
    color = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 4.	En tu cantón, ¿cómo varía el estado civil entre los distintos grupos de edad y sexo? Presenta los datos y analiza.------------------------------------------------
# Analiza: ¿Qué dinámicas poblacionales o tendencias culturales podrían estar incidiendo en la distribución del estado civil según edad y sexo en tu cantón?
# Tabla cruzada entre grupo de edad, sexo y estado civil
tabla_estado_civil <- datos3 %>%
  group_by(grupo_edad, sexo, e_civil) %>%
  summarise(total = n(), .groups = "drop")

# Visualización de la tabla cruzada para el ejercicio 4
ggplot(tabla_estado_civil, aes(x = grupo_edad, y = total, fill = e_civil)) +
  geom_bar(stat = "identity", position = "fill") +  # Proporciones
  facet_wrap(~ sexo) +
  labs(
    title = "Distribución del estado civil por grupo de edad y sexo",
    x = "Grupo de Edad",
    y = "Proporción",
    fill = "Estado Civil"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# ANOVA ---------------------------------------------------------------------------------------------------------------------------------------------------------------
## 1.	¿Existen diferencias en la edad promedio de las personas según su grupo de edad declarado en tu cantón?----------------------------------------------------------
# Analiza: ¿En qué medida los grupos etarios reflejan diferencias reales de edad promedio, y qué implicaciones tendría esto para clasificaciones estandarizadas utilizadas en análisis sociales?
# Verificar supuestos básicos: varianzas y normalidad visual
boxplot(edad ~ grupo_edad, data = datos3, 
        main = "Distribución de la Edad según Grupo Etario Declarado",
        xlab = "Grupo Etario",
        ylab = "Edad")

# ANOVA
anova_model1 <- aov(edad ~ grupo_edad, data = datos3)
summary(anova_model1)

# Comparaciones post-hoc si es significativo
if(summary(anova_model1)[[1]]$`Pr(>F)`[1] < 0.05){
  library(emmeans)
  posthoc1 <- emmeans(anova_model1, pairwise ~ grupo_edad)
  print(posthoc1)
}

## 2.	Entre las personas viudas, ¿la edad promedio de viudez varía según el grupo de edad actual de la persona? (edad_viud, grupo_edad, filtrar solo quienes son viudos)-----
# Analiza: ¿Qué hipótesis podrías plantear sobre los patrones demográficos y la diferencia en longevidad entre parejas?
# Filtrar solo personas viudas
datos_viudos <- datos3 %>% filter(anios_viud > 0)

# Verificar distribución
boxplot(edad_viud ~ grupo_edad, data = datos_viudos,
        main = "Edad de Viudez según Grupo Etario Actual",
        xlab = "Grupo Etario Actual",
        ylab = "Edad al Enviudar")

# ANOVA
anova_model2 <- aov(edad_viud ~ grupo_edad, data = datos_viudos)
summary(anova_model2)

# Comparaciones post-hoc si es significativo
if(summary(anova_model2)[[1]]$`Pr(>F)`[1] < 0.05){
  posthoc2 <- emmeans(anova_model2, pairwise ~ grupo_edad)
  print(posthoc2)
}


# TUKEY ----------------------------------------------------------------------------------------------------------------------------------------------------
## 1.	¿Entre qué grupos de edad se presentan las diferencias más marcadas en la edad promedio?--------------------------------------------------------------
# Analiza: ¿Estas diferencias confirman la coherencia del sistema de clasificación por grupo etario o evidencian posibles solapamientos o inconsistencias?



  
## 2.	¿Qué diferencias significativas se observan entre los grupos de edad actual en relación con la edad promedio al momento de enviudar?------------------
# Analiza: ¿Qué hipótesis pueden explicarse con base en estas diferencias, considerando aspectos como la edad del cónyuge, duración del vínculo y contexto demográfico?


  


# Chi-cuadrado ---------------------------------------------------------------------------------------------------------------------------------------------------------
## 1.	¿Existe asociación entre grupo de edad y ser madre o padre en tu cantón?------------------------------------------------------------------------------
# Analiza: ¿Qué reflexiones podrías hacer sobre la maternidad y paternidad en las distintas etapas de vida a nivel local?



  
## 2.	¿Hay una asociación entre grupo de edad y tipo de discapacidad en tu cantón?---------------------------------------------------------------------------
# Analiza: ¿Qué podría sugerir esta relación sobre los ciclos de vida y los riesgos específicos que enfrentan ciertos grupos etarios?




# Boxplot --------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1.	¿Cómo varía la edad de las personas pensionistas según el sexo en tu cantón?---------------------------------------------------------------------------
# Analiza: ¿Qué observas en cuanto a dispersión o valores extremos? ¿Estas diferencias podrían deberse a trayectorias laborales diferenciadas entre hombres y mujeres?


  
## 2.	¿Cuál es la distribución de la edad al momento de enviudar según el sexo en tu cantón?-----------------------------------------------------------------
# Analiza: ¿Las diferencias de rango y valores atípicos podrían relacionarse con patrones demográficos, sociales o de salud?



  

# Correlación -------------------------------------------------------------
## 1. Calcula la matriz de correlación entre todas las variables numéricas disponibles en tu cantón. ¿Qué relaciones relevantes o inesperadas se evidencian entre estas variables?-----
# Analiza: ¿Qué pares de variables parecen estar fuertemente relacionadas y cómo podrías interpretar esa asociación en términos demográficos o sociales?





# Exportacion de resultados
# Exportar tablas cruzadas a CSV - Por confirmar

