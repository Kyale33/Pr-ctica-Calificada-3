#===========================================================================
# Proyecto: Análisis de la participación ciudadana según el acceso a servicios básicos con datos de la ENAHO
# Autor: Kyara Ronchi
# Fecha: 14-06
# Objetivo del script: Cargar los módulos y hacer los joins
#===========================================================================

# 1. Carga de librerías------------------------------------------------------
library(rio)
library(tidyverse)
library(janitor)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# 2. Se usa renv-------------------------------------------------------------
# Se utiliza renv::init() para guardar la versión actual de las librerías utilizadas
renv::snapshot(force = TRUE)

# 3. Importar datos------------------------------------------------------
# Se cargan los módulos de vivienda, educación, salud y participación ciudadana para el análisis
# - Módulo 100: Vivienda
# - Módulo 300: Educación
# - Módulo 400: Salud
# - Módulo 800: Participación Ciudadana

mod100  = import("Enaho01-2025-100.csv", encoding = "Latin-1")
mod300  = import("Enaho01A-2025-300.csv", encoding = "Latin-1")
mod400  = import("Enaho01A-2025-400.csv", encoding = "Latin-1")
mod_pc1 = import("Enaho01-2025-800A.csv", encoding = "Latin-1")
mod_pc2 = import("Enaho01-2025-800B.csv", encoding = "Latin-1")

# 4. Unión de datos-------------------------------------------------------
# El key de hogar se estructura para darle un identificador al hogar encuestado
keys_hogar <- c("AÑO", "MES", "CONGLOME", "VIVIENDA", "HOGAR")
# El key de persona se estructura para darle un identificador a la persona encuestada
keys_personas <- c(keys_hogar, "CODPERSO")

# Unimos los módulos 300 (educación) y 400 (salud) por las variables comunes de keys_personas, ya que las preguntas de ambos módulos son para todas las personas y no para una sola por hogar
base_x_persona <- mod400 %>%
  left_join(mod300, by = keys_personas)

# Unimos el módulo 800 (participación ciudadana) adaptando la key que falta con otras variables que hay en común, ya que no necesariamente todos los que respondieron el módulo 300 y 400 también respondieron el 800 (Que suele ser sólo para el informante del hogar)
base_personas <- base_x_persona %>%
  left_join(mod_pc1, by = c(
    "AÑO" = "AÑO", 
    "MES" = "MES", 
    "CONGLOME" = "CONGLOME", 
    "VIVIENDA" = "VIVIENDA", 
    "HOGAR" = "HOGAR", 
    "CODPERSO" = "CODINFOR"
  ))

# Unimos las bases de datos para que podamos tener como unidad de análisis al informante del hogar
base_final <- base_personas %>%
  left_join(mod100, by = keys_hogar)

# 5. Se inicia el análisis
# Primero visualizamos cuál es la cantidad de participación en cada una de las 20 iniciativas sobre las que pregunta la ENAHO (P801_1 - P801_20)
tabla_participacion <- base_final %>%
  select(starts_with("P801_")) %>%
  pivot_longer(cols = everything(), names_to = "Iniciativa", values_to = "Respuesta") %>%
  mutate(Respuesta = trimws(as.character(Respuesta))) %>%
  filter(Respuesta != "0" & !is.na(Respuesta) & Respuesta != "NA") %>%
  count(Iniciativa) %>%
  arrange(desc(n))
print(tabla_participacion)

# Cambiamos los nombres de las variables a cada iniciativa
nombres_iniciativas <- c(
  "P801_1" = "Clubes Deportivos", "P801_2" = "Partido Político", 
  "P801_3" = "Clubes Culturales", "P801_4" = "Junta Vecinal",
  "P801_5" = "Ronda Campesina", "P801_6" = "Asoc. Regantes", 
  "P801_7" = "Asoc. Profesional", "P801_8" = "Sindicato / Trabajadores",
  "P801_9" = "Club de Madres", "P801_10" = "APAFA", 
  "P801_11" = "Vaso de Leche", "P801_12" = "Comedor Popular",
  "P801_13" = "CLAS (Salud)", "P801_14" = "Presupuesto Participativo",
  "P801_15" = "CCLD (Municipio)", "P801_16" = "Comunidad Campesina",
  "P801_17" = "Asoc. Agropecuaria", "P801_18" = "Otros",
  "P801_19" = "No participa", "P801_20" = "Desayuno/Almuerzo Escolar"
)

# Graficamos
tabla_participacion %>%
  mutate(Iniciativa_Nombre = nombres_iniciativas[Iniciativa]) %>%
  filter(!is.na(Iniciativa_Nombre)) %>% 
  ggplot(aes(x = reorder(Iniciativa_Nombre, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  
  theme_minimal() +
  labs(
    title = "Participación de los Ciudadanos según Iniciativa Organizacional",
    subtitle = "Módulo de Gobernabilidad - ENAHO",
    x = "Iniciativa Ciudadana",
    y = "Cantidad de Participantes (N)"
  )

# Creamos 2 columnas:
# - Índice de participación: sumatoria de la cantidad de iniciativas ciudadanas en las que el informante participa
# - Variable dummy sobre si hay participación ciudadana o no
base_final <- base_final %>%
  mutate(
    indice_participacion = (!P801_1  %in% c("0", NA)) + (!P801_2  %in% c("0", NA)) + 
      (!P801_3  %in% c("0", NA)) + (!P801_4  %in% c("0", NA)) + 
      (!P801_5  %in% c("0", NA)) + (!P801_6  %in% c("0", NA)) + 
      (!P801_7  %in% c("0", NA)) + (!P801_8  %in% c("0", NA)) + 
      (!P801_9  %in% c("0", NA)) + (!P801_10 %in% c("0", NA)) + 
      (!P801_11 %in% c("0", NA)) + (!P801_12 %in% c("0", NA)) + 
      (!P801_13 %in% c("0", NA)) + (!P801_14 %in% c("0", NA)) + 
      (!P801_15 %in% c("0", NA)) + (!P801_16 %in% c("0", NA)) + 
      (!P801_17 %in% c("0", NA)) + (!P801_18 %in% c("0", NA)) + 
      (!P801_20 %in% c("0", NA)),
    participacion_ciudadana = ifelse(indice_participacion > 0, "Sí", "No")
  )

# Posteriormente se puede continuar con el cruce de la información de participación ciudadana con variables como salud, educación, acceso a agua, electricidad y desagüe

