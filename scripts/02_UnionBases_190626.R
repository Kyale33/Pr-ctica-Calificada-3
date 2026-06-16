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
