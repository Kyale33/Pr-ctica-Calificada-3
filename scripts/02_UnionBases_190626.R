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
