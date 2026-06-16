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
