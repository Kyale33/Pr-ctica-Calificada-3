# ==============================================================================
# Proyecto: Análisis de la Participación Ciudadana 
# Script: Acondicionamiento
# Autor: Kyara Ronchi
# Fecha: 03/07/2026
# ==============================================================================

# ==============================================================================
#  1. Carga de librerías  —-----------------------------------------------------------------------------------
# ==============================================================================
library(rio)
library(tidyverse)
library(janitor)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(arrow)
library(naniar)

# ==============================================================================
# 2. Importar la base de datos con los joins —----------------------------------------------------------
# ==============================================================================
base_final_0726 <- read_parquet("base_final_0726.parquet")

# ==============================================================================
# 3. Carga, selección y renombrado —--------------------------------------------------------------------
# ==============================================================================
# Seleccionamos las variables relevantes de la base de datos para que no estén duplicados

enaho_seleccion <- base_final_0726 %>%
  filter(!is.na(p801_1)) %>% 
  select(
    # Llaves de integración y factores de expansión
    año                  = ano.x,
    mes                  = mes.x,
    conglome             = conglome.x, 
    vivienda             = vivienda.x, 
    hogar                = hogar.x, 
    codperso             = codperso,
    estrato             = estrato.x,
    factor_07 = factor_07, 
    
    # MODULO 100: Vivienda y Servicios Básicos
    agua_procedencia, agua_potable, agua_acceso, desague_tipo, 
    electricidad, lampara, vela, generador, alumbrado, sin_alumbrado,
    
    # MODULO 300: Educación
    edu_nivel_max, edu_año_max, edu_grado_max,
    
    # MODULO 400: Salud 
    seguro_essalud, seguro_sis, 
    seguro_ffa_pnp, seguro_privado, seguro_entidad, 
    seguro_universitario, seguro_escolar, seguro_otro,
    
    # Módulo 800: Participación
    starts_with("p801_")
  )
