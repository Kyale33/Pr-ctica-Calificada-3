#===========================================================================
# Proyecto: Análisis de la participación ciudadana según el acceso a servicios básicos con datos de la ENAHO
# Autor: Kyara Ronchi
# Fecha: 14-06
# Objetivo del script: Cargar los módulos y hacer los joins
#===========================================================================

#===========================================================================
# Proyecto: Análisis de la participación ciudadana según el acceso a servicios básicos con datos de la ENAHO
# Autor: Kyara Ronchi
# Fecha: 14-06
# Objetivo del script: Cargar los módulos y hacer los joins
#===========================================================================

# ==============================================================================
# 1. Carga de librerías  —------------------------------------------------------------------------------------
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

# ==============================================================================
# 2. Utilizamos renv —---------------------------------------------------------------------------------------
# ==============================================================================
# Se utiliza renv::init() en la consola para guardar la versión actual de las librerías utilizadas

renv::snapshot(force = TRUE)

# ==============================================================================
# 3. Importar datos —---------------------------------------------------------------------------------------
# ==============================================================================
# Se cargan los módulos de vivienda, educación, salud y participación ciudadana para el análisis

# ----------------------------------------------------------------------------------------------------------------
# - Módulo 100: Vivienda
mod100  <- import("datos/crudos/Enaho01-2025-100.csv", encoding = "Latin-1") %>% clean_names()

# ----------------------------------------------------------------------------------------------------------------
# - Módulo 300: Educación
mod300  <- import("datos/crudos/Enaho01A-2025-300.csv", encoding = "Latin-1") %>% clean_names()

# ----------------------------------------------------------------------------------------------------------------
# - Módulo 400: Salud
mod400    <- import("datos/crudos/Enaho01A-2025-400.csv", encoding = "Latin-1") %>% clean_names()

# ----------------------------------------------------------------------------------------------------------------
# - Módulo 800: Participación Ciudadana
mod_pc1 <- import("datos/crudos/Enaho01-2025-800A.csv", encoding = "Latin-1") %>% clean_names()

# ==============================================================================
# 4. Creación de id —----------------------------------------------------------------------------------------
# ==============================================================================
# Construímos un id común en las bases de datos para que no ocurran solapamientos ni sobreescrituras

# ----------------------------------------------------------------------------------------------------------------
# Para el id de persona se utilizan las siguientes variables: 

# - CONGLOME
# - CODPERSO

mod400$id_persona <- paste(str_trim(as.character(mod400$conglome)), 
                           str_trim(as.character(mod400$codperso)), sep = "_")

mod300$id_persona <- paste(str_trim(as.character(mod300$conglome)), 
                           str_trim(as.character(mod300$codperso)), sep = "_")

if ("codinfor" %in% colnames(mod_pc1)) {
  mod_pc1$id_persona <- paste(str_trim(as.character(mod_pc1$conglome)), 
                              str_trim(as.character(mod_pc1$codinfor)), sep = "_")
} else {
  mod_pc1$id_persona <- paste(str_trim(as.character(mod_pc1$conglome)), 
                              str_trim(as.character(mod_pc1$codperso)), sep = "_")
}
# ----------------------------------------------------------------------------------------------------------------
# Para el id de hogar se utilizan las siguientes variables: 

# - CONGLOME
# - VIVIENDA
# - HOGAR

mod400$id_hogar <- paste(str_trim(as.character(mod400$conglome)), 
                         str_trim(as.character(mod400$vivienda)), 
                         str_trim(as.character(mod400$hogar)), sep = "_")

mod100$id_hogar <- paste(str_trim(as.character(mod100$conglome)), 
                         str_trim(as.character(mod100$vivienda)), 
                         str_trim(as.character(mod100$hogar)), sep = "_")

# ==============================================================================
# 5. Extracción de las variables relevantes —------------------------------------------------------------
# ==============================================================================
# Extraemos las variables relevantes de cada base de datos a utilizar para evitar que se sobreescriban
# Esta decisión fue tomada frente al borrado de una de las variables al momentos de unir previamente las bases de datos, por lo que se tuvo que optar por seleccionar variables específicas y unificarlas

# ----------------------------------------------------------------------------------------------------------------
# Módulo 100: Vivienda y Servicios Básicos

vivienda_extracto <- data.frame(
  id_hogar        = mod100$id_hogar, # El id de hogar que generamos
  estrato            = mod100$estrato, 
  agua_procedencia   = mod100$p110, # ¿De qué fuente procede principalmente el agua de su hogar?
  agua_potable       = mod100$p110a1, # ¿El agua es potable?
  agua_acceso        = mod100$p110c, # ¿El hogar tiene acceso al servicio de agua todos los días de la semana?
  desague_tipo       = mod100$p111a, # ¿El baño o servicio higiénico que tiene su hogar esta conectado a…?
  electricidad       = mod100$p1121, # Tipo de alumbrado del hogar: Electricidad
  lampara            = mod100$p1123, # Tipo de alumbrado del hogar: Petróleo/Gas (lámpara)
  vela               = mod100$p1124, # Tipo de alumbrado del hogar: Vela
  generador          = mod100$p1125, # Tipo de alumbrado del hogar: Generador
  alumbrado          = mod100$p1126, # Tipo de alumbrado del hogar: Otro
  sin_alumbrado      = mod100$p1127 # No utiliza alumbrado en el hogar
) %>% distinct(id_hogar, .keep_all = TRUE)

# —-------------------------------------------------------------------------------------------------------------
# Módulo 300: Educación 

edu_extracto <- data.frame(
  id_persona       = mod300$id_persona, # El id de persona que generamos
  edu_nivel_max      = mod300$p301a, # ¿Cuál es el último año o grado de estudios y nivel que aprobó? 
  edu_año_max        = mod300$p301b, # ¿Cuál es el último año o grado de estudios y nivel que aprobó? - Año
  edu_grado_max      = mod300$p301c # ¿Cuál es el último año o grado de estudios y nivel que aprobó? - Grado
) %>% distinct(id_persona, .keep_all = TRUE)

# —-------------------------------------------------------------------------------------------------------------
# Módulo 400: Salud 

salud_extracto <- mod400 %>% 
  select(
    id_persona, # El id de persona que generamos
    seguro_essalud       = p4191, # Afiliado al seguro: Essalud
    seguro_sis           = p4195, # Afiliado al seguro: SIS
    seguro_ffa_pnp       = p4194, # Afiliado al seguro: FFAA/PNP
    seguro_privado       = p4192, # Afiliado al seguro: Privado
    seguro_entidad       = p4193, # Afiliado al seguro: Entidad
    seguro_universitario = p4196, # Afiliado al seguro: Universitario
    seguro_escolar       = p4197, # Afiliado al seguro: Escolar
    seguro_otro          = p4198, # Afiliado al seguro: Otro
    factor_07 = factor07
  ) %>%  distinct(id_persona, .keep_all = TRUE)

# ==============================================================================
# 5. Unión de los módulos  —-------------------------------------------------------------------------------
# ==============================================================================
# Generamos una base de datos provisional con la información básica de nuestra unidad de análisis

# —-------------------------------------------------------------------------------------------------------------
# Unimos paso a paso las módulos

base_personas <- mod400 %>% 
  select(ano, mes, conglome, vivienda, hogar, codperso, id_persona, id_hogar)

base_paso1 <- base_personas %>% 
  left_join(edu_extracto, by = "id_persona")

base_paso2 <- base_paso1 %>% 
  left_join(salud_extracto, by = "id_persona")

base_paso3 <- base_paso2 %>% 
  left_join(distinct(mod_pc1, id_persona, .keep_all = TRUE), by = "id_persona")

base_final <- base_paso3 %>% 
  left_join(vivienda_extracto, by = "id_hogar")

# ==============================================================================
# 6. Exportar la base de datos generada —--------------------------------------------------------------
# ==============================================================================
# Se exporta la nueva base de datos creada

write_parquet(base_final, "base_final_0726.parquet")
renv::snapshot(force = TRUE)

