# ==============================================================================
# Proyecto: Análisis de la Participación Ciudadana 
# Script: Exploración 
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
library(arrow)
library(naniar)
library(srvyr) 
library(flextable)
library(scales)

# CONFIGURACIONES GLOBALES DE ESTÉTICA Y RUTAS
color <- "#1F4E79"
ruta_salida <- "outputs/tablas"
ruta_graficos <- "outputs/gráficos_univ"
ruta_bivariado <- "outputs/gráficos_biv"

dir.create(ruta_salida, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_bivariado, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 2. Cargamos la base de datos a utilizar —-------------------------------------------------------------
# ==============================================================================

enaho_explorar <- read_parquet("datos/procesados/enaho_acondicionada.parquet") 

# ==============================================================================
# 3. Preparación de etiquetas —---------------------------------------------------------------------------
# ==============================================================================

enaho_lbl <- enaho_explorar %>%
  mutate(
    # —-------------------------------------------------------------------------------------------------------------
    # --- MÓDULO 100: Vivienda y Servicios Básicos —
    # --- Agua  ---
    agua_procedencia_lbl = case_match(as.character(agua_procedencia),
                                      "1" ~ "Red pública, dentro de la vivienda",
                                      "2" ~ "Red pública, fuera de la vivienda",
                                      "3" ~ "Pilón o pileta de uso público",
                                      "4" ~ "Camión-cisterna u otro similar",
                                      "5" ~ "Pozo (agua subterránea)",
                                      "6" ~ "Manantial o puquio",
                                      "7" ~ "Otra",
                                      "8" ~ "Río, acequia, lago, laguna",
                                      .default = NA_character_
    ),
    agua_potable_lbl = case_match(as.character(agua_potable),
                                  "1" ~ "Sí",
                                  "2" ~ "No",
                                  .default = NA_character_
    ),
    agua_acceso_lbl = case_match(as.character(agua_acceso),
                                 "1" ~ "Sí",
                                 "2" ~ "No",
                                 .default = NA_character_
    ),
    # --- Desagüe ---
    desague_tipo_lbl = case_match(as.character(desague_tipo),
                                  "1" ~ "Red pública de desagüe dentro de la vivienda",
                                  "2" ~ "Red pública fuera de la vivienda",
                                  "3" ~ "Letrina (con tratamiento)",
                                  "4" ~ "Pozo séptico o biodigestor",
                                  "5" ~ "Pozo ciego o negro",
                                  "6" ~ "Río, acequia, canal o similar",
                                  "7" ~ "Otra",
                                  "9" ~ "Campo abierto o al aire libre",
                                  .default = NA_character_
    ),
    
    # --- Alumbrado ---
    tipo_alumbrado = case_when(
      electricidad    == "1" ~ 1,
      lampara         == "1" ~ 2,
      vela            == "1" ~ 3,
      generador       == "1" ~ 4,
      alumbrado       == "1" ~ 5,
      sin_alumbrado   == "1" ~ 6,
      TRUE ~ NA_real_
    ),
    tipo_alumbrado_lbl = factor(
      tipo_alumbrado,
      levels = 1:6,
      labels = c(
        "Electricidad",
        "Petróleo/gas (Lámpara)",
        "Vela",
        "Generador",
        "Otro",
        "No cuenta con alumbrado"
      )
    ),
    
    # —-------------------------------------------------------------------------------------------------------------
    # --- MÓDULO 300: Educación ---
    edu_nivel_max_lbl = case_match(as.character(edu_nivel_max),
                                   "1"  ~ "Sin nivel",
                                   "2"  ~ "Educación inicial",
                                   "3"  ~ "Primaria incompleta",
                                   "4"  ~ "Primaria completa",
                                   "5"  ~ "Secundaria incompleta",
                                   "6"  ~ "Secundaria completa",
                                   "7"  ~ "Superior no univ. Incompleta",
                                   "8"  ~ "Superior no univ. completa",
                                   "9"  ~ "Superior univ. incompleta",
                                   "10" ~ "Superior univ. completa",
                                   "11" ~ "Maestría/Doctorado",
                                   "12" ~ "Básica especial",
                                   "99" ~ "Missing value",
                                   .default = NA_character_
    ),
    
    # —-------------------------------------------------------------------------------------------------------------
    # --- MÓDULO 400: Salud ---
    seguro_essalud_lbl       = ifelse(seguro_essalud == "1", "EsSalud", "No"),
    seguro_privado_lbl       = ifelse(seguro_privado == "1", "Seguro Privado", "No"),
    seguro_entidad_lbl       = ifelse(seguro_entidad == "1", "Entidad Prestadora", "No"),
    seguro_ffa_pnp_lbl       = ifelse(seguro_ffa_pnp == "1", "Seguro FF.AA./Policiales", "No"),
    seguro_sis_lbl           = ifelse(seguro_sis == "1", "SIS", "No"),
    seguro_universitario_lbl = ifelse(seguro_universitario == "1", "Seguro Universitario", "No"),
    seguro_escolar_lbl       = ifelse(seguro_escolar == "1", "Seguro Escolar Privado", "No"),
    seguro_otro_lbl          = ifelse(seguro_otro == "1", "Otro", "No")
  )

# —-------------------------------------------------------------------------------------------------------------
# --- MÓDULO 800: Participación ---
nombres_organizaciones <- c(
  "p801_1"  = "Clubes Deportivos",        "p801_2"  = "Partido Político", 
  "p801_3"  = "Clubes Culturales",        "p801_4"  = "Junta Vecinal",
  "p801_5"  = "Ronda Campesina",          "p801_6"  = "Asoc. Regantes", 
  "p801_7"  = "Asoc. Profesional",        "p801_8"  = "Sindicato / Trabajadores",
  "p801_9"  = "Club de Madres",           "p801_10" = "APAFA", 
  "p801_11" = "Vaso de Leche",            "p801_12" = "Comedor Popular",
  "p801_13" = "CLAS (Salud)",             "p801_14" = "Presupuesto Participativo",
  "p801_15" = "CCLD (Municipio)",         "p801_16" = "Comunidad Campesina",
  "p801_17" = "Asoc. Agropecuaria",       "p801_18" = "Otros",
  "p801_19" = "No participa",             "p801_20" = "Desayuno/Almuerzo Escolar"
)

# —-------------------------------------------------------------------------------------------------------------
for (col in names(nombres_organizaciones)) {
  if (col %in% colnames(enaho_lbl)) {
    org_nombre <- nombres_organizaciones[col]
    enaho_lbl <- enaho_lbl %>%
      mutate(!!paste0(col, "_lbl") := ifelse(.data[[col]] == "1", paste("Sí -", org_nombre), "No"))
  }
}
