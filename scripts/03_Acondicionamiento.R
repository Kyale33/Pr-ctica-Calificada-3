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
base_final_0726 <- read_parquet("datos/procesados/base_final_0726.parquet")

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


# ==============================================================================
# 4. Diagnóstico de NAs y Reporte —---------------------------------------------------------------------
# ==============================================================================

# —-------------------------------------------------------------------------------------------------------------
# 4. 1 Visualización gráfica de valores perdidos (naniar) ===============================

grafico_nas <- gg_miss_var(enaho_seleccion, show_pct = TRUE) +
  labs(
    title = "Porcentaje de Valores Perdidos (NAs) por Variable Original",
    subtitle = "Proyecto: Análisis de Acceso a Servicios Dignos y Participación (ENAHO)",
    y = "% de Valores Perdidos",
    x = "Variables"
  ) +
  theme_minimal()

ggsave("outputs/tratamiento_nas/Grafico_NAs_Variables.png", plot = grafico_nas, 
       width = 9, height = 7, bg = "white")

# —-------------------------------------------------------------------------------------------------------------
# 4. 2 Reporte Tabular ============================================================

reporte_nas <- enaho_seleccion %>%
  summarise(across(everything(), ~ round(sum(is.na(.)) / n() * 100, 2))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "porcentaje_na") %>%
  arrange(desc(porcentaje_na))

write_csv(reporte_nas, "outputs/tratamiento_nas/datos_perdidos_Reporte_Datos_Perdidos.csv")


# ==============================================================================
# 5. Tratamiento y limpieza de NAs —--------------------------------------------------------------------
# ==============================================================================
# Realizamos el tratamiento de los datos para reemplazar los valores perdidos o NAs.

# Definición de la función de la Moda 
calcular_moda <- function(x) {
  ux <- unique(na.omit(x)) 
  if(length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

# Aplicación del tratamiento por módulos
enaho_limpia_sin_nas <- enaho_seleccion %>%
  mutate(
    # --- MÓDULO VIVIENDA Y SERVICIOS BÁSICOS —
    # Como nuestras variables son principalmente categóricas, utilizaremos el valor más repetido para rellenar los NAs
    agua_procedencia = replace_na(agua_procedencia, calcular_moda(agua_procedencia)),
    agua_potable     = replace_na(agua_potable, calcular_moda(agua_potable)),
    agua_acceso      = replace_na(agua_acceso, calcular_moda(agua_acceso)),
    desague_tipo     = replace_na(desague_tipo, calcular_moda(desague_tipo)),
    electricidad     = replace_na(electricidad, calcular_moda(electricidad)),
    lampara          = replace_na(lampara, calcular_moda(lampara)),
    vela             = replace_na(vela, calcular_moda(vela)),
    generador        = replace_na(generador, calcular_moda(generador)),
    alumbrado        = replace_na(alumbrado, calcular_moda(alumbrado)),
    sin_alumbrado    = replace_na(sin_alumbrado, calcular_moda(sin_alumbrado)),
    
    # --- MÓDULO EDUCACIÓN ---
    # Si es NA, metodológicamente se asume "0" (Sin Nivel / No sabe)
    edu_nivel_max = replace_na(edu_nivel_max, "0"),
    
    # --- MÓDULO SALUD (SEGUROS) ---
    # Los casilleros en blanco en la lista de seguros se estandarizan a "0" (No tiene)
    seguro_essalud       = replace_na(as.character(seguro_essalud), "0"),
    seguro_sis           = replace_na(as.character(seguro_sis), "0"),
    seguro_ffa_pnp       = replace_na(as.character(seguro_ffa_pnp), "0"),
    seguro_privado       = replace_na(as.character(seguro_privado), "0"),
    seguro_entidad       = replace_na(as.character(seguro_entidad), "0"),
    seguro_universitario = replace_na(as.character(seguro_universitario), "0"),
    seguro_escolar       = replace_na(as.character(seguro_escolar), "0"),
    seguro_otro          = replace_na(as.character(seguro_otro), "0")
  )

# ==============================================================================
# 6. Exportación de la base de datos acondicionada
# ==============================================================================

write_parquet(enaho_limpia_sin_nas, "enaho_acondicionada.parquet") 
