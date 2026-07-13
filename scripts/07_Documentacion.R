# ==============================================================================
# Proyecto: Análisis de la Participación Ciudadana
# Script: Documentar
# Autor: Kyara Ronchi
# Objetivo: Añadir metadatos a la base analítica y generar el codebook final.
# ==============================================================================

# ==============================================================================
# 0. Configuración y Paquetes --------------------------------------------------
# ==============================================================================
# Instala solo lo que te falte (evita reinstalar en cada corrida)
library(tidyverse)
library(arrow)
library(here)
library(labelled)   # Para inyectar etiquetas y metadatos en las variables
library(codebook)   # Para automatizar el libro de códigos interactivo
library(dataMaid)   # Para auditoría y reportes rápidos de calidad de datos

# Cargamos la base analítica con la que se concluyó el script de clasificación
enaho_final <- read_parquet(here("datos", "procesados", "enaho_final.parquet"))
  
# ==============================================================================
# 1. Selección de variables para el codebook  ----------------------------------
# ==============================================================================

# Base con las variables clave: indicadores de participación, servicios dignos
# y las variables de diseño muestral usadas en el análisis

enaho_codebook <- enaho_final %>%
  select(
    # Variables base (Módulos 100,300, 400 y 800, ya etiquetadas en 04_Exploracion.R)
    agua_procedencia_lbl, desague_tipo_lbl, tipo_alumbrado_lbl, edu_nivel_max_lbl,
    # Indicadores binarios y sus etiquetas (construidos en 06_Clasificacion.R)
    indice_participacion, participacion_ciudadana,
    serv_agua_digna_lbl, serv_desague_digno_lbl, serv_energia_digna_lbl,
    todos_servicios_dignos_lbl, serv_salud_seguro_lbl, seguro_contributivo_lbl,
    edu_secundaria_completa_lbl, edu_superior_completa_lbl,
    indice_servicios_dignos, indice_servicios_dignos_lbl,
    # Diseño muestral
    conglome, estrato, factor_07
  ) %>%
  mutate(across(where(is.character), as.factor)) # para que 'codebook' detecte las etiquetas

# Exportamos la base final del proyecto, ya lista para documentar
write_parquet(enaho_codebook, here("datos", "procesados", "enaho_participacion_codebook.parquet"))

# ==============================================================================
# 2. Inyectamos todos los metadatos --------------------------------------------------
# ==============================================================================
# Un codebook requiere la etiqueta descriptiva y la fuente original de cada variable.
# Usamos var_label() para darles un nombre humano y coherente.

# A. Variables base (categorías originales, Módulos 100 y 300 de la ENAHO) ---------------------------------------
var_label(enaho_codebook$agua_procedencia_lbl) <- "Procedencia del agua en el hogar (Módulo 100: Vivienda y Servicios Básicos)"
var_label(enaho_codebook$desague_tipo_lbl) <- "Tipo de conexión de desagüe del hogar (Módulo 100: Vivienda y Servicios Básicos)"
var_label(enaho_codebook$tipo_alumbrado_lbl) <- "Tipo de alumbrado del hogar, derivado de electricidad/lámpara/vela/generador (Módulo 100: Vivienda y Servicios Básicos)"
var_label(enaho_codebook$edu_nivel_max_lbl) <- "Nivel educativo máximo alcanzado por el encuestado (Módulo 300: Educación)"

# B. Índices principales -----------------------------------------------------------------------------------------
var_label(enaho_codebook$indice_participacion) <- "Índice aditivo de participación ciudadana (N° de organizaciones, Módulo 800: Participación, p801_1 a p801_20)"
var_label(enaho_codebook$participacion_ciudadana) <- "Participa en al menos una organización (Sí/No)"
var_label(enaho_codebook$indice_servicios_dignos) <- "Índice aditivo de acceso a servicios dignos (0-5)"
var_label(enaho_codebook$indice_servicios_dignos_lbl) <- "Nivel de acceso a servicios dignos (Bajo/Medio/Alto)"

# C. Componentes del índice de servicios dignos
# —-------------------------------------------------------------------------------------------------------------
# C.1. Servicios del hogar dignos
var_label(enaho_codebook$serv_agua_digna_lbl) <- "Acceso a agua digna (Sí/No), a partir de agua_procedencia + agua_potable + agua_acceso (Módulo 100)"
var_label(enaho_codebook$serv_desague_digno_lbl) <- "Acceso a desagüe digno (Sí/No), a partir de desague_tipo (Módulo 100)"
var_label(enaho_codebook$serv_energia_digna_lbl) <- "Acceso a energía eléctrica digna (Sí/No), a partir de tipo_alumbrado (Módulo 100)"
var_label(enaho_codebook$todos_servicios_dignos_lbl) <- "Acceso simultáneo a agua, desagüe y energía dignos (Sí/No)"
# —-------------------------------------------------------------------------------------------------------------
# C.2. Acceso digno a servicios de salud
var_label(enaho_codebook$serv_salud_seguro_lbl) <- "Cuenta con algún seguro de salud (Sí/No), a partir de seguro_essalud/sis/privado/entidad/ffa_pnp/universitario/escolar (Módulo 400: Salud)"
var_label(enaho_codebook$seguro_contributivo_lbl) <- "Cuenta con seguro contributivo: Essalud, privado o de entidad (Sí/No) (Módulo 400: Salud)"
# —-------------------------------------------------------------------------------------------------------------
# C.3. Acceso digno a servicios educativos
var_label(enaho_codebook$edu_secundaria_completa_lbl) <- "Nivel educativo máximo: secundaria completa o superior (Sí/No), a partir de edu_nivel_max (Módulo 300)"
var_label(enaho_codebook$edu_superior_completa_lbl) <- "Nivel educativo máximo: superior completa (Sí/No), a partir de edu_nivel_max (Módulo 300)"

# —-------------------------------------------------------------------------------------------------------------
# D. Variables de diseño muestral
var_label(enaho_codebook$conglome) <- "Conglomerado (identificador de conglomerado muestral ENAHO)"
var_label(enaho_codebook$estrato) <- "Estrato de la muestra ENAHO"
var_label(enaho_codebook$factor_07) <- "Factor de expansión poblacional (ponderador ENAHO)"

# ==============================================================================
# 3. Documentación de decisiones metodológicas ---------------------------------
# ==============================================================================

# Diccionario de decisiones metodológicas
dict_metadata <- list(
  agua_procedencia_lbl = "Recodificación de la pregunta de procedencia del agua (Módulo 100) en 8 categorías: red pública dentro/fuera de la vivienda, pilón público, camión-cisterna, pozo, manantial/puquio, río/acequia/lago y otra.",
  
  desague_tipo_lbl = "Recodificación del tipo de conexión de desagüe (Módulo 100) en 8 categorías: red pública dentro/fuera de la vivienda, letrina con tratamiento, pozo séptico/biodigestor, pozo ciego/negro, río/acequia/canal, campo abierto y otra.",
  
  tipo_alumbrado_lbl = "Variable construida a partir de 6 columnas binarias del Módulo 100 (electricidad, lámpara, vela, generador, alumbrado, sin_alumbrado): se asigna la primera categoría que aplique, en ese orden de prioridad (electricidad tiene prioridad sobre las demás).",
  
  edu_nivel_max_lbl = "Recodificación del nivel educativo máximo alcanzado (Módulo 300) en 13 categorías, desde 'Sin nivel' hasta 'Maestría/Doctorado', incluyendo 'Básica especial' y 'Missing value' (código 99).",
  
  indice_participacion = "Suma del número de tipos de organización (columnas p801_1 a p801_18 y p801_20, Módulo 800) en las que el encuestado reportó participar. Los valores NA o codificados como 'NA' en cada columna se recodificaron a '0' (no participa) antes de sumar. La categoría p801_19 ('No participa') se excluye deliberadamente de la suma.",
  
  participacion_ciudadana = "Variable dicotómica derivada de indice_participacion: toma el valor 'Sí' si el índice es mayor a 0, y 'No' en caso contrario.",
  
  serv_agua_digna_lbl = "Un hogar tiene agua digna si la procedencia del agua está en categorías {1,2} (red pública), agua_potable == 1 y agua_acceso == 1 (acceso diario/continuo). Fuente: Módulo 100 (Vivienda y Servicios Básicos).",
  
  serv_desague_digno_lbl = "Un hogar tiene desagüe digno si desague_tipo está en categorías {1,2} (conexión a red pública). Fuente: Módulo 100.",
  
  serv_energia_digna_lbl = "Un hogar tiene energía digna si tipo_alumbrado == 1 (electricidad por red pública). Fuente: Módulo 100.",
  
  todos_servicios_dignos_lbl = "Toma el valor 'Sí' únicamente si el hogar cumple simultáneamente con serv_agua_digna, serv_desague_digno y serv_energia_digna.",
  
  serv_salud_seguro_lbl = "Toma el valor 'Sí' si la persona respondió afirmativamente (== '1') en al menos uno de los siguientes tipos de seguro: Essalud, SIS, privado, de entidad empleadora, FFAA/PNP, universitario o escolar. Fuente: Módulo 400 (Salud).",
  
  seguro_contributivo_lbl = "Subconjunto de serv_salud_seguro: toma 'Sí' solo si la afiliación es a Essalud, seguro privado o seguro de entidad empleadora (excluye SIS y seguros no contributivos).",
  
  edu_secundaria_completa_lbl = "Toma el valor 'Sí' si edu_nivel_max está en el rango {6,...,11}, correspondiente a secundaria completa o niveles educativos superiores.",
  
  edu_superior_completa_lbl = "Toma el valor 'Sí' si edu_nivel_max está en {8, 10, 11}, correspondiente a superior no universitaria completa, superior universitaria completa o posgrado.",
  
  indice_servicios_dignos = "Suma de 5 componentes binarios (0/1): serv_agua_digna + serv_desague_digno + serv_energia_digna + serv_salud_seguro + edu_secundaria_completa. Rango teórico: 0 a 5. Los casos con valor NA fueron excluidos de la base analítica.",
  
  indice_servicios_dignos_lbl = "Recodificación categórica de indice_servicios_dignos: 'Acceso Bajo (0-2 recursos)' si <= 2; 'Acceso Medio (3-4 recursos)' si está en {3,4}; 'Acceso Alto (Todos los recursos)' si == 5. Variable declarada como factor ordenado en ese mismo orden."
)

# Aplicamos las descripciones iterativamente a las columnas correspondientes -------------------------------------------------------------------
for (var in names(dict_metadata)) {
  attr(enaho_codebook[[var]], "description") <- dict_metadata[[var]]
}

# Agregamos metadatos a nivel de ESTUDIO (Ficha Técnica) ---------------------------------------------------------------------------------------
metadata(enaho_codebook)$name <- "Base de Datos Analítica - Participación Ciudadana ENAHO"
metadata(enaho_codebook)$description <- "Base construida a partir de la Encuesta Nacional de Hogares (ENAHO), con indicadores de participación ciudadana y de acceso a servicios dignos (agua, desagüe, energía, salud y educación)."
metadata(enaho_codebook)$creator <- "Kyara Ronchi"

# Guardamos la base con toda la metadata inyectada ---------------------------------------------------------------------------------------------
write_parquet(enaho_codebook, here("datos", "procesados", "enaho_participacion_codebook.parquet"))

# ==============================================================================
# 4. Generación automatizada de documentación ----------------------------------
# ==============================================================================

# ------------------------------------------------------------------------------
# Reporte rápido
# ------------------------------------------------------------------------------
# Ideal para detectar anomalías finales antes de publicar.
# Genera un HTML/PDF con la distribución, NAs y valores atípicos de cada variable.

makeDataReport(
  enaho_codebook,
  output = "html",
  file = here("outputs", "CodeBook_dataMaid.Rmd"),
  replace = TRUE,
  reportTitle = "CodeBook del proyecto - Participación Ciudadana ENAHO"
)

# ------------------------------------------------------------------------------
# Reporte más detallado --------------------------------------------------------
# ------------------------------------------------------------------------------
# Genera un CodeBook más completo, incluyendo frecuencias, tipos, etiquetas
# y estadísticos básicos de manera interactiva.

codebook(enaho_codebook)
