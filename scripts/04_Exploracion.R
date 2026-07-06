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

# ==============================================================================
# 4. Diseño muestral Univariado —------------------------------------------------------------------------
# ==============================================================================

# Configuración del diseño muestral mediante el factor de expansión
class(enaho_lbl$factor_07)
str(enaho_lbl$factor_07)
enaho_lbl <- enaho_lbl %>%
  mutate(factor_07 = as.numeric(gsub(",", ".", factor_07)))
enaho_diseno_uni <- enaho_lbl %>%
  filter(!is.na(factor_07)) %>%  
  as_survey_design(ids = conglome, strata = estrato, weights = factor_07, nest = TRUE)

# ==============================================================================
# 5. Exploración Univariada: Tablas Descriptivvas —---------------------------------------------------
# ==============================================================================

# Definimos una función para crear un formato Flextable estandarizado
formato_table <- function(df, titulo) {
  flextable(df) %>%
    set_header_labels(Poblacion = "Población", Porcentaje = "Porcentaje (%)") %>%
    add_header_lines(values = titulo) %>%
    add_footer_lines(values = "Fuente: INEI - Encuesta Nacional de Hogares (ENAHO). Datos expandidos.") %>%
    theme_vanilla() %>% autofit() %>% align(align = "center", part = "all") %>% bold(part = "header")
}

# —-------------------------------------------------------------------------------------------------------------
# 5.1 VIVIENDA —-------------------------------------------------------------------------------------------
# —-------------------------------------------------------------------------------------------------------------

# 5.1.1 Tabla de distribución de la población según procedencia del agua
tabla_agua <- enaho_diseno_uni %>%
  group_by(agua_procedencia_lbl) %>%
  summarise(Poblacion = survey_total(vartype = NULL), Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Poblacion)) %>%
  mutate(Poblacion = comma(round(Poblacion, 0)), Porcentaje = paste0(round(Porcentaje, 2), "%"))

ft_agua <- formato_table(tabla_agua, "Tabla 5.1.1: Distribución de la población según procedencia del agua")
save_as_image(ft_agua, path = paste0(ruta_salida, "/5_1_1_Tabla_Agua.png"))

# —-------------------------------------------------------------------------------------------------------------

# 5.1.2 Tabla de distribución de la población según tipo de desagüe
tabla_desague <- enaho_diseno_uni %>%
  group_by(desague_tipo_lbl) %>%
  summarise(Poblacion = survey_total(vartype = NULL), Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Poblacion)) %>%
  mutate(Poblacion = comma(round(Poblacion, 0)), Porcentaje = paste0(round(Porcentaje, 2), "%"))

ft_desague <- formato_table(tabla_desague, "Tabla 5.1.2: Distribución de la población según tipo de desagüe")
save_as_image(ft_desague, path = paste0(ruta_salida, "/5_1_2_Tabla_Desague.png"))

# —-------------------------------------------------------------------------------------------------------------

# 5.1.3 Tabla de distribución de la población según procedencia del agua
tabla_alumbrado <- enaho_diseno_uni %>%
  group_by(tipo_alumbrado_lbl) %>%
  summarise(Poblacion = survey_total(vartype = NULL), Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Poblacion)) %>%
  mutate(Poblacion = comma(round(Poblacion, 0)), Porcentaje = paste0(round(Porcentaje, 2), "%"))

ft_alumbrado <- formato_table(tabla_alumbrado, "Tabla 5.1.3: Distribución de la población según tipo de alumbrado")
save_as_image(ft_alumbrado, path = paste0(ruta_salida, "/5_1_3_Tabla_Alumbrado.png"))

# —-------------------------------------------------------------------------------------------------------------
# 5.2 EDUCACIÓN —-----------------------------------------------------------------------------------------
# —-------------------------------------------------------------------------------------------------------------

# 5.2 Tabla de distribución de la población según máximo nivel educativo alcanzado
tabla_nivel_educacion <- enaho_diseno_uni %>%
  group_by(edu_nivel_max_lbl) %>%
  summarise(Poblacion = survey_total(vartype = NULL), Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  mutate(Poblacion = comma(round(Poblacion, 0)), Porcentaje = paste0(round(Porcentaje, 2), "%"))

ft_nivel_educacion <- formato_table(tabla_nivel_educacion, "Tabla 5.2: Distribución del nivel educativo máximo")
save_as_image(ft_nivel_educacion, path = paste0(ruta_salida, "/5_2_Tabla_Educacion.png"))

# —-------------------------------------------------------------------------------------------------------------
# 5.3 SALUD —------------------------------------------------------------------------------------------------
# —-------------------------------------------------------------------------------------------------------------

# 5.3 Tabla de distribución de la población según tipo de seguro
seguros_lista <- c("seguro_sis", "seguro_essalud", "seguro_privado", "seguro_ffa_pnp", "seguro_universitario")
tabla_salud_resultados <- data.frame()

for (seg in seguros_lista) {
  calc <- enaho_diseno_uni %>%
    group_by(!!!syms(seg)) %>%
    summarise(Poblacion = survey_total(vartype = NULL)) %>%
    filter(get(seg) == "1") %>%
    mutate(Tipo_Seguro = seg) %>%
    select(Tipo_Seguro, Poblacion)
  tabla_salud_resultados <- bind_rows(tabla_salud_resultados, calc)
}

tabla_salud_final <- tabla_salud_resultados %>%
  mutate(
    Tipo_Seguro = case_match(Tipo_Seguro,
                             "seguro_sis" ~ "Seguro Integral de Salud (SIS)",
                             "seguro_essalud" ~ "EsSalud",
                             "seguro_privado" ~ "Seguro Privado",
                             "seguro_ffa_pnp" ~ "Fuerzas Armadas / Policiales",
                             "seguro_universitario" ~ "Seguro Universitario"),
    Porcentaje = paste0(round((Poblacion / sum(enaho_lbl$factor_07, na.rm = TRUE)) * 100, 2), "%"),
    Poblacion_num = Poblacion, 
    Poblacion = comma(round(Poblacion, 0))
  )

ft_salud <- flextable(tabla_salud_final %>% select(-Poblacion_num)) %>%
  set_header_labels(Tipo_Seguro = "Tipo de Seguro", Poblacion = "Afiliados", Porcentaje = "Porcentaje (%)") %>%
  add_header_lines(values = "Tabla 5.3: Afiliación a seguros de salud (Respuestas Múltiples)") %>%
  theme_vanilla() %>% autofit() %>% align(align = "center", part = "all")
save_as_image(ft_salud, path = paste0(ruta_salida, "/5_3_Tabla_Salud.png"))

# —-------------------------------------------------------------------------------------------------------------
# 5.4 PARTICIPACIÓN —------------------------------------------------------------------------------------
# —-------------------------------------------------------------------------------------------------------------

# 5.4 Tabla de distribución de la población según Organización Ciudadana
tabla_organizaciones <- enaho_diseno_uni %>%
  summarise(across(
    all_of(names(nombres_organizaciones)),
    list(
      Poblacion  = ~survey_total(.x == as.numeric(str_remove(cur_column(), "p801_")), vartype = NULL),
      Porcentaje = ~survey_mean(.x == as.numeric(str_remove(cur_column(), "p801_")), vartype = NULL) * 100
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", ".value"),
    names_sep = "__"
  ) %>%
  mutate(Organizacion = nombres_organizaciones[variable]) %>%
  select(Organizacion, Poblacion, Porcentaje) %>%
  arrange(desc(Poblacion)) %>%
  mutate(
    Poblacion_num = as.numeric(Poblacion),          # 1. numérico puro, PRIMERO
    Poblacion = comma(round(Poblacion_num, 0)),      # 2. luego formateamos usando Poblacion_num
    Porcentaje = paste0(round(Porcentaje, 2), "%")
  )

ft_organizaciones <- formato_table(tabla_organizaciones, "Tabla 5.4: Distribución de la población según participación en organizaciones")
save_as_image(ft_organizaciones, path = paste0(ruta_salida, "/5_4_Tabla_Organizaciones.png"))

# ==============================================================================
# 6. Gráficos Individuales
# ==============================================================================

# —-------------------------------------------------------------------------------------------------------------
# 6.1 VIVIENDA —-------------------------------------------------------------------------------------------
# —-------------------------------------------------------------------------------------------------------------

# 6.1.1 Gráfico de distribución de la población según procedencia del agua
data_plot_agua <- enaho_diseno_uni %>% 
  group_by(agua_procedencia_lbl) %>% 
  summarise(Poblacion = survey_total(vartype = NULL)) %>% 
  as_tibble()

plot_agua <- ggplot(data_plot_agua, aes(x = reorder(agua_procedencia_lbl, Poblacion), y = Poblacion)) + 
  geom_col(fill = color, width = 0.7) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) + 
  labs(title = "Gráfico 6.1.1: Procedencia del agua", x = "Fuente", y = "Habitantes") + 
  theme_minimal()

ggsave(paste0(ruta_graficos, "/6_1_1Grafico_Agua.png"), plot = plot_agua, width = 8, height = 5, bg = "white")

# —-------------------------------------------------------------------------------------------------------------
# 6.1.2 Gráfico de distribución de la población según tipo de servicio higiénico
data_plot_desague <- enaho_diseno_uni %>% 
  group_by(desague_tipo_lbl) %>% 
  summarise(Poblacion = survey_total(vartype = NULL)) %>% 
  as_tibble()

plot_desague <- ggplot(data_plot_desague, aes(x = reorder(desague_tipo_lbl, Poblacion), y = Poblacion)) + 
  geom_col(fill = color, width = 0.7) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) + 
  labs(title = "Gráfico 6.1.2: Tipo de servicio higiénico", x = "Tipo", y = "Habitantes") + 
  theme_minimal()

ggsave(paste0(ruta_graficos, "/6_1_2_Grafico_Desague.png"), plot = plot_desague, width = 8, height = 5, bg = "white")

# —-------------------------------------------------------------------------------------------------------------
# 6.1.3 Gráfico de distribución de la población según tipo de alumbrado
data_plot_luz <- enaho_diseno_uni %>% 
  group_by(tipo_alumbrado_lbl) %>% 
  summarise(Poblacion = survey_total(vartype = NULL)) %>% 
  as_tibble()

plot_luz_ind <- ggplot(data_plot_luz, aes(x = reorder(tipo_alumbrado_lbl, Poblacion), y = Poblacion)) + 
  geom_col(fill = color, width = 0.2) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) + 
  labs(title = "Gráfico 6.1.3: Tipo de alumbrado del hogar", x = "Acceso", y = "Habitantes") + 
  theme_minimal()

ggsave(paste0(ruta_graficos, "/6_1_3_Grafico_Electricidad.png"), plot = plot_luz_ind, width = 6, height = 5, bg = "white")

# —-------------------------------------------------------------------------------------------------------------
# 6.2 EDUCACIÓN —-----------------------------------------------------------------------------------------
# —-------------------------------------------------------------------------------------------------------------

# 6.2 Gráfico de distribución de la población según máximo nivel educativo alcanzado
data_plot_edu <- enaho_diseno_uni %>% 
  group_by(edu_nivel_max_lbl) %>% 
  summarise(Poblacion = survey_total(vartype = NULL)) %>% 
  as_tibble()

plot_edu <- ggplot(data_plot_edu, aes(x = reorder(edu_nivel_max_lbl, Poblacion), y = Poblacion)) + 
  geom_col(fill = color, width = 0.7) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) + 
  labs(title = "Gráfico 6.2: Último nivel educativo aprobado", x = "Nivel", y = "Habitantes") + 
  theme_minimal()

ggsave(paste0(ruta_graficos, "/6_2_Grafico_Educacion.png"), plot = plot_edu, width = 8, height = 5, bg = "white")

# —-------------------------------------------------------------------------------------------------------------
# 6.3 SALUD —---------------------------------------------------------------------------------------------------
# —-------------------------------------------------------------------------------------------------------------

# 6.3 Gráfico de distribución de la población según tipo de seguro contratado
plot_salud <- ggplot(tabla_salud_final, aes(x = reorder(Tipo_Seguro, Poblacion_num), y = Poblacion_num)) + 
  geom_col(fill = "#5B9BD5", width = 0.5) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) + 
  labs(title = "Gráfico 6.3: Cobertura por tipo de seguro médico", x = "Seguro", y = "Afiliados") + 
  theme_minimal()

ggsave(paste0(ruta_graficos, "/6_3_Grafico_Salud.png"), plot = plot_salud, width = 8, height = 5, bg = "white")

# —-------------------------------------------------------------------------------------------------------------
# 6.4 PARTICIPACIÓN —------------------------------------------------------------------------------------
# —-------------------------------------------------------------------------------------------------------------

# 6.4 Gráfico de distribución de la población según Organización Ciudadana
plot_organizaciones <- ggplot(tabla_organizaciones, aes(x = reorder(Organizacion, Poblacion_num), y = Poblacion_num)) + 
  geom_col(fill = "#5B9BD5", width = 0.5) + 
  coord_flip() + 
  scale_y_continuous(labels = comma) + 
  labs(title = "Gráfico 6.4: Participación en organizaciones sociales", x = "Organización", y = "Personas") + 
  theme_minimal()

ggsave(paste0(ruta_graficos, "/6_4_Grafico_Organizaciones.png"), plot = plot_organizaciones, width = 8, height = 5, bg = "white")

