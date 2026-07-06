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

# ==============================================================================
# 7. Exploración bivariada
# ==============================================================================
enaho_bivariado <- enaho_explorar %>%
  mutate(
    factor_07 = as.numeric(gsub(",", ".", factor_07)),
    agua_grupo = case_match(as.character(agua_procedencia),
                            "1" ~ "Red Pública (Vivienda)",
                            c("2", "3") ~ "Red Pública (Fuera/Pilón)",
                            c("4", "5", "6", "7", "8") ~ "No Red (Cisterna/Pozo/Río)",
                            .default = "Otros"),
    
    edu_grupo = case_when(
      edu_nivel_max %in% c("0", "1", "2", "3", "4") ~ "Bajo (Hasta Primaria)",
      edu_nivel_max %in% c("5", "6")                ~ "Medio (Secundaria)",
      edu_nivel_max %in% c("7", "8", "9", "10", "11") ~ "Alto (Superior)",
      TRUE ~ "Sin especificar"
    ),
    
    num_servicios = (electricidad == "1") + (agua_potable == "1") + (desague_tipo == "1"),
    
    num_organizaciones = (p801_1 == 1) + (p801_2 == 2) + (p801_3 == 3) + (p801_4 == 4) +
      (p801_5 == 5) + (p801_6 == 6) + (p801_7 == 7) + (p801_8 == 8) + (p801_9 == 9) +
      (p801_10 == 10) + (p801_11 == 11) + (p801_12 == 12) + (p801_13 == 13) +
      (p801_14 == 14) + (p801_15 == 15) + (p801_16 == 16) + (p801_17 == 17) +
      (p801_18 == 18) + (p801_20 == 20)
    # p801_19 ("No participa") queda excluida a propósito
  )

enaho_diseno_biv <- enaho_bivariado %>%
  mutate(factor_07 = as.numeric(gsub(",", ".", factor_07))) %>%
  filter(!is.na(factor_07)) %>%  
  as_survey_design(ids = conglome, strata = estrato, weights = factor_07, nest = TRUE)

# —-------------------------------------------------------------------------------------------------------------
# 7.1 Análisis Bivariado (2 categóricas) ---
# Las variables utilizadas son
# - Grupo/Nivel educativo
# - Procedencia del agua

# 7.1.1 Tabla de distribución de la población según grupo educativo y la procedencia del agua en su hogar
tabla_biv1 <- enaho_diseno_biv %>%
  group_by(edu_grupo, agua_grupo) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  group_by(edu_grupo) %>%
  mutate(Porcentaje = (Poblacion / sum(Poblacion)) * 100,
         Celda = paste0(comma(round(Poblacion, 0)), " (", round(Porcentaje, 1), "%)")) %>%
  select(edu_grupo, agua_grupo, Celda) %>%
  pivot_wider(names_from = agua_grupo, values_from = Celda)

ft_biv1 <- flextable(tabla_biv1) %>%
  add_header_lines(values = "Tabla Bivariada 1: Procedencia del agua según nivel educativo") %>% theme_vanilla() %>% autofit()
save_as_image(ft_biv1, path = paste0(ruta_bivariado, "/Tabla_Bivariada_1.png"))

# 7.1.2 Gráfico de distribución de la población según grupo educativo y la procedencia del agua en su hogar
plot_biv1 <- ggplot(enaho_bivariado, aes(x = edu_grupo, fill = agua_grupo, weight = factor_07)) +
  geom_bar(position = "fill") + scale_y_continuous(labels = percent) + scale_fill_brewer(palette = "Set2") +
  labs(title = "Gráfico Bivariado 1: Educación y Abastecimiento de Agua", x = "Nivel Educativo", y = "Proporción", fill = "Agua") + theme_minimal()
ggsave(paste0(ruta_bivariado, "/Grafico_Bivariado_1.png"), plot = plot_biv1, width = 8, height = 5, bg = "white")

# —-------------------------------------------------------------------------------------------------------------
# 7.2 Análisis Bivariado (1 categórica y 1 numérica) ---
# Las variables utilizadas son
# - Grupo/Nivel educativo
# - Acceso a servicios del hogar (luz, agua y desagüe)

# 7.2.1 Tabla de distribución de la población según grupo educativo y la cantidad de recursos básicos del hogar
tabla_biv2 <- enaho_diseno_biv %>%
  group_by(edu_grupo) %>%
  summarise(
    Promedio_Servicios = survey_mean(num_servicios, vartype = NULL)
  ) %>%
  ungroup() %>%
  mutate(Promedio_Servicios = round(Promedio_Servicios, 2))

ft_biv2 <- flextable(tabla_biv2) %>%
  set_header_labels(edu_grupo = "Nivel Educativo", Promedio_Servicios = "Promedio de Servicios Básicos") %>%
  add_header_lines(values = "Tabla Bivariada 2: Número promedio de servicios básicos según nivel educativo") %>%
  theme_vanilla() %>%
  autofit()
save_as_image(ft_biv2, path = paste0(ruta_bivariado, "/Tabla_Bivariada_2.png"))

# 7.2.2 Gráfico de distribución de la población según grupo educativo y la cantidad de recursos básicos del hogar
plot_biv2 <- ggplot(tabla_biv2, aes(x = edu_grupo, y = Promedio_Servicios, fill = edu_grupo)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  labs(title = "Gráfico Bivariado 2: Promedio de servicios básicos por nivel educativo",
       x = "Nivel Educativo", y = "Promedio de servicios") +
  theme_minimal()
ggsave(paste0(ruta_bivariado, "/Grafico_Bivariado_2.png"), plot = plot_biv2, width = 7, height = 5, bg = "white")

# 7.2.3 Gráfico Boxplot de distribución de la población según grupo educativo y la cantidad de recursos básicos del hogar
plot_biv2_box <- ggplot(enaho_bivariado, aes(x = edu_grupo, y = num_servicios, fill = edu_grupo)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.2) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  labs(title = "Gráfico Bivariado 2: Distribución de servicios básicos por nivel educativo",
       x = "Nivel Educativo", y = "N° de servicios básicos") +
  theme_minimal()
ggsave(paste0(ruta_bivariado, "/Grafico_Bivariado_2_Boxplot.png"), plot = plot_biv2_box, width = 7, height = 5, bg = "white")

# —-------------------------------------------------------------------------------------------------------------
# 7.3 Análisis Bivariado (2 numéricas) ---
# Las variables utilizadas son
# - N° de organizaciones a las que se pertenece
# - Acceso a servicios del hogar (luz, agua y desagüe)

# 7.2.1 Tabla de distribución de la población según n°de organizaciones en las que se participa y la cantidad de recursos básicos del hogar
correlacion_ponderada <- cov.wt(enaho_bivariado[, c("num_servicios", "num_organizaciones")], wt = enaho_bivariado$factor_07, cor = TRUE)$cor

tabla_correlacion <- as.data.frame(correlacion_ponderada) %>% 
  rownames_to_column(var = "Variable") %>% 
  mutate(across(where(is.numeric), ~ round(., 3)))

ft_biv3 <- flextable(tabla_correlacion) %>% 
  add_header_lines(values = "Tabla Bivariada 3: Matriz de correlación de Pearson entre servicios y organizaciones") %>% 
  theme_vanilla() %>% 
  autofit()
save_as_image(ft_biv3, path = paste0(ruta_bivariado, "/Tabla_Bivariada_3.png"))

# 7.2.2 Gráfico de distribución de la población según n°de organizaciones en las que se participa y la cantidad de recursos básicos del hogar
plot_biv3 <- ggplot(enaho_bivariado, aes(x = num_servicios, y = num_organizaciones, weight = factor_07)) +
  geom_jitter(alpha = 0.1, color = "#1F4E79", width = 0.2, height = 0.2) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(title = "Gráfico Bivariado 3: Tendencia entre Servicios del Hogar y Participación en Organizaciones", 
       x = "Servicios Básicos (0-3)", y = "Organizaciones (0-19)") + 
  theme_minimal()

ggsave(paste0(ruta_bivariado, "/Grafico_Bivariado_3.png"), plot = plot_biv3, width = 8, height = 5, bg = "white")

# 7.2.3 Gráfico de calor de distribución de la población según n°de organizaciones en las que se participa y la cantidad de recursos básicos del hogar
tabla_heatmap <- enaho_diseno_biv %>%
  group_by(num_servicios, num_organizaciones) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  ungroup()

plot_biv3_heatmap <- ggplot(tabla_heatmap, aes(x = num_servicios, y = num_organizaciones, fill = Poblacion)) +
  geom_tile() +
  scale_fill_viridis_c(option = "mako", labels = comma) +
  labs(title = "Gráfico Bivariado 3: Concentración de casos según servicios y organizaciones",
       x = "Servicios Básicos (0-3)", y = "Organizaciones (0-19)", fill = "Población") +
  theme_minimal()

ggsave(paste0(ruta_bivariado, "/Grafico_Bivariado_3_Heatmap.png"), plot = plot_biv3_heatmap, width = 8, height = 6, bg = "white")

write_parquet(enaho_lbl, "datos/procesados/enaho_explorada.parquet") 

