# ==============================================================================
# Proyecto: Análisis de la Participación Ciudadana 
# Script: Clasificación e Índices Complejos
# Autor: Kyara Ronchi
# ==============================================================================

# ==============================================================================
# 1. Abrimos las librerías a utilizar ----------------------------------------------------------------------------- 
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
library(wCorr)
library(survey)

# ==============================================================================
# 2. Carga de datos original ---------------------------------------------------------------------------------------
# ==============================================================================

enaho_procesar <- read_parquet("datos/procesados/enaho_explorada.parquet")

# ==============================================================================
# 3. Construcción de indicadores aditivos --------------------------------------------------------------------------
# ==============================================================================
cols_participacion <- paste0("p801_", c(1:18, 20))

enaho_con_indice <- enaho_procesar %>%
  mutate(across(all_of(cols_participacion), ~ ifelse(is.na(.) | . == "NA", "0", as.character(.)))) %>%
  mutate(
    # índice de Participación Ciudadana
    indice_participacion = rowSums(across(all_of(cols_participacion), ~ . != "0")),
    participacion_ciudadana = ifelse(indice_participacion > 0, "Sí", "No"),
    
    # Índice de Acceso a Servicios Dignos
    serv_agua_digna = ifelse(agua_procedencia %in% c("1", "2") & agua_potable == "1" & agua_acceso == "1", 1, 0),
    serv_agua_digna_lbl = ifelse(serv_agua_digna == 1, "Sí", "No"),
    serv_desague_digno = ifelse(desague_tipo %in% c("1", "2"), 1, 0),
    serv_desague_digno_lbl = ifelse(serv_desague_digno == 1, "Sí", "No"),
    serv_energia_digna = ifelse(tipo_alumbrado %in% c("1"), 1, 0),
    serv_energia_digna_lbl = ifelse(serv_energia_digna == 1, "Sí", "No"),
    todos_servicios_dignos = ifelse(serv_agua_digna == 1 & serv_desague_digno == 1 & serv_energia_digna == 1, 1, 0),
    todos_servicios_dignos_lbl = ifelse(todos_servicios_dignos == 1, "Sí", "No"),
    
    serv_salud_seguro = ifelse(
      seguro_essalud == "1" | seguro_sis == "1" | seguro_privado == "1" | 
        seguro_entidad == "1" | seguro_ffa_pnp == "1" | seguro_universitario == "1" | 
        seguro_escolar == "1", 1, 0
    ),
    serv_salud_seguro_lbl = ifelse(serv_salud_seguro == 1, "Sí", "No"),
    seguro_contributivo = ifelse(seguro_essalud == "1" | seguro_privado == "1" | seguro_entidad == "1", 1, 0),
    seguro_contributivo_lbl = ifelse(seguro_contributivo == 1, "Sí", "No"),
    
    edu_secundaria_completa = ifelse(edu_nivel_max %in% c("6", "7", "8", "9", "10", "11"), 1, 0),
    edu_secundaria_completa_lbl = ifelse(edu_secundaria_completa == 1, "Sí", "No"),
    edu_superior_completa = ifelse(edu_nivel_max %in% c("8", "10", "11"), 1, 0),
    edu_superior_completa_lbl = ifelse(edu_superior_completa == 1, "Sí", "No"),
    
    indice_servicios_dignos = serv_agua_digna + serv_desague_digno + serv_energia_digna + 
      serv_salud_seguro + edu_secundaria_completa
  ) %>%
  filter(!is.na(indice_servicios_dignos)) %>%
  mutate(
    indice_servicios_dignos_lbl = case_when(
      indice_servicios_dignos <= 2 ~ "Acceso Bajo (0-2 recursos)",
      indice_servicios_dignos %in% c(3, 4) ~ "Acceso Medio (3-4 recursos)",
      indice_servicios_dignos == 5 ~ "Acceso Alto (Todos los recursos)",
      TRUE ~ "Sin especificar"
    ),
    indice_servicios_dignos_lbl = factor(indice_servicios_dignos_lbl, 
                                         levels = c("Acceso Bajo (0-2 recursos)", 
                                                    "Acceso Medio (3-4 recursos)", 
                                                    "Acceso Alto (Todos los recursos)"))
  )

enaho_diseno_recursos <- enaho_con_indice %>%
  filter(!is.na(factor_07)) %>%  
  as_survey_design(ids = conglome, strata = estrato, weights = factor_07, nest = TRUE)

# ==============================================================================
# 4. Activación del diseño muestral para el análisis consistente -------------------------------------------------------
# ==============================================================================
enaho_diseno_recursos <- enaho_con_indice %>%
  filter(!is.na(factor_07)) %>%  
  as_survey_design(ids = conglome, strata = estrato, weights = factor_07, nest = TRUE)

# ==============================================================================
# 5. Gráficos -------------------------------------------------------------------------------------------------------
# ==============================================================================

# Gráfico de participación ciudadana según el acceso a servicios básicos
data_grafico1 <- enaho_diseno_recursos %>%
  group_by(indice_servicios_dignos_lbl, participacion_ciudadana) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  group_by(indice_servicios_dignos_lbl) %>%
  mutate(porcentaje = (Poblacion / sum(Poblacion)) * 100) %>%
  filter(participacion_ciudadana == "Sí") %>%
  as_tibble()

p1 <- ggplot(data_grafico1, aes(x = indice_servicios_dignos_lbl, y = porcentaje, fill = indice_servicios_dignos_lbl)) +
  geom_col(width = 0.5, show.legend = FALSE, color = "black") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), vjust = -0.5, fontface = "bold", size = 5) +
  scale_fill_brewer(palette = "Reds", direction = -1) + 
  labs(
    title = "Tasa de Participación Ciudadana según Nivel de Recursos y Servicios",
    subtitle = "Evidencia nacional: A menor acceso a derechos básicos, la población se organiza más",
    x = "Nivel de Acceso Integral (Servicios + Salud + Educación)",
    y = "Porcentaje que participa en Organizaciones (%)",
    caption = "Fuente: INEI - Encuesta Nacional de Hogares (ENAHO). Datos expandidos ponderados."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 11, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, max(data_grafico1$porcentaje) + 10)

ggsave("outputs/gráficos_clasificados/grafico_barras_participacion.png", plot = p1, width = 9, height = 6, dpi = 300)


# ==============================================================================
# 6. Prueba de Correlación ---------------------------------------------------------------------------------------
# ==============================================================================
rho_ponderado <- weightedCorr(
  x = enaho_con_indice$indice_servicios_dignos,
  y = enaho_con_indice$indice_participacion,
  weights = enaho_con_indice$factor_07,
  method = "Spearman"
)

rho_ponderado <- round(rho_ponderado, 4)
print(rho_ponderado)

# Convertimos el diseño srvyr a un diseño con réplicas bootstrap
diseno_rep <- as.svrepdesign(enaho_diseno_recursos, type = "bootstrap", replicates = 200)

# Función que calcula el rho de Spearman ponderado en cada réplica
calc_rho <- function(w, data) {
  weightedCorr(data$indice_servicios_dignos, data$indice_participacion,
               weights = w, method = "Spearman")
}

resultado_boot <- withReplicates(diseno_rep, calc_rho)
print(resultado_boot)
theta   <- coef(resultado_boot)[1]
se      <- SE(resultado_boot)[1]
z       <- theta / se
p_valor <- 2 * pnorm(-abs(z))

cat("Rho de Spearman ponderado:", round(theta, 4), "\n")
cat("Error estándar:", round(se, 4), "\n")
cat("Estadístico z:", round(z, 4), "\n")
cat("p-valor:", format.pval(p_valor, digits = 4), "\n")
cat("\n========================================================\n")
cat(" RESULTADOS DE LA PRUEBA DE CORRELACIÓN PONDERADA (Spearman)\n")
cat("========================================================\n")
cat("Coeficiente de correlación (Rho de Spearman, ponderado):", round(theta, 4), "\n")
cat("Significancia estadística (p-value):", format.pval(p_valor, digits = 4), "\n")
cat("--------------------------------------------------------\n")

if (p_valor < 0.05) {
  cat("INTERPRETACIÓN: La relación es ESTADÍSTICAMENTE SIGNIFICATIVA (p < 0.05).\n")
  if (theta < 0) {
    cat("Dirección: INVERSA/NEGATIVA. A mayor acceso a servicios y recursos dignos,\n")
    cat("  disminuye de forma sistemática la participación ciudadana en el Perú.\n")
  } else {
    cat("Dirección: DIRECTA/POSITIVA. A mayor acceso, mayor participación.\n")
  }
} else {
  cat("INTERPRETACIÓN: NO hay evidencia estadística de correlación.\n")
}
cat("========================================================\n")

# Intervalo de confianza del modelo
ic_inferior <- theta - 1.96 * se
ic_superior <- theta + 1.96 * se

cat("Rho de Spearman ponderado:", round(theta, 4), "\n")
cat("IC 95%: [", round(ic_inferior, 4), ",", round(ic_superior, 4), "]\n")

