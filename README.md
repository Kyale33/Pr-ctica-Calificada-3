# PC3: Análisis de Participación Ciudadana y Acceso a servicios básicos usando datos de la ENAHO

**Autoría: Kyara Ronchi**

## Fuente de datos

Se utilizaron los siguientes módulos de la ENAHO:

| Módulo | Contenido |
|---|---|
| 100 | Características de la vivienda |
| 300 | Educación |
| 400 | Salud |
| 800A | Participación ciudadana |

La **unidad de análisis** es el informante del hogar (persona mayor de 18 años que responde el módulo 800A).

El análisis explora la relación entre la participación ciudadana y el acceso a:
- Servicio de agua de calidad
- Servicio de electricidad de calidad
- Servicio de desagüe de calidad
- Servicio de educación
- Servicio de salud

## Librerías utilizadas

| Librería | Uso en el proyecto |
|---|---|
| `usethis` | Automatización y configuración inicial del proyecto (conexión con GitHub) |
| `rio` | Importación y exportación rápida de datos |
| `tidyverse` (`dplyr`, `tidyr`, `ggplot2`, `stringr`, `readr`) | Procesamiento, transformación, limpieza de texto y visualización |
| `janitor` | Limpieza y estandarización de nombres/datos |
| `arrow` | Lectura y escritura de archivos `.parquet` |
| `naniar` | Diagnóstico y visualización de valores perdidos (NAs) |
| `srvyr` / `survey` | Diseño muestral complejo (conglomerados, estratos, factores de expansión) |
| `flextable` | Tablas formateadas para exportación |
| `scales` | Formateo de ejes y etiquetas en gráficos |
| `wCorr` | Correlación de Spearman ponderada por el diseño muestral |
| `labelled` | Inyección de etiquetas y metadatos a nivel de variable |
| `codebook` | Generación del libro de códigos interactivo |
| `dataMaid` | Auditoría automática de calidad de datos y reporte de codebook |
| `here` | Manejo de rutas relativas al proyecto |
| `renv` | Control de versiones de las librerías utilizadas |

## Estructura del directorio

```
├── Análisis Participación Ciudadana.Rproj   # Archivo de inicialización del entorno R
├── datos/
│   ├── crudos/                              # Módulos originales de la ENAHO (100, 300, 400, 800A) sin modificación
│   └── procesados/                          # Bases procesadas en formato .parquet
│       ├── base_final_190626.parquet                # Unión (joins) de los módulos — script 02
│       ├── base_final_0726.parquet                  # Base con id creado y variables extraídas — script 02
│       ├── enaho_acondicionada.parquet              # Selección, renombrado y tratamiento de NAs — script 03
│       ├── enaho_explorada.parquet                  # Base con etiquetas de respuesta insertadas — script 04
│       └── enaho_participacion_codebook.parquet     # Base final documentada, lista para el codebook — script 07
├── scripts/
│   ├── 01_GestionInicial_190626.R           # Creación de carpetas y enlace del proyecto con Git y GitHub
│   ├── 02_UnionBases_190626.R               # Carga, creación de id y unión (joins) de los módulos de la ENAHO
│   ├── 03_Acondicionamiento.R               # Selección, renombrado y diagnóstico/tratamiento de NAs
│   ├── 04_Exploracion.R                     # Etiquetado, diseño muestral y EDA univariado/bivariado
│   ├── 05_Informe_Exploracion_Inicial.Rmd   # Informe descriptivo en R Markdown (a partir del script 04)
│   ├── 06_Clasificacion.R                   # Creación de índices y prueba de correlación
│   └── 07_Documentar.R                      # Metadatos, decisiones metodológicas y generación del codebook
├── outputs/
│   ├── tratamiento_nas/                     # Diagnóstico de valores perdidos — script 03
│   ├── tablas/                              # Tablas descriptivas univariadas (agua, desagüe, alumbrado, educación, salud, organizaciones) — script 04
│   ├── gráficos_univ/                       # Gráficos univariados correspondientes — script 04
│   ├── gráficos_biv/                        # Tablas y gráficos del análisis bivariado — script 04
│   ├── gráficos_clasificados/               # Gráfico de barras a partir de los índices creados — script 06
│   ├── CodeBook_dataMaid.html                       # Codebook generado con el paquete dataMaid — script 07
│   └── CodeBook_codebook.html                       # Codebook generado con el paquete codebook — script 07
├── docs/                                    # Diccionario de datos y ficha técnica de la ENAHO
├── renv/                                    # Entorno aislado de paquetes del proyecto
├── renv.lock                                # Registro exacto de versiones de las librerías
└── .gitignore                               # Exclusión de datos crudos y archivos pesados del repositorio
```

## Flujo de trabajo

A continuación se detallan las principales decisiones y acciones tomadas en cada etapa. Para dudas puntuales, referirse al script correspondiente.

### 1. Extraer
Se descargaron los módulos 100, 300, 400 y 800A de la ENAHO para el periodo 2021-2025 en formato `.csv`, junto con el diccionario de datos y la ficha técnica.

### 2. Gestionar (`01_GestionInicial_190626.R`)
Se creó el R Project, la estructura de carpetas del proyecto y la conexión con Git/GitHub mediante `usethis`. La carpeta `datos/crudos` se mantiene vacía en el repositorio (excluida vía `.gitignore`) para no sobrecargarlo con archivos pesados; este README y los scripts documentan el proceso de forma reproducible. Las versiones de las librerías se gestionan con `renv`.

### 3. Acondicionar (`02_UnionBases_190626.R`, `03_Acondicionamiento.R`)
En el script 02 se cargan los módulos de vivienda, educación, salud y participación ciudadana, se crea un identificador único por hogar/persona y se realiza la unión (*joins*) entre módulos. En el script 03 se seleccionan y renombran las variables relevantes, se diagnostican los valores perdidos (exportado a `outputs/tratamiento_nas`) y se aplica el tratamiento correspondiente. El resultado es la base acondicionada, filtrada solo a los informantes del hogar que respondieron el módulo de participación ciudadana.

### 4. Explorar (`04_Exploracion.R`, `05_Informe_Exploracion_Inicial.Rmd`)
Se cargan la base acondicionada y se etiquetan las opciones de respuesta según el diccionario de datos de la ENAHO. Se activa el diseño muestral (con los factores de expansión correspondientes) y se realiza un análisis exploratorio de datos:
- **Univariado**: vivienda (agua, desagüe, alumbrado), educación, salud y participación.
- **Bivariado**: nivel educativo × acceso al agua; nivel educativo × número de servicios básicos; número de organizaciones × número de servicios básicos.

Las tablas se exportan a `outputs/tablas` y `outputs/gráficos_biv`; los gráficos univariados y bivariados a `outputs/gráficos_univ` y `outputs/gráficos_biv`, respectivamente. Estos productos se usan en el script 05 para redactar el informe descriptivo en R Markdown.

### 5. Clasificar (`06_Clasificacion.R`)
Se construyen dos indicadores aditivos a partir de las variables de participación ciudadana (`p801_1` a `p801_20`):
- **Índice de participación ciudadana**: número de organizaciones en las que participa la persona.
- **Índice de acceso a servicios dignos**: número de servicios básicos con los que cuenta el hogar.

Se activa el diseño muestral, se generan los gráficos correspondientes (`outputs/gráficos_clasificados`) y se realiza una prueba de correlación de Spearman ponderada entre ambos índices, para evaluar la relación entre el acceso a servicios y la participación ciudadana.

### 6. Documentar (`07_Documentar.R`)
Se depura la base final, quedándose solo con las variables relevantes de los scripts de exploración y clasificación. Se inyectan etiquetas descriptivas y la fuente (módulo ENAHO) de cada variable, junto con metadatos sobre las decisiones metodológicas y la lógica de construcción de cada indicador analítico. Con esta información se generan dos libros de códigos:
- `CodeBook_dataMaid`: auditoría de calidad de datos (distribución, NAs, valores atípicos).
- `CodeBook_codebook`: libro de códigos interactivo con frecuencias, tipos y etiquetas.

Ambos se exportan en HTML/Rmd a `outputs/`. El diccionario de datos y la ficha técnica de la ENAHO 2025 se encuentran en `docs/`. Las decisiones metodológicas también quedan registradas en los *commits* de cada script.
