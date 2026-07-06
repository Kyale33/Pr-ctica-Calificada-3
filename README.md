# PC3: Análisis de Participación Ciudadana y Acceso a servicios básicos usando datos de la ENAHO
**Autoría: Kyara Ronchi**
Este proyecto incluye el código y el flujo de trabajo estadístico inicial para el "Análisis de Participación Ciudadana y Acceso a servicios básicos usando datos de la ENAHO" en el marco de la Práctica Calificada 3 del curso de Taller de Procesamiento de Datos 2026-1. Para este, se han utilizado distintos módulos de la Encuesta Nacional de Hogares con los resultados entre 2021-2025.

Los módulos utilizados son los siguientes:
- Módulo 100: Características de la vivienda
- Módulo 300: Educación
- Módulo 400: Salud
- Módulo 800A y 800B: Participación Ciudadana

La unidad de análisis del presente proyecto es el informante del hogar, quien mayoritariamente responde al módulo 800 pues las preguntas se realizan a personas mayores de 18 años.

El análisis explora la relación entre la participación ciudadana (en las diversas iniciativas comunitarias) y las siguientes dimensiones:
* Acceso al servicio de agua de calidad
* Acceso al servicio de electricidad de calidad
* Acceso al servicio de desagüe de calidad
* Acceso al servicio de educación
* Acceso al servicio de salud

## Librerías utilizadas
* **Usethis**: para la automatización y configuración inicial del proyecto en GitHub
* **Rio**: para la importación y exportación de datos de manera rápida y eficiente
* **Tidyverse**: para el procesamiento y el análisis de los datos
* **Janitor**: para la limpieza y legibilidad de los datos
* **Readr**: para la lectura de los datos en csv separados por comas
* **Dplyr**: para filtrar y mutar la data
* **Tydir**: para la reestructuración de los datos
* **Ggplot2**: para la realización de gráficos
* **Stringr**: para remover los espacios en blanco
* **Renv**: para guardar la versión de las librerías con las que se trabaja

## Estructura del directorio

El directorio se organiza a través de la siguiente estructura de carpetas:

├── Análisis Participación Ciudadana.Rproj    # Archivo de inicialización del entorno R

├── datos/                                    # No se incluyen los datos crudos en este repositorio debido a su peso

│   ├── crudos/                                       # Módulos originales de la ENAHO (100, 300, 400 y 800) sin modificación

│   └── procesados/                                   # Bases procesadas en formato .parquet

│       ├── base_final_190626.parquet                 # Base resultado de la unión (joins) de los módulos, script 02

│       ├── base_final_0726.parquet                    # Base final con id creado y variables extraídas, script 02

│       ├── enaho_acondicionada.parquet                # Base con selección, renombrado y tratamiento de NAs, script 03

│       └── enaho_explorada.parquet                    # Base con etiquetas de respuesta insertadas, script 04

├── scripts/

│   ├── 01_GestionIncial_190626.R              # Creación de carpetas y enlace del proyecto con Git y GitHub

│   ├── 02_UnionBases_190626.R                 # Carga, creación de id y unión (joins) de los módulos de la ENAHO

│   ├── 03_Acondicionamiento.R                  # Selección, renombrado y diagnóstico/tratamiento de valores perdidos (NAs)

│   ├── 04_Exploracion.R                        # Definición de etiquetas, diseño muestral y EDA univariado y bivariado (tablas y gráficos exportados)

│   ├── 05_Informe_Exploración_Inicial.Rmd      # Informe descriptivo en RMarkdown a partir de las tablas y gráficos del script 04

│   └── 06_Clasificacion.R                      # Creación de índices (participación ciudadana, acceso a servicios) y prueba de correlación

├── outputs/                                            # Outputs finales generados por los scripts del proyecto

│   ├── tratamiento_nas/                                # Gráfico de diagnóstico de valores perdidos (exportación del script 03)

│   ├── tablas/                                         # Tablas descriptivas univariadas: agua, desagüe, alumbrado, educación, salud y organizaciones (script 04)

│   ├── gráficos_univ/                                  # Gráficos univariados correspondientes a las tablas anteriores (script 04)

│   ├── gráficos_biv/                                   # Tablas y gráficos del análisis bivariado (script 04)

│   └── gráficos_clasificados/                          # Gráfico de barras a partir de los índices creados (script 06)

├── docs/                                               # Documentos técnicos de la ENAHO

├── renv/                                               # Carpeta aislada del entorno local de paquetes

├── renv.lock                                           # Registro exacto de las versiones de las librerías

└── .gitignore                                          # Configuración de exclusión para evitar la subida de datos masivos al repositorio

A continuación, se detalla las principales decisiones y acciones tomadas en cada paso del flujo de trabajo. Si se tienen dudas más específicas, por favor, referirse al script en concreto.

# EXTRAER
Se descargó los módulos 100, 300, 400 y 800 (A y B) de la Encuesta Nacional de Hogares para el periodo 2021-2025. Se guardó las bases de datos (.csv) en la carpeta correspondiente, así como el diccionario y la ficha técnica.

# GESTIONAR
En el script 01, se creó un R.project con el título del trabajo, se generó la estructura de carpetas presentada en la sección anterior y se realizó la conexión con Git y GitHub desde RStudio mediante el paquete usethis. Debe tenerse en cuenta que, en este repositorio, la carpeta "datos/crudos" está vacía puesto que se evitó subir las bases de datos originales para no sobrecargar el repositorio debido a su peso; esto se especificó en el archivo ".gitignore". No obstante, el presente README detalla los módulos utilizados y cada script permite reproducir el procesamiento. Finalmente, se utiliza el paquete renv para gestionar las versiones de las librerías empleadas.

# ACONDICIONAR
En el script 02, se cargan los módulos de vivienda, educación, salud y participación ciudadana (específicamente el submódulo A), se crea un identificador único por hogar/persona y se realiza la unión (joins) entre los módulos, dando como resultado las primeras bases de datos procesadas. En el script 03, se seleccionan y renombran las variables de interés relevantes para el análisis, se realiza un diagnóstico de valores perdidos (exportado como gráfico en "outputs/tratamiento_nas") y se aplica el tratamiento correspondiente a las variables con NAs. Como resultado, se exportó la base de datos acondicionada, filtrando únicamente a los informantes del hogar que respondieron el módulo de participación ciudadana.

# EXPLORAR
En el script 04, se carga la base acondicionada y, de manera previa a la creación de tablas y gráficos, se definen las etiquetas de las opciones de respuesta de las variables de interés, guiándose del diccionario de datos de la ENAHO. Se activa el diseño muestral (utilizando los factores de expansión correspondientes) y se realiza un análisis exploratorio de datos (EDA) univariado —sobre vivienda (agua, desagüe, alumbrado), educación, salud y participación— y bivariado —cruzando el nivel educativo con el acceso al agua, el nivel educativo con el número de servicios básicos, y el número de organizaciones con el número de servicios básicos—. Las tablas resultantes se exportan a "outputs/tablas" y "outputs/gráficos_biv", y los gráficos univariados y bivariados a "outputs/gráficos_univ" y "outputs/gráficos_biv", respectivamente. Estos productos son utilizados en el script 05, donde se redacta el informe descriptivo de los datos en formato RMarkdown.

# CLASIFICAR
En el script 06, se construyen indicadores aditivos a partir de las variables de participación ciudadana (p801_1 a p801_20): un índice de participación ciudadana (número de organizaciones en las que participa la persona) y un índice de acceso a servicios dignos (número de servicios básicos con los que cuenta el hogar). Se activa el diseño muestral para un análisis consistente con los factores de expansión, se generan los gráficos correspondientes (exportados a "outputs/gráficos_clasificados") y se realiza una prueba de correlación de Pearson entre el número de servicios básicos y el número de organizaciones, con el objetivo de evaluar la relación entre el acceso a servicios y la participación ciudadana.
