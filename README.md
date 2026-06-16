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

## Estructura de carpetas
Existen 4 carpetas principales:
1. **Datos:** En donde se encuentran todos los datos divididos en 2 subcarpetas: 
   * `datos/crudos`: Contiene los archivos originales directamente descargados de la base de datos de la ENAHO (Módulos 100, 300, 400 y 800) sin ninguna modificación.
   * `datos/procesados`: Almacena la base de datos unificada final (`base_final`), ya limpia y con las variables combinadas
2. **Outputs**: En donde se guardan los productos gráficos del análisis
3. **Docs:** La carpeta de documentación teórica y metodológica del proyecto
4. **Scripts:** los archivos de R en los que se procesa la data
