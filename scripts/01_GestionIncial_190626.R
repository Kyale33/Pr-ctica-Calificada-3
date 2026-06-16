#===========================================================================
# Proyecto: Análisis de la participación ciudadana según el acceso a servicios básicos
# Autor: Kyara Ronchi
# Fecha: 19-06
# Objetivo del script: Creación de carpetas y enlace con GitHub
#===========================================================================

# 1. Creamos el R Project---------------------------------------------------
Se creó el R Project llamado "Análisis participación ciudadana" y se generó este primer script titulado "Gestión inicial"

# 2. Creamos las carpetas---------------------------------------------------
dir.create("datos")
dir.create("datos/crudos")
dir.create("datos/procesados")
dir.create("outputs")
dir.create("docs")
dir.create("scripts")

# 3. Instalamos las librerías necesarias-------------------------------------
install.packages(c("stringi", "rlang", "glue", "gert", "usethis"), dependencies = TRUE)

# 4. Enlazamos con Git y GitHub----------------------------------------------
usethis::use_git_config(
  user.name = "Kyale33", 
  user.email = "ronchi.kyara@pucp.edu.pe"
)
usethis::create_github_token()
gitcreds::gitcreds_set()
usethis::use_git()
usethis::use_github()
