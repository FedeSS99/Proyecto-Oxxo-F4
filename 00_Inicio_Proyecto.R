# ------------------------------------------------------------------------------
# [DATATHON OXXO]  00_Inicio_Proyecto.R
# 
# Autora    : Semiramis G. de la Cruz
# Revision    : 24.05.2025
# Descripcion : Inicio de proyecto
#               
# ------------------------------------------------------------------------------

rm(list=ls())
cat("\14")
cat("\n")

# ==============================================================================

# RUTA INICIAL (local)
setwd("C:/Users/semiramis/Documents/Me/DatathonOxxo/syntax")
setwd("..")
PATH <- getwd()

# Asegurarse de que la ruta termina con "/"
if(substr(PATH, nchar(PATH), nchar(PATH)) != "/") PATH <- paste0(PATH, "/")
#setwd(PATH)
# DIRECTORIOS (SE AGREGAN DIRECTORIOS PARA SHINY MEMORIA I)
DATADIR    <- paste0(PATH, 'data/')
INPUTDIR   <- paste0(PATH, 'input/')
OUTPUTDIR  <- paste0(PATH, 'output/')
TEMPDIR    <- paste0(PATH, 'temp/')
DOCDIR     <- paste0(PATH, 'doc/')
GRAPHDIR   <- paste0(TEMPDIR, 'graphs/')

SYNTAXDIR  <- paste0(PATH,"syntax/") 
SCRIPTSDIR <- paste0(SYNTAXDIR, 'scripts/')

if (! dir.exists(DATADIR))    { dir.create(DATADIR);    cat('Carpeta creada:', DATADIR,    '\n') }
if (! dir.exists(DOCDIR))     { dir.create(DOCDIR);     cat('Carpeta creada:', DOCDIR,     '\n') }
if (! dir.exists(INPUTDIR))   { dir.create(INPUTDIR);   cat('Carpeta creada:', INPUTDIR,   '\n') }
if (! dir.exists(OUTPUTDIR))  { dir.create(OUTPUTDIR);  cat('Carpeta creada:', OUTPUTDIR,  '\n') }
if (! dir.exists(SYNTAXDIR))  { dir.create(SYNTAXDIR);  cat('Carpeta creada:', SYNTAXDIR,  '\n') }
if (! dir.exists(TEMPDIR))    { dir.create(TEMPDIR);    cat('Carpeta creada:', TEMPDIR,    '\n') }
if (! dir.exists(SCRIPTSDIR)) { dir.create(SCRIPTSDIR); cat('Carpeta creada:', SCRIPTSDIR, '\n') }
# if (! dir.exists(APPSDIR))  	 { dir.create(APPSDIR);  	 cat('Carpeta creada:', APPSDIR,  	'\n') }
# if (! dir.exists(LOGSDIR))  	 { dir.create(LOGSDIR);  	 cat('Carpeta creada:', LOGSDIR,  	'\n') }


# ==============================================================================
# 3. Instalación de y carga de Librerias

# install.packages("tidyverse")
# library(dplyr)
# 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#Poner los paquetes a instalar/cargar en el vector "packages"
packages <- c("Hmisc", "lubridate", "dplyr","tidyverse","janitor","readxl","xlsx","RCT",
              "openxlsx")
ipak(packages)


# ==============================================================================
# 4. Opciones del entorno
options(scipen = 999) #evitamos notación científica
options(stringsAsFactors = FALSE) #evitamos que nos tome las variables como factores


# ==============================================================================
# 5. Carga de funciones
# Obtener la lista de archivos en el directorio
# archivos <- list.files(path = SCRIPTSDIR, pattern = "\\.R$", full.names = TRUE)
# 
# # Iterar sobre cada archivo y ejecutarlo con source
# for (archivo in archivos) {
#   source(archivo)
# }

