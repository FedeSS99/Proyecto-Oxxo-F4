# ------------------------------------------------------------------------------
# [DATATHON OXXO]  01_Definicion_Muestras.R
# 
# Autora    : Semiramis G. de la Cruz
# Revision    : 24.05.2025
# Descripcion : 
#               
# ------------------------------------------------------------------------------

library(tidygeocoder)
library(stringr)

# LECTURA DATOS ----------------------------------------------------------------

DIM_TIENDA <- read.csv(paste0(DATADIR, "DIM_TIENDA.csv"))
DIM_TIENDA_TEST <- read.csv(paste0(DATADIR, "DIM_TIENDA_TEST.csv")) # Validacion
Venta <- read.csv(paste0(DATADIR, "Venta.csv"))

# REVISION DE VARIABLES --------------------------------------------------------

str(DIM_TIENDA) # 951, 12
str(DIM_TIENDA_TEST) # 105, 12
str(Venta)

# Revisión variables
# TIENDA_ID 
n_distinct(DIM_TIENDA$TIENDA_ID)
# 951

# PLAZA_CVE
table(DIM_TIENDA$PLAZA_CVE, useNA = "ifany")
# 1   2   3   4   5   6 
# 163 208 112 165 130 173

# NIVELSOCIOECONOMICO_DES
table(DIM_TIENDA$NIVELSOCIOECONOMICO_DES, useNA = "ifany")
# A  AB   B  BC   C  CD   D 
# 8  31  92  73 670  61  16 

# ENTORNO_DES
table(DIM_TIENDA$ENTORNO_DES, useNA = "ifany")
# Base    Hogar Peatonal   Receso 
# 228      576        7      140

# MTS2VENTAS_NUM
DIM_TIENDA %>%
  summarise(
    conteo = n(),
    n_missings = sum(is.na(MTS2VENTAS_NUM)),
    media = mean(MTS2VENTAS_NUM, na.rm = TRUE),
    mediana = median(MTS2VENTAS_NUM, na.rm = TRUE),
    minimo = min(MTS2VENTAS_NUM, na.rm = TRUE),
    maximo = max(MTS2VENTAS_NUM, na.rm = TRUE),
    desviacion = sd(MTS2VENTAS_NUM, na.rm = TRUE)
  )
# conteo n_missings    media mediana minimo maximo desviacion
# 1    951          0 78.89746  102.87      0  214.2    51.4123

# PUERTASREFRIG_NUM
table(DIM_TIENDA$PUERTASREFRIG_NUM, useNA = "ifany")
#   0   6   7   8   9  10  11  12  13  14  15  16  17  18 
# 197   3   2  13  12 163  40 159 314  27  17   1   2   1

# CAJONESESTACIONAMIENTO_NUM
DIM_TIENDA %>%
  summarise(
    conteo = n(),
    n_missings = sum(is.na(CAJONESESTACIONAMIENTO_NUM)),
    media = mean(CAJONESESTACIONAMIENTO_NUM, na.rm = TRUE),
    mediana = median(CAJONESESTACIONAMIENTO_NUM, na.rm = TRUE),
    minimo = min(CAJONESESTACIONAMIENTO_NUM, na.rm = TRUE),
    maximo = max(CAJONESESTACIONAMIENTO_NUM, na.rm = TRUE),
    desviacion = sd(CAJONESESTACIONAMIENTO_NUM, na.rm = TRUE)
  )
# conteo n_missings    media mediana minimo maximo desviacion
# 1    951          0 4.023134       0      0     17   4.717436

# SEGMENTO_MAESTRO_DESC
table(DIM_TIENDA$SEGMENTO_MAESTRO_DESC, useNA = "ifany")
#       Barrio Competido          Clásico    Hogar Reunión      Oficinistas 
# 1               27               69              609               96 
# Parada Técnica      <NA> 
#   143                6 

# LID_UBICACION_TIENDA
table(DIM_TIENDA$LID_UBICACION_TIENDA, useNA = "ifany")
# UT_CARRETERA_GAS    UT_DENSIDAD        UT_GAS_URBANA  UT_TRAFICO_PEATONAL 
# 57                  532                  102                   44 
# UT_TRAFICO_VEHICULAR 
# 216 

# DATASET 
table(DIM_TIENDA$DATASET, useNA = "ifany")


# VALIDACION UBICACION ---------------------------------------------------------

procesar_dim_tienda <- function(
    tabla_tiendas,
    tiendas_excluir = NULL,
    nombre_archivo = "DIM_TIENDA_PROCESADA.csv",
    datadir = DATADIR
) {
  library(dplyr)
  library(stringr)
  library(tidygeocoder)
  library(readr)
  
  tabla_filtrada <- tabla_tiendas %>%
    filter(!TIENDA_ID %in% tiendas_excluir) %>%
    mutate(latlon_key = paste0(round(LATITUD_NUM, 6), "_", round(LONGITUD_NUM, 6)))
  
  ubicaciones <- tabla_filtrada %>%
    select(LATITUD_NUM, LONGITUD_NUM) %>%
    distinct()
  
  resultados <- ubicaciones %>%
    reverse_geocode(
      lat = LATITUD_NUM,
      long = LONGITUD_NUM,
      method = "osm",
      address = "direccion"
    )
  
  # Limpieza de resultados
  resultados_limpio <- resultados %>%
    mutate(
      codigo_postal = str_extract(direccion, "\\b\\d{5}\\b"),
      estado = case_when(
        str_detect(direccion, regex("Nuevo León", ignore_case = TRUE)) ~ "Nuevo León",
        str_detect(direccion, regex("Tamaulipas", ignore_case = TRUE)) ~ "Tamaulipas",
        TRUE ~ NA_character_
      ),
      latlon_key = paste0(round(LATITUD_NUM, 6), "_", round(LONGITUD_NUM, 6))
    ) %>%
    select(latlon_key, direccion, codigo_postal, estado)
  
  tabla_final <- tabla_filtrada %>%
    left_join(resultados_limpio, by = "latlon_key") %>%
    select(-latlon_key)
  
  write.csv(
    tabla_final,
    paste0(datadir, nombre_archivo),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  return(tabla_final)
}

# BASE LIMPIA ------------------------------------------------------------------

# Para DIM_TIENDA
DIM_TIENDA_V2 <- procesar_dim_tienda(
  tabla_tiendas = DIM_TIENDA,
  tiendas_excluir = c(857, 858, 767, 864, 869),
  nombre_archivo = "DIM_TIENDA_V2.csv",
  datadir = DATADIR
)

# Para DIM_TIENDA_TEST
DIM_TIENDA_TEST_V2 <- procesar_dim_tienda(
  tabla_tiendas = DIM_TIENDA_TEST,
  nombre_archivo = "DIM_TIENDA_TEST_V2.csv",
  datadir = DATADIR)

