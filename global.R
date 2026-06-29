# =============================================================================
# global.R - Carga de paquetes, datos y funciones (ejecuta una vez al iniciar)
# =============================================================================

# Paquetes ---------------------------------------------------------------
library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(writexl)
library(sf)
library(leaflet)

# Funciones del motor ----------------------------------------------------
scripts <- c(
  "utils.R",
  "00_inamhi_idf.R",
  "00b_exploracion_espacial.R",
  "00c_funciones_soporte.R",
  "01_validation.R",
  "02_huff_method.R",
  "03_scs_method.R",
  "04_alternating_block.R",
  "05_plotting.R",
  "06_export.R",
  "07_custom_method.R"
)

for (s in scripts) {
  source(file.path("scripts", s), local = FALSE)
}

# Datos del sistema -------------------------------------------------------
PARAMETROS_INAMHI <- read.csv("data_sistema/inamhi_parametros.csv",
                               stringsAsFactors = FALSE)
IDTR              <- read.csv("data_sistema/inamhi_idtr.csv",
                               stringsAsFactors = FALSE)
CURVAS_HUFF       <- read.csv("data_sistema/huff_curves_standard.csv",
                               stringsAsFactors = FALSE)
CURVAS_SCS        <- read.csv("data_sistema/scs_curves_standard.csv",
                               stringsAsFactors = FALSE)

ZONAS <- tryCatch(
  sf::st_read("data_sistema/zonas_intensidad.shp", quiet = TRUE),
  error = function(e) {
    warning("No se pudo cargar el shapefile de zonas: ", conditionMessage(e))
    NULL
  }
)

ESTACIONES_SF <- sf::st_as_sf(IDTR, coords = c("X", "Y"), crs = 32717)  # UTM 17S para cálculos

# Constantes de UI -------------------------------------------------------
TR_OPCIONES     <- c(2, 5, 10, 25, 50, 100)
ZONAS_CHOICES   <- setNames(1:72, paste("Zona", 1:72))
METODOS_CHOICES <- c(
  "Huff"                = "huff",
  "SCS"                 = "scs",
  "Bloque Alterno"      = "bloque_alterno",
  "Curva personalizada" = "personalizado"
)
HUFF_CUARTILES <- c(
  "Q1 — t < 6 h"        = 1,
  "Q2 — 6 ≤ t < 12 h"  = 2,
  "Q3 — 12 ≤ t < 24 h" = 3,
  "Q4 — t ≥ 24 h"      = 4
)
HUFF_PROB  <- c("10%" = 10, "50%" = 50, "90%" = 90)
SCS_TIPOS  <- c("Tipo I" = "I", "Tipo IA" = "IA", "Tipo II" = "II", "Tipo III" = "III")

huff_cuartil_para_duracion <- function(dur_horas) {
  if      (dur_horas <  6) 1L
  else if (dur_horas < 12) 2L
  else if (dur_horas < 24) 3L
  else                     4L
}
