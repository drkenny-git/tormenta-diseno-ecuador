# =============================================================================
# 00_setup.R - Configuración inicial y carga de librerías
# =============================================================================

#' Instalar y cargar paquete si es necesario
#'
#' @param pkg Nombre del paquete
#' @param silent Si TRUE, no muestra mensajes
instalar_y_cargar <- function(pkg, silent = FALSE) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (!silent) cat(paste("Instalando paquete:", pkg, "\n"))
    install.packages(pkg, quiet = silent)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  if (!silent) cat(paste("✓", pkg, "\n"))
}

#' Cargar todas las librerías necesarias
#'
#' @param incluir_espaciales Si TRUE, carga paquetes espaciales (sf, leaflet, etc.)
#' @param silent Si TRUE, no muestra mensajes
cargar_librerias <- function(incluir_espaciales = TRUE, silent = FALSE) {
  
  if (!silent) {
    cat("\n==============================================================\n")
    cat("  CARGANDO LIBRERÍAS\n")
    cat("==============================================================\n\n")
  }
  
  # Librerías básicas
  if (!silent) cat("Librerías básicas:\n")
  instalar_y_cargar("ggplot2", silent)
  instalar_y_cargar("writexl", silent)
  
  # Librerías espaciales (solo si se necesitan)
  if (incluir_espaciales) {
    if (!silent) cat("\nLibrerías espaciales:\n")
    instalar_y_cargar("sf", silent)
    instalar_y_cargar("leaflet", silent)
    instalar_y_cargar("htmlwidgets", silent)
    instalar_y_cargar("lwgeom", silent)
  }
  
  if (!silent) {
    cat("\n✓ Todas las librerías cargadas exitosamente\n")
  }
}

#' Cargar todas las funciones del proyecto
#'
#' @param ruta_scripts Ruta a la carpeta de scripts
cargar_funciones <- function(ruta_scripts = "scripts") {
  
  cat("\n==============================================================\n")
  cat("  CARGANDO FUNCIONES\n")
  cat("==============================================================\n\n")
  
  # Lista de scripts en orden
  scripts <- c(
    "00_inamhi_idf.R",
    "00b_exploracion_espacial.R",
    "utils.R",
    "01_validation.R",
    "02_huff_method.R",
    "03_scs_method.R",
    "04_alternating_block.R",
    "05_plotting.R",
    "06_export.R"
  )
  
  for (script in scripts) {
    ruta <- file.path(ruta_scripts, script)
    if (file.exists(ruta)) {
      source(ruta)
      cat(paste("✓", script, "\n"))
    } else {
      cat(paste("⚠ No encontrado:", script, "\n"))
    }
  }
  
  cat("\n✓ Todas las funciones cargadas\n")
}

#' Cargar datos del sistema (zonas, estaciones, parámetros)
#'
#' @param ruta_datos Ruta a la carpeta de datos del sistema
#' @return Lista con todos los datos cargados
cargar_datos_sistema <- function(ruta_datos = "data_sistema") {
  
  cat("\n==============================================================\n")
  cat("  CARGANDO DATOS DEL SISTEMA\n")
  cat("==============================================================\n\n")
  
  datos <- list()
  
  # Cargar parámetros INAMHI
  ruta_params <- file.path(ruta_datos, "inamhi_parametros.csv")
  if (file.exists(ruta_params)) {
    datos$parametros_inamhi <- read.csv(ruta_params, stringsAsFactors = FALSE)
    cat("✓ Parámetros INAMHI:", nrow(datos$parametros_inamhi), "ecuaciones\n")
  } else {
    warning("No se encontró inamhi_parametros.csv")
  }
  
  # Cargar Idtr
  ruta_idtr <- file.path(ruta_datos, "inamhi_idtr.csv")
  if (file.exists(ruta_idtr)) {
    datos$idtr <- read.csv(ruta_idtr, stringsAsFactors = FALSE)
    cat("✓ Idtr INAMHI:", nrow(datos$idtr), "estaciones\n")
  } else {
    warning("No se encontró inamhi_idtr.csv")
  }
  
  # Cargar curvas Huff
  ruta_huff <- file.path(ruta_datos, "huff_curves_standard.csv")
  if (file.exists(ruta_huff)) {
    datos$curvas_huff <- read.csv(ruta_huff, stringsAsFactors = FALSE)
    cat("✓ Curvas Huff cargadas\n")
  } else {
    warning("No se encontró huff_curves_standard.csv")
  }
  
  # Cargar curvas SCS
  ruta_scs <- file.path(ruta_datos, "scs_curves_standard.csv")
  if (file.exists(ruta_scs)) {
    datos$curvas_scs <- read.csv(ruta_scs, stringsAsFactors = FALSE)
    cat("✓ Curvas SCS cargadas\n")
  } else {
    warning("No se encontró scs_curves_standard.csv")
  }
  
  # Cargar zonas de intensidad (shapefile)
  ruta_zonas <- file.path(ruta_datos, "zonas_intensidad.shp")
  if (file.exists(ruta_zonas)) {
    if (requireNamespace("sf", quietly = TRUE)) {
      datos$zonas <- sf::st_read(ruta_zonas, quiet = TRUE)
      cat("✓ Zonas de intensidad:", nrow(datos$zonas), "zonas\n")
    } else {
      warning("Paquete 'sf' no disponible para cargar zonas")
    }
  } else {
    cat("⚠ No se encontró shapefile de zonas (opcional)\n")
  }
  
  # Crear estaciones espaciales
  if (!is.null(datos$idtr) && requireNamespace("sf", quietly = TRUE)) {
    datos$estaciones_sf <- sf::st_as_sf(
      datos$idtr,
      coords = c("X", "Y"),
      crs = 32717  # UTM 17S
    )
    cat("✓ Estaciones espaciales creadas\n")
  }
  
  cat("\n✓ Datos del sistema cargados\n")
  
  return(datos)
}

#' Inicializar proyecto completo
#'
#' @param incluir_espaciales Cargar paquetes espaciales
#' @param silent Modo silencioso
#' @return Lista con datos del sistema
inicializar_proyecto <- function(incluir_espaciales = TRUE, silent = FALSE) {
  
  # Cargar librerías
  cargar_librerias(incluir_espaciales, silent)
  
  # Cargar funciones
  cargar_funciones()
  
  # Cargar datos del sistema
  datos <- cargar_datos_sistema()
  
  cat("\n")
  cat("==============================================================\n")
  cat("  PROYECTO INICIALIZADO\n")
  cat("==============================================================\n")
  cat("\n")
  
  return(datos)
}
