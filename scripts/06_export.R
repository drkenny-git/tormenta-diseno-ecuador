# =============================================================================
# 06_export.R - Exportar resultados a Excel
# =============================================================================

#' Exportar hietograma a Excel
#'
#' @param hietograma Data frame con el hietograma
#' @param nombre_archivo Nombre del archivo Excel
#' @param directorio Directorio donde guardar
#' @param nombre_hoja Nombre de la hoja en Excel
exportar_hietograma_excel <- function(hietograma, nombre_archivo, 
                                     directorio = "output/tables",
                                     nombre_hoja = "Hietograma") {
  
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("El paquete 'writexl' es necesario. Instálalo con: install.packages('writexl')")
  }
  
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  ruta_completa <- file.path(directorio, nombre_archivo)
  
  # Preparar datos para exportar
  datos_export <- hietograma[, c("paso", "tiempo_horas", "tiempo_minutos", 
                                 "precip_incr_mm", "precip_acum_mm", "intensidad_mm_h")]
  
  # Renombrar columnas para mejor legibilidad
  colnames(datos_export) <- c(
    "Paso",
    "Tiempo (h)",
    "Tiempo (min)",
    "P incremental (mm)",
    "P acumulada (mm)",
    "Intensidad (mm/h)"
  )
  
  writexl::write_xlsx(list(Hietograma = datos_export), path = ruta_completa)
  
  cat("Hietograma exportado a:", ruta_completa, "\n")
}

#' Exportar múltiples hietogramas a Excel (una hoja por TR)
#'
#' @param lista_hietogramas Lista de data frames con hietogramas
#' @param nombre_archivo Nombre del archivo Excel
#' @param directorio Directorio donde guardar
#' @param metodo Nombre del método (para incluir en nombres de hojas)
exportar_multiples_excel <- function(lista_hietogramas, nombre_archivo,
                                    directorio = "output/tables",
                                    metodo = "Metodo") {
  
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("El paquete 'writexl' es necesario.")
  }
  
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  ruta_completa <- file.path(directorio, nombre_archivo)
  
  # Preparar lista de hojas para Excel
  hojas <- list()
  
  for (nombre_tr in names(lista_hietogramas)) {
    hietograma <- lista_hietogramas[[nombre_tr]]
    
    # Preparar datos
    datos_export <- hietograma[, c("paso", "tiempo_horas", "tiempo_minutos",
                                   "precip_incr_mm", "precip_acum_mm", "intensidad_mm_h")]
    
    colnames(datos_export) <- c(
      "Paso",
      "Tiempo (h)",
      "Tiempo (min)",
      "P incremental (mm)",
      "P acumulada (mm)",
      "Intensidad (mm/h)"
    )
    
    # Usar TR como nombre de hoja
    nombre_hoja <- gsub("_", " ", nombre_tr)
    hojas[[nombre_hoja]] <- datos_export
  }
  
  # Agregar hoja de resumen
  resumen <- crear_hoja_resumen(lista_hietogramas, metodo)
  hojas <- c(list(Resumen = resumen), hojas)
  
  writexl::write_xlsx(hojas, path = ruta_completa)
  
  cat("Hietogramas exportados a:", ruta_completa, "\n")
  cat("Número de hojas:", length(hojas), "\n")
}

#' Crear hoja de resumen con información general
#'
#' @param lista_hietogramas Lista de hietogramas
#' @param metodo Nombre del método
#' @return Data frame con resumen
crear_hoja_resumen <- function(lista_hietogramas, metodo) {
  
  TRs <- numeric()
  P_total <- numeric()
  duraciones <- numeric()
  pasos <- numeric()
  P_max_incr <- numeric()
  I_max <- numeric()
  
  for (nombre_tr in names(lista_hietogramas)) {
    hiet <- lista_hietogramas[[nombre_tr]]
    
    TRs <- c(TRs, unique(hiet$TR)[1])
    P_total <- c(P_total, max(hiet$precip_acum_mm))
    duraciones <- c(duraciones, max(hiet$tiempo_horas))
    pasos <- c(pasos, max(hiet$paso))
    P_max_incr <- c(P_max_incr, max(hiet$precip_incr_mm))
    I_max <- c(I_max, max(hiet$intensidad_mm_h))
  }
  
  resumen <- data.frame(
    Metodo = metodo,
    `TR (años)` = TRs,
    `P total (mm)` = round(P_total, 2),
    `Duracion (h)` = duraciones,
    `Num pasos` = pasos,
    `P incr max (mm)` = round(P_max_incr, 2),
    `I max (mm/h)` = round(I_max, 2),
    check.names = FALSE
  )
  
  return(resumen)
}

#' Exportar comparación de métodos a Excel
#'
#' @param lista_huff Lista de hietogramas Huff
#' @param lista_scs Lista de hietogramas SCS
#' @param nombre_archivo Nombre del archivo
#' @param directorio Directorio donde guardar
exportar_comparacion_excel <- function(lista_huff, lista_scs, nombre_archivo,
                                      directorio = "output/tables") {
  
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("El paquete 'writexl' es necesario.")
  }
  
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  ruta_completa <- file.path(directorio, nombre_archivo)
  
  hojas <- list()
  
  # Resumen Huff
  resumen_huff <- crear_hoja_resumen(lista_huff, "Huff")
  hojas[["Resumen Huff"]] <- resumen_huff
  
  # Resumen SCS
  resumen_scs <- crear_hoja_resumen(lista_scs, "SCS")
  hojas[["Resumen SCS"]] <- resumen_scs
  
  # Comparación lado a lado para cada TR
  TRs_comunes <- intersect(names(lista_huff), names(lista_scs))
  
  for (nombre_tr in TRs_comunes) {
    huff <- lista_huff[[nombre_tr]]
    scs <- lista_scs[[nombre_tr]]
    
    # Crear data frame comparativo
    comparacion <- data.frame(
      Paso = huff$paso,
      `Tiempo (h)` = huff$tiempo_horas,
      `Huff P incr (mm)` = huff$precip_incr_mm,
      `Huff P acum (mm)` = huff$precip_acum_mm,
      `SCS P incr (mm)` = scs$precip_incr_mm,
      `SCS P acum (mm)` = scs$precip_acum_mm,
      check.names = FALSE
    )
    
    nombre_hoja <- paste("Comp", gsub("_", " ", nombre_tr))
    hojas[[nombre_hoja]] <- comparacion
  }
  
  writexl::write_xlsx(hojas, path = ruta_completa)
  
  cat("Comparación exportada a:", ruta_completa, "\n")
}

#' Crear archivo Excel con formato mejorado (requiere openxlsx)
#'
#' @param lista_hietogramas Lista de hietogramas
#' @param nombre_archivo Nombre del archivo
#' @param directorio Directorio donde guardar
#' @param metodo Nombre del método
exportar_excel_formateado <- function(lista_hietogramas, nombre_archivo,
                                     directorio = "output/tables",
                                     metodo = "Metodo") {
  
  # Intentar usar openxlsx si está disponible
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    cat("Usando openxlsx para formato mejorado...\n")
    exportar_con_openxlsx(lista_hietogramas, nombre_archivo, directorio, metodo)
  } else {
    cat("openxlsx no disponible. Usando writexl (sin formato)...\n")
    cat("Instala openxlsx para formato mejorado: install.packages('openxlsx')\n")
    exportar_multiples_excel(lista_hietogramas, nombre_archivo, directorio, metodo)
  }
}

#' Exportar con openxlsx (formato mejorado)
#' @keywords internal
exportar_con_openxlsx <- function(lista_hietogramas, nombre_archivo, directorio, metodo) {
  
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  ruta_completa <- file.path(directorio, nombre_archivo)
  
  wb <- openxlsx::createWorkbook()
  
  # Estilos
  headerStyle <- openxlsx::createStyle(
    fontSize = 11,
    fontColour = "#FFFFFF",
    fgFill = "#4F81BD",
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    border = "TopBottomLeftRight"
  )
  
  # Agregar hojas
  for (nombre_tr in names(lista_hietogramas)) {
    hietograma <- lista_hietogramas[[nombre_tr]]
    
    datos_export <- hietograma[, c("paso", "tiempo_horas", "tiempo_minutos",
                                   "precip_incr_mm", "precip_acum_mm", "intensidad_mm_h")]
    
    colnames(datos_export) <- c("Paso", "Tiempo (h)", "Tiempo (min)",
                                "P incremental (mm)", "P acumulada (mm)", "Intensidad (mm/h)")
    
    nombre_hoja <- gsub("_", " ", nombre_tr)
    openxlsx::addWorksheet(wb, nombre_hoja)
    openxlsx::writeData(wb, nombre_hoja, datos_export, startRow = 1, headerStyle = headerStyle)
    openxlsx::addStyle(wb, nombre_hoja, headerStyle, rows = 1, cols = 1:ncol(datos_export))
    openxlsx::setColWidths(wb, nombre_hoja, cols = 1:ncol(datos_export), widths = "auto")
  }
  
  openxlsx::saveWorkbook(wb, ruta_completa, overwrite = TRUE)
  
  cat("Excel formateado guardado en:", ruta_completa, "\n")
}
