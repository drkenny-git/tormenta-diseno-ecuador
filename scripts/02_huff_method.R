# =============================================================================
# 02_huff_method.R - Método de curvas de Huff
# =============================================================================

#' Cargar curvas de Huff desde archivo CSV
#'
#' @param ruta_archivo Ruta al archivo CSV con las curvas de Huff
#' @return Data frame con las curvas de Huff
cargar_curvas_huff <- function(ruta_archivo = "data_sistema/huff_curves_standard.csv") {
  if (!file.exists(ruta_archivo)) {
    stop("No se encuentra el archivo de curvas de Huff: ", ruta_archivo)
  }
  
  curvas <- read.csv(ruta_archivo, stringsAsFactors = FALSE)
  return(curvas)
}

#' Obtener nombre de columna para cuartil y probabilidad específicos
#'
#' @param cuartil Número de cuartil (1-4)
#' @param probabilidad Probabilidad (10, 50, 90)
#' @return Nombre de la columna
obtener_columna_huff <- function(cuartil, probabilidad) {
  paste0("Q", cuartil, "_P", probabilidad)
}

#' Calcular hietograma usando método de Huff
#'
#' @param precip_total Precipitación total (mm)
#' @param duracion_horas Duración total (horas)
#' @param paso_minutos Paso de tiempo (minutos)
#' @param cuartil Número de cuartil (1-4)
#' @param probabilidad Probabilidad (10, 50, 90)
#' @param curvas_huff Data frame con curvas de Huff (opcional)
#' @return Data frame con el hietograma
calcular_hietograma_huff <- function(precip_total, duracion_horas, paso_minutos, 
                                     cuartil, probabilidad, curvas_huff = NULL) {
  
  # Cargar curvas si no se proporcionan
  if (is.null(curvas_huff)) {
    curvas_huff <- cargar_curvas_huff()
  }
  
  # Validar parámetros específicos de Huff
  validacion <- validar_parametros_huff(cuartil, probabilidad)
  if (!validacion$valido) {
    stop("Parámetros de Huff inválidos: ", paste(validacion$errores, collapse = "; "))
  }
  
  # Crear tiempo normalizado para el paso especificado
  tiempo_norm <- crear_tiempo_normalizado(duracion_horas, paso_minutos)
  
  # Obtener la columna correspondiente
  col_name <- obtener_columna_huff(cuartil, probabilidad)
  
  # Interpolar precipitación acumulada en porcentaje
  precip_acum_pct <- interpolar_lineal(
    x_known = curvas_huff$time_fraction,
    y_known = curvas_huff[[col_name]],
    x_new = tiempo_norm
  )
  
  # Convertir a mm
  precip_acum_mm <- precip_acum_pct * precip_total / 100
  
  # Calcular precipitación incremental
  precip_incr_mm <- acumulada_a_incremental(precip_acum_mm)
  
  # Calcular tiempo en horas y minutos
  tiempo_horas <- tiempo_norm_a_horas(tiempo_norm, duracion_horas)
  tiempo_minutos <- tiempo_horas * 60
  
  # Crear data frame resultado
  resultado <- data.frame(
    paso = seq_along(tiempo_norm) - 1,
    tiempo_norm = tiempo_norm,
    tiempo_horas = tiempo_horas,
    tiempo_minutos = tiempo_minutos,
    precip_acum_frac = precip_acum_pct / 100,  # Fracción normalizada (0-1)
    precip_acum_mm = precip_acum_mm,
    precip_incr_mm = precip_incr_mm,
    intensidad_mm_h = precip_incr_mm / (paso_minutos / 60)
  )
  
  return(resultado)
}

#' Calcular hietogramas de Huff para múltiples TRs
#'
#' @param precip_total Vector de precipitación total por TR
#' @param TR Vector de periodos de retorno
#' @param duracion_horas Duración total
#' @param paso_minutos Paso de tiempo
#' @param cuartil Número de cuartil
#' @param probabilidad Probabilidad
#' @return Lista de data frames, uno por cada TR
calcular_multiples_huff <- function(precip_total, TR, duracion_horas, paso_minutos,
                                    cuartil, probabilidad) {
  
  curvas_huff <- cargar_curvas_huff()
  
  resultados <- list()
  
  for (i in seq_along(TR)) {
    cat(sprintf("Calculando hietograma Huff para TR = %d años...\n", TR[i]))
    
    hietograma <- calcular_hietograma_huff(
      precip_total = precip_total[i],
      duracion_horas = duracion_horas,
      paso_minutos = paso_minutos,
      cuartil = cuartil,
      probabilidad = probabilidad,
      curvas_huff = curvas_huff
    )
    
    hietograma$TR <- TR[i]
    resultados[[paste0("TR_", TR[i])]] <- hietograma
  }
  
  return(resultados)
}

#' Recomendar cuartil de Huff según duración
#'
#' @param duracion_horas Duración de la tormenta en horas
#' @return Lista con cuartil recomendado y explicación
recomendar_cuartil_huff <- function(duracion_horas) {
  if (duracion_horas <= 6) {
    return(list(
      cuartil = 1,
      explicacion = "Cuartil 1 recomendado para duraciones ≤ 6 horas (37% de tormentas)"
    ))
  } else if (duracion_horas <= 12) {
    return(list(
      cuartil = 2,
      explicacion = "Cuartil 2 recomendado para duraciones 6-12 horas (27% de tormentas)"
    ))
  } else if (duracion_horas <= 24) {
    return(list(
      cuartil = 3,
      explicacion = "Cuartil 3 recomendado para duraciones 12-24 horas (21% de tormentas)"
    ))
  } else {
    return(list(
      cuartil = 4,
      explicacion = "Cuartil 4 recomendado para duraciones > 24 horas (15% de tormentas)"
    ))
  }
}

#' Mostrar recomendaciones y permitir selección manual o automática
#'
#' @param duracion_horas Duración de la tormenta
#' @param cuartil_manual Cuartil seleccionado manualmente (NULL para usar recomendación)
#' @param probabilidad_manual Probabilidad seleccionada manualmente (NULL para usar 50)
#' @param tipo_scs_manual Tipo SCS seleccionado manualmente (NULL para usar "II")
#' @return Lista con parámetros seleccionados
aplicar_recomendaciones <- function(duracion_horas, 
                                   cuartil_manual = NULL, 
                                   probabilidad_manual = NULL,
                                   tipo_scs_manual = NULL) {
  
  cat("\n--- RECOMENDACIONES DE PARÁMETROS ---\n\n")
  
  # Recomendación Huff - Cuartil
  rec_huff <- recomendar_cuartil_huff(duracion_horas)
  cat("MÉTODO HUFF:\n")
  cat("  Recomendación:", rec_huff$explicacion, "\n")
  
  if (is.null(cuartil_manual)) {
    cuartil_final <- rec_huff$cuartil
    cat("  ✓ Usando cuartil recomendado:", cuartil_final, "\n")
  } else {
    cuartil_final <- cuartil_manual
    cat("  ⚠ Usando cuartil manual:", cuartil_final, "\n")
    if (cuartil_final != rec_huff$cuartil) {
      cat("    (Difiere de la recomendación:", rec_huff$cuartil, ")\n")
    }
  }
  
  # Recomendación Huff - Probabilidad
  if (is.null(probabilidad_manual)) {
    probabilidad_final <- 50
    cat("  ✓ Usando probabilidad por defecto: 50% (mediana)\n")
  } else {
    probabilidad_final <- probabilidad_manual
    cat("  ⚠ Usando probabilidad manual:", probabilidad_final, "%\n")
  }
  
  # Recomendación SCS
  cat("\nMÉTODO SCS:\n")
  cat("  Recomendación: Tipo II (más común, tormentas convectivas)\n")
  
  if (is.null(tipo_scs_manual)) {
    tipo_scs_final <- "II"
    cat("  ✓ Usando tipo recomendado: II\n")
  } else {
    tipo_scs_final <- tipo_scs_manual
    cat("  ⚠ Usando tipo manual:", tipo_scs_final, "\n")
  }
  
  cat("\n" , rep("=", 40), "\n\n", sep = "")
  
  return(list(
    cuartil_huff = cuartil_final,
    probabilidad_huff = probabilidad_final,
    tipo_scs = tipo_scs_final
  ))
}
