# =============================================================================
# 01_validation.R - Validación de parámetros de entrada
# =============================================================================

#' Validar parámetros de entrada para generación de hietogramas
#'
#' @param precip_total Vector de precipitación total por TR (mm)
#' @param TR Vector de periodos de retorno (años)
#' @param duracion_horas Duración total de la tormenta (horas)
#' @param paso_minutos Paso de tiempo (minutos)
#' @return Lista con status y mensajes de error (si los hay)
validar_parametros <- function(precip_total, TR, duracion_horas, paso_minutos) {
  errores <- character()
  
  # Validar que los vectores tengan la misma longitud
  if (length(precip_total) != length(TR)) {
    errores <- c(errores, "Los vectores 'precip_total' y 'TR' deben tener la misma longitud")
  }
  
  # Validar precipitación total
  if (any(is.na(precip_total))) {
    errores <- c(errores, "El vector 'precip_total' contiene valores NA")
  }
  if (any(precip_total <= 0)) {
    errores <- c(errores, "Todos los valores de 'precip_total' deben ser positivos")
  }
  if (any(precip_total > 1000)) {
    errores <- c(errores, "Advertencia: Valores de precipitación muy altos (>1000 mm)")
  }
  
  # Validar periodos de retorno
  if (any(is.na(TR))) {
    errores <- c(errores, "El vector 'TR' contiene valores NA")
  }
  if (any(TR <= 0)) {
    errores <- c(errores, "Todos los valores de 'TR' deben ser positivos")
  }
  if (any(TR > 10000)) {
    errores <- c(errores, "Advertencia: Periodos de retorno muy altos (>10000 años)")
  }
  
  # Validar duración
  if (is.na(duracion_horas) || duracion_horas <= 0) {
    errores <- c(errores, "La duración debe ser un valor positivo")
  }
  if (duracion_horas > 72) {
    errores <- c(errores, "Advertencia: Duración muy larga (>72 horas)")
  }
  
  # Validar paso de tiempo
  if (is.na(paso_minutos) || paso_minutos <= 0) {
    errores <- c(errores, "El paso de tiempo debe ser un valor positivo")
  }
  if (paso_minutos >= duracion_horas * 60) {
    errores <- c(errores, "El paso de tiempo debe ser menor que la duración total")
  }
  if ((duracion_horas * 60) %% paso_minutos != 0) {
    errores <- c(errores, "Advertencia: La duración total no es divisible exactamente por el paso de tiempo")
  }
  
  # Retornar resultado
  if (length(errores) > 0) {
    return(list(valido = FALSE, errores = errores))
  } else {
    return(list(valido = TRUE, errores = NULL))
  }
}

#' Validar método seleccionado
#'
#' @param metodo String con el método ("huff", "scs", "bloque_alterno")
#' @return Lista con status y mensaje de error
validar_metodo <- function(metodo) {
  metodos_validos <- c("huff", "scs", "bloque_alterno")
  
  if (!(metodo %in% metodos_validos)) {
    return(list(
      valido = FALSE, 
      error = paste("Método no válido. Debe ser uno de:", paste(metodos_validos, collapse = ", "))
    ))
  }
  
  return(list(valido = TRUE, error = NULL))
}

#' Validar parámetros específicos del método Huff
#'
#' @param cuartil Número de cuartil (1, 2, 3, 4)
#' @param probabilidad Probabilidad (10, 50, 90)
#' @return Lista con status y mensaje de error
validar_parametros_huff <- function(cuartil, probabilidad) {
  errores <- character()
  
  if (!(cuartil %in% 1:4)) {
    errores <- c(errores, "El cuartil debe ser 1, 2, 3 o 4")
  }
  
  if (!(probabilidad %in% c(10, 50, 90))) {
    errores <- c(errores, "La probabilidad debe ser 10, 50 o 90")
  }
  
  if (length(errores) > 0) {
    return(list(valido = FALSE, errores = errores))
  } else {
    return(list(valido = TRUE, errores = NULL))
  }
}

#' Validar parámetros específicos del método SCS
#'
#' @param tipo_scs Tipo de distribución SCS ("I", "IA", "II", "III")
#' @return Lista con status y mensaje de error
validar_parametros_scs <- function(tipo_scs) {
  tipos_validos <- c("I", "IA", "II", "III")
  
  if (!(tipo_scs %in% tipos_validos)) {
    return(list(
      valido = FALSE,
      error = paste("Tipo SCS no válido. Debe ser uno de:", paste(tipos_validos, collapse = ", "))
    ))
  }
  
  return(list(valido = TRUE, error = NULL))
}

#' Mostrar resumen de parámetros validados
#'
#' @param precip_total Vector de precipitación total
#' @param TR Vector de periodos de retorno
#' @param duracion_horas Duración total
#' @param paso_minutos Paso de tiempo
mostrar_resumen_parametros <- function(precip_total, TR, duracion_horas, paso_minutos) {
  cat("\n=== RESUMEN DE PARÁMETROS ===\n")
  cat("Duración total:", duracion_horas, "horas\n")
  cat("Paso de tiempo:", paso_minutos, "minutos\n")
  cat("Número de pasos:", (duracion_horas * 60) / paso_minutos + 1, "\n")
  cat("\nPeriodos de retorno y precipitación:\n")
  for (i in seq_along(TR)) {
    cat(sprintf("  TR = %d años: P = %.2f mm\n", TR[i], precip_total[i]))
  }
  cat("=============================\n\n")
}

#' Ejecutar validación completa y mostrar resultados
#'
#' @param precip_total Vector de precipitación total
#' @param TR Vector de periodos de retorno
#' @param duracion_horas Duración total
#' @param paso_minutos Paso de tiempo
#' @return TRUE si es válido, detiene ejecución si no
ejecutar_validacion <- function(precip_total, TR, duracion_horas, paso_minutos) {
  cat("Validando parámetros de entrada...\n")
  validacion <- validar_parametros(precip_total, TR, duracion_horas, paso_minutos)
  
  if (!validacion$valido) {
    cat("\nERRORES DE VALIDACIÓN:\n")
    for (error in validacion$errores) {
      cat("  -", error, "\n")
    }
    stop("Corrija los errores antes de continuar.")
  } else {
    cat("✓ Parámetros válidos\n")
  }
  
  return(TRUE)
}
