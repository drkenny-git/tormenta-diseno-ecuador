# =============================================================================
# utils.R - Funciones auxiliares generales
# =============================================================================

#' Interpolar valores usando interpolación lineal
#'
#' @param x_known Vector de valores x conocidos
#' @param y_known Vector de valores y conocidos
#' @param x_new Vector de valores x donde interpolar
#' @return Vector de valores y interpolados
interpolar_lineal <- function(x_known, y_known, x_new) {
  approx(x = x_known, y = y_known, xout = x_new, method = "linear", rule = 2)$y
}

#' Crear secuencia de tiempo normalizado
#'
#' @param duracion_horas Duración total en horas
#' @param paso_minutos Paso de tiempo en minutos
#' @return Vector con tiempos normalizados (0 a 1)
crear_tiempo_normalizado <- function(duracion_horas, paso_minutos) {
  n_pasos <- (duracion_horas * 60) / paso_minutos + 1
  seq(0, 1, length.out = n_pasos)
}

#' Convertir precipitación acumulada a incremental
#'
#' @param precip_acum Vector de precipitación acumulada
#' @return Vector de precipitación incremental
acumulada_a_incremental <- function(precip_acum) {
  c(precip_acum[1], diff(precip_acum))
}

#' Calcular tiempo en horas desde fracción normalizada
#'
#' @param tiempo_norm Vector de tiempo normalizado (0-1)
#' @param duracion_horas Duración total en horas
#' @return Vector de tiempo en horas
tiempo_norm_a_horas <- function(tiempo_norm, duracion_horas) {
  tiempo_norm * duracion_horas
}

#' Formatear número con decimales consistentes
#'
#' @param x Número a formatear
#' @param decimales Número de decimales
#' @return String formateado
formatear_numero <- function(x, decimales = 2) {
  format(round(x, decimales), nsmall = decimales)
}
