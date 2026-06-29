# =============================================================================
# 07_custom_method.R - Método de hietograma con curva personalizada del usuario
# =============================================================================
#
# El usuario puede cargar su propia curva de precipitación acumulada
# estandarizada como CSV con columnas X (fracción de tiempo) e Y
# (fracción de precipitación acumulada), ambas en [0, 1].
#
# Reglas de validación:
#   - Al menos 3 puntos
#   - Primer punto exactamente (0, 0)
#   - Último punto exactamente (1, 1)
#   - X estrictamente creciente
#   - Y no decreciente (monótona)
#   - Todos los valores en [0, 1]
# =============================================================================

#' Leer y normalizar columnas de un data frame de curva personalizada
#'
#' Acepta columnas nombradas X/Y, x/y, o cualquier par de dos columnas
#' (en ese caso las renombra a X e Y en orden).
#'
#' @param df Data frame leído del CSV del usuario
#' @return Lista con `df` normalizado y `mensaje` si hubo renombrado
normalizar_columnas_curva <- function(df) {
  nombres <- colnames(df)

  # Caso 1: columnas ya se llaman X e Y (cualquier capitalización)
  idx_x <- which(toupper(nombres) == "X")
  idx_y <- which(toupper(nombres) == "Y")

  if (length(idx_x) == 1 && length(idx_y) == 1) {
    colnames(df)[idx_x] <- "X"
    colnames(df)[idx_y] <- "Y"
    return(list(df = df, mensaje = NULL))
  }

  # Caso 2: exactamente 2 columnas con cualquier nombre → renombrar
  if (ncol(df) == 2) {
    colnames(df) <- c("X", "Y")
    return(list(df = df,
                mensaje = paste0("Columnas renombradas a X e Y (originales: '",
                                 nombres[1], "', '", nombres[2], "')")))
  }

  # No se pudo resolver
  return(list(df = NULL,
              mensaje = paste0("No se encontraron columnas X e Y y el CSV no tiene ",
                               "exactamente 2 columnas (tiene ", ncol(df), ").")))
}

#' Validar curva de precipitación acumulada personalizada
#'
#' @param df Data frame con columnas X (tiempo normalizado) e Y (precip normalizada).
#'   Puede venir con nombres alternativos; se normalizan automáticamente.
#' @return Lista con:
#'   \item{valido}{TRUE si la curva es aceptable}
#'   \item{errores}{Vector de mensajes de error (vacío si valido = TRUE)}
#'   \item{advertencias}{Vector de advertencias no bloqueantes}
#'   \item{df}{Data frame con columnas X e Y limpias (o NULL si hay errores)}
validar_curva_personalizada <- function(df) {
  errores      <- character()
  advertencias <- character()

  # --- Normalizar columnas ---
  norm <- normalizar_columnas_curva(df)
  if (is.null(norm$df)) {
    return(list(valido = FALSE,
                errores = norm$mensaje,
                advertencias = character(),
                df = NULL))
  }
  df <- norm$df
  if (!is.null(norm$mensaje)) {
    advertencias <- c(advertencias, norm$mensaje)
  }

  # --- Verificar que las columnas sean numéricas ---
  if (!is.numeric(df$X)) {
    errores <- c(errores, "La columna X debe ser numérica")
  }
  if (!is.numeric(df$Y)) {
    errores <- c(errores, "La columna Y debe ser numérica")
  }

  # Si no son numéricas no tiene sentido seguir validando
  if (length(errores) > 0) {
    return(list(valido = FALSE, errores = errores,
                advertencias = advertencias, df = NULL))
  }

  # --- Eliminar filas con NA ---
  n_antes <- nrow(df)
  df <- df[!is.na(df$X) & !is.na(df$Y), ]
  if (nrow(df) < n_antes) {
    advertencias <- c(advertencias,
                      sprintf("Se eliminaron %d filas con valores NA",
                              n_antes - nrow(df)))
  }

  # --- Mínimo de puntos ---
  if (nrow(df) < 3) {
    errores <- c(errores,
                 sprintf("La curva debe tener al menos 3 puntos (tiene %d)", nrow(df)))
  }

  # --- Rango de X ---
  if (any(df$X < 0) || any(df$X > 1)) {
    errores <- c(errores,
                 "Todos los valores de X deben estar en [0, 1]")
  }

  # --- Rango de Y ---
  if (any(df$Y < 0) || any(df$Y > 1)) {
    errores <- c(errores,
                 "Todos los valores de Y deben estar en [0, 1]")
  }

  # --- Primer punto (0, 0) ---
  tol <- 1e-9
  if (abs(df$X[1] - 0) > tol || abs(df$Y[1] - 0) > tol) {
    errores <- c(errores,
                 sprintf("El primer punto debe ser (0, 0); se encontró (%.6g, %.6g)",
                         df$X[1], df$Y[1]))
  }

  # --- Último punto (1, 1) ---
  n <- nrow(df)
  if (abs(df$X[n] - 1) > tol || abs(df$Y[n] - 1) > tol) {
    errores <- c(errores,
                 sprintf("El último punto debe ser (1, 1); se encontró (%.6g, %.6g)",
                         df$X[n], df$Y[n]))
  }

  # --- X estrictamente creciente ---
  if (nrow(df) >= 2 && any(diff(df$X) <= 0)) {
    idx_mal <- which(diff(df$X) <= 0)
    errores <- c(errores,
                 sprintf("X debe ser estrictamente creciente (problema en fila(s): %s)",
                         paste(idx_mal + 1, collapse = ", ")))
  }

  # --- Y no decreciente ---
  if (nrow(df) >= 2 && any(diff(df$Y) < 0)) {
    idx_mal <- which(diff(df$Y) < 0)
    errores <- c(errores,
                 sprintf("Y debe ser no decreciente (problema en fila(s): %s)",
                         paste(idx_mal + 1, collapse = ", ")))
  }

  if (length(errores) > 0) {
    return(list(valido = FALSE, errores = errores,
                advertencias = advertencias, df = NULL))
  }

  return(list(valido = TRUE, errores = character(),
              advertencias = advertencias, df = df))
}

#' Calcular hietograma usando una curva de precipitación personalizada
#'
#' @param precip_total Precipitación total (mm)
#' @param duracion_horas Duración total (horas)
#' @param paso_minutos Paso de tiempo (minutos)
#' @param curva_df Data frame con columnas X e Y de la curva validada
#' @return Data frame con el hietograma (misma estructura que Huff/SCS)
calcular_hietograma_curva_personalizada <- function(precip_total, duracion_horas,
                                                    paso_minutos, curva_df) {
  # Validar curva
  val <- validar_curva_personalizada(curva_df)
  if (!val$valido) {
    stop("Curva inválida:\n  ", paste(val$errores, collapse = "\n  "))
  }
  df <- val$df

  # Crear tiempo normalizado
  tiempo_norm <- crear_tiempo_normalizado(duracion_horas, paso_minutos)

  # Interpolar fracción acumulada de precipitación
  precip_acum_frac <- interpolar_lineal(
    x_known = df$X,
    y_known = df$Y,
    x_new   = tiempo_norm
  )

  # Convertir a mm
  precip_acum_mm <- precip_acum_frac * precip_total

  # Calcular incremental
  precip_incr_mm <- acumulada_a_incremental(precip_acum_mm)

  # Tiempo en horas
  tiempo_horas <- tiempo_norm_a_horas(tiempo_norm, duracion_horas)

  resultado <- data.frame(
    paso            = seq_along(tiempo_norm) - 1,
    tiempo_norm     = tiempo_norm,
    tiempo_horas    = tiempo_horas,
    tiempo_minutos  = tiempo_horas * 60,
    precip_acum_frac = precip_acum_frac,
    precip_acum_mm  = precip_acum_mm,
    precip_incr_mm  = precip_incr_mm,
    intensidad_mm_h = precip_incr_mm / (paso_minutos / 60)
  )

  return(resultado)
}

#' Calcular hietogramas con curva personalizada para múltiples TRs
#'
#' @param precip_total Vector de precipitación total por TR (mm)
#' @param TR Vector de periodos de retorno
#' @param duracion_horas Duración total (horas)
#' @param paso_minutos Paso de tiempo (minutos)
#' @param curva_df Data frame con columnas X e Y
#' @return Lista de data frames, uno por TR
calcular_multiples_curva_personalizada <- function(precip_total, TR, duracion_horas,
                                                   paso_minutos, curva_df) {
  if (length(precip_total) != length(TR)) {
    stop("precip_total y TR deben tener la misma longitud")
  }

  # Validar la curva una sola vez
  val <- validar_curva_personalizada(curva_df)
  if (!val$valido) {
    stop("Curva inválida:\n  ", paste(val$errores, collapse = "\n  "))
  }

  resultados <- list()

  for (i in seq_along(TR)) {
    cat(sprintf("Calculando hietograma curva personalizada para TR = %d años...\n", TR[i]))

    hietograma <- calcular_hietograma_curva_personalizada(
      precip_total   = precip_total[i],
      duracion_horas = duracion_horas,
      paso_minutos   = paso_minutos,
      curva_df       = val$df
    )

    hietograma$TR <- TR[i]
    resultados[[paste0("TR_", TR[i])]] <- hietograma
  }

  return(resultados)
}
