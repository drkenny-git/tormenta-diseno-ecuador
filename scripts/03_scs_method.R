# =============================================================================
# 03_scs_method.R - Método de tormenta sintética SCS (mejorado para cualquier duración)
# =============================================================================

#' Cargar curvas SCS desde archivo CSV
#'
#' @param ruta_archivo Ruta al archivo CSV con las curvas SCS
#' @return Data frame con las curvas SCS
cargar_curvas_scs <- function(ruta_archivo = "data_sistema/scs_curves_standard.csv") {
  if (!file.exists(ruta_archivo)) {
    stop("No se encuentra el archivo de curvas SCS: ", ruta_archivo)
  }
  
  curvas <- read.csv(ruta_archivo, stringsAsFactors = FALSE)
  return(curvas)
}

#' Obtener nombre de columna para tipo SCS específico
#'
#' @param tipo_scs Tipo de distribución ("I", "IA", "II", "III")
#' @return Nombre de la columna
obtener_columna_scs <- function(tipo_scs) {
  paste0("Type_", tipo_scs)
}

#' Calcular pivote (punto de máxima intensidad) para cada tipo SCS
#'
#' @param curvas_scs Data frame con curvas SCS estandarizadas
#' @param tipo_scs Tipo de distribución
#' @return Tiempo normalizado del pivote (0-1)
calcular_pivote_scs <- function(curvas_scs, tipo_scs) {
  
  col_name <- obtener_columna_scs(tipo_scs)
  
  # Calcular diferencias (intensidad incremental)
  precip_acum <- curvas_scs[[col_name]]
  precip_incr <- diff(precip_acum)
  
  # Encontrar el índice del máximo incremental
  idx_max <- which.max(precip_incr)
  
  # El pivote está en el punto medio del intervalo de máxima intensidad
  tiempo_pivote <- (curvas_scs$time_fraction[idx_max] + curvas_scs$time_fraction[idx_max + 1]) / 2
  
  return(tiempo_pivote)
}

#' Extraer y re-estandarizar segmento de curva SCS
#'
#' @param curvas_scs Data frame con curvas SCS completas
#' @param tipo_scs Tipo de distribución
#' @param duracion_horas Duración deseada
#' @param pivote Punto pivote (calculado automáticamente si NULL)
#' @return Data frame con curva re-estandarizada (0-1 en ambos ejes)
extraer_segmento_scs <- function(curvas_scs, tipo_scs, duracion_horas, pivote = NULL) {
  
  col_name <- obtener_columna_scs(tipo_scs)
  
  # Calcular pivote si no se proporciona
  if (is.null(pivote)) {
    pivote <- calcular_pivote_scs(curvas_scs, tipo_scs)
  }
  
  # Convertir pivote de fracción (0-1) a horas (asumiendo 24h base)
  pivote_horas <- pivote * 24
  
  # Caso 1: Duración < 24 horas
  if (duracion_horas < 24) {
    
    # Calcular límites de la ventana
    t_inicio <- pivote_horas - duracion_horas / 2
    t_fin <- pivote_horas + duracion_horas / 2
    
    # Ajustar si t_inicio es negativo
    if (t_inicio < 0) {
      t_inicio <- 0
      t_fin <- duracion_horas
      cat("  Advertencia: Pivote muy cercano al inicio. Cortando desde t=0 hasta duración.\n")
    }
    
    # Convertir de nuevo a fracción
    t_inicio_frac <- t_inicio / 24
    t_fin_frac <- t_fin / 24
    
    # Extraer segmento
    mask <- curvas_scs$time_fraction >= t_inicio_frac & curvas_scs$time_fraction <= t_fin_frac
    
    tiempo_seg <- curvas_scs$time_fraction[mask]
    precip_seg <- curvas_scs[[col_name]][mask]
    
    # Re-estandarizar X (tiempo) de 0 a 1
    tiempo_norm <- (tiempo_seg - min(tiempo_seg)) / (max(tiempo_seg) - min(tiempo_seg))
    
    # Re-estandarizar Y (precipitación) de 0 a 1
    precip_norm <- (precip_seg - min(precip_seg)) / (max(precip_seg) - min(precip_seg))
    
  } else if (duracion_horas == 24) {
    # Caso 2: Duración = 24 horas (usar curva completa tal como está)
    tiempo_norm <- curvas_scs$time_fraction
    precip_norm <- curvas_scs[[col_name]]
    
  } else {
    # Caso 3: Duración > 24 horas
    cat("  Advertencia: Duración > 24h. El método SCS no fue diseñado para eventos mayores a 24 horas.\n")
    cat("              Se usará la curva completa estandarizada hasta la duración total.\n")
    
    # Usar curva completa tal como está
    tiempo_norm <- curvas_scs$time_fraction
    precip_norm <- curvas_scs[[col_name]]
  }
  
  # Crear data frame con curva re-estandarizada
  curva_restandarizada <- data.frame(
    time_fraction = tiempo_norm,
    precip_fraction = precip_norm
  )
  
  return(curva_restandarizada)
}

#' Calcular hietograma usando método SCS para cualquier duración
#'
#' @param precip_total Precipitación total (mm)
#' @param duracion_horas Duración total (cualquier duración)
#' @param paso_minutos Paso de tiempo (minutos)
#' @param tipo_scs Tipo de distribución SCS ("I", "IA", "II", "III")
#' @param curvas_scs Data frame con curvas SCS (opcional)
#' @return Data frame con el hietograma
calcular_hietograma_scs <- function(precip_total, duracion_horas, paso_minutos,
                                    tipo_scs, curvas_scs = NULL) {
  
  # Cargar curvas si no se proporcionan
  if (is.null(curvas_scs)) {
    curvas_scs <- cargar_curvas_scs()
  }
  
  # Validar parámetros específicos de SCS
  validacion <- validar_parametros_scs(tipo_scs)
  if (!validacion$valido) {
    stop("Parámetros de SCS inválidos: ", validacion$error)
  }
  
  # Extraer y re-estandarizar segmento según duración
  curva_estandarizada <- extraer_segmento_scs(curvas_scs, tipo_scs, duracion_horas)
  
  # Crear tiempo normalizado para el paso especificado
  tiempo_norm <- crear_tiempo_normalizado(duracion_horas, paso_minutos)
  
  # Interpolar precipitación acumulada (fracción de 0 a 1)
  precip_acum_frac <- interpolar_lineal(
    x_known = curva_estandarizada$time_fraction,
    y_known = curva_estandarizada$precip_fraction,
    x_new = tiempo_norm
  )
  
  # Convertir a mm
  precip_acum_mm <- precip_acum_frac * precip_total
  
  # Calcular precipitación incremental
  precip_incr_mm <- acumulada_a_incremental(precip_acum_mm)
  
  # Calcular tiempo en horas
  tiempo_horas <- tiempo_norm_a_horas(tiempo_norm, duracion_horas)
  
  # Crear data frame resultado
  resultado <- data.frame(
    paso = seq_along(tiempo_norm) - 1,
    tiempo_norm = tiempo_norm,
    tiempo_horas = tiempo_horas,
    tiempo_minutos = tiempo_horas * 60,
    precip_acum_frac = precip_acum_frac,
    precip_acum_mm = precip_acum_mm,
    precip_incr_mm = precip_incr_mm,
    intensidad_mm_h = precip_incr_mm / (paso_minutos / 60)
  )
  
  return(resultado)
}

#' Calcular hietogramas SCS para múltiples TRs
#'
#' @param precip_total Vector de precipitación total por TR
#' @param TR Vector de periodos de retorno
#' @param duracion_horas Duración total (cualquier duración)
#' @param paso_minutos Paso de tiempo
#' @param tipo_scs Tipo de distribución SCS
#' @return Lista de data frames, uno por cada TR
calcular_multiples_scs <- function(precip_total, TR, duracion_horas, paso_minutos,
                                   tipo_scs) {
  
  curvas_scs <- cargar_curvas_scs()
  
  resultados <- list()
  
  for (i in seq_along(TR)) {
    cat(sprintf("Calculando hietograma SCS para TR = %d años...\n", TR[i]))
    
    hietograma <- calcular_hietograma_scs(
      precip_total = precip_total[i],
      duracion_horas = duracion_horas,
      paso_minutos = paso_minutos,
      tipo_scs = tipo_scs,
      curvas_scs = curvas_scs
    )
    
    hietograma$TR <- TR[i]
    resultados[[paste0("TR_", TR[i])]] <- hietograma
  }
  
  return(resultados)
}

#' Obtener información sobre los tipos de distribución SCS
#'
#' @return Data frame con descripción de cada tipo
info_tipos_scs <- function() {
  data.frame(
    Tipo = c("I", "IA", "II", "III"),
    Region = c(
      "Costa Pacífico",
      "Zonas costeras con tormentas intensas",
      "Resto de EEUU (más común)",
      "Costa del Golfo y Costa Atlántica"
    ),
    Caracteristica = c(
      "Clima marítimo, inviernos húmedos",
      "Alta intensidad costera",
      "Tormentas convectivas intensas",
      "Tormentas costeras tropicales"
    ),
    stringsAsFactors = FALSE
  )
}

#' Recomendar tipo SCS según región (para referencia)
#'
#' @param region Descripción de la región
#' @return Lista con tipo recomendado y explicación
recomendar_tipo_scs <- function(region = NULL) {
  info <- info_tipos_scs()
  
  if (is.null(region)) {
    return(list(
      tipo = "II",
      explicacion = "Tipo II es el más común y ampliamente usado",
      info_completa = info
    ))
  }
  
  # Aquí se podría agregar lógica más específica según región
  return(list(
    tipo = "II",
    explicacion = "Por defecto se recomienda Tipo II (más común)",
    info_completa = info
  ))
}

#' Mostrar pivotes calculados para cada tipo SCS
#'
#' @param curvas_scs Data frame con curvas SCS (opcional)
mostrar_pivotes_scs <- function(curvas_scs = NULL) {
  
  if (is.null(curvas_scs)) {
    curvas_scs <- cargar_curvas_scs()
  }
  
  tipos <- c("I", "IA", "II", "III")
  
  cat("\n=== PIVOTES CALCULADOS (Puntos de máxima intensidad) ===\n")
  for (tipo in tipos) {
    pivote <- calcular_pivote_scs(curvas_scs, tipo)
    pivote_horas <- pivote * 24
    cat(sprintf("  Tipo %s: t = %.2f (%.2f horas en escala de 24h)\n", 
                tipo, pivote, pivote_horas))
  }
  cat("========================================================\n\n")
}
