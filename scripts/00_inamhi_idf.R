# =============================================================================
# 00_inamhi_idf.R - Método INAMHI para curvas IDF Ecuador
# =============================================================================

#' Cargar tabla de parámetros de ecuaciones IDF por zona
#'
#' @param ruta_archivo Ruta al archivo CSV con parámetros
#' @return Data frame con parámetros por zona
cargar_parametros_inamhi <- function(ruta_archivo = "data/inamhi_parametros.csv") {
  if (!file.exists(ruta_archivo)) {
    stop("No se encuentra el archivo de parámetros INAMHI: ", ruta_archivo)
  }
  
  parametros <- read.csv(ruta_archivo, stringsAsFactors = FALSE)
  
  # Validar columnas necesarias
  cols_necesarias <- c("ZONA", "CODIGO", "NOMBRE.ESTACION", "DURACION.INICIO", 
                       "DURACION.FIN", "K", "n", "R_CUADRADO")
  
  if (!all(cols_necesarias %in% colnames(parametros))) {
    stop("El archivo debe contener las columnas: ", paste(cols_necesarias, collapse = ", "))
  }
  
  return(parametros)
}

#' Cargar tabla de valores Idtr por estación
#'
#' @param ruta_archivo Ruta al archivo CSV con Idtr
#' @return Data frame con Idtr por estación y TR
cargar_idtr_inamhi <- function(ruta_archivo = "data/inamhi_idtr.csv") {
  if (!file.exists(ruta_archivo)) {
    stop("No se encuentra el archivo de Idtr INAMHI: ", ruta_archivo)
  }
  
  idtr <- read.csv(ruta_archivo, stringsAsFactors = FALSE)
  
  # Validar columnas necesarias
  cols_necesarias <- c("CODIGO", "ESTACION", "X", "Y", "Z", 
                       "TR2", "TR5", "TR10", "TR25", "TR50", "TR100")
  
  if (!all(cols_necesarias %in% colnames(idtr))) {
    stop("El archivo debe contener las columnas: ", paste(cols_necesarias, collapse = ", "))
  }
  
  return(idtr)
}

#' Obtener Idtr para un TR y estación específicos
#'
#' @param tabla_idtr Data frame con valores Idtr
#' @param codigo_estacion Código de la estación
#' @param TR Periodo de retorno (2, 5, 10, 25, 50, 100)
#' @return Valor de Idtr (mm/h)
obtener_idtr <- function(tabla_idtr, codigo_estacion, TR) {
  
  # Verificar que la estación existe
  estacion <- tabla_idtr[tabla_idtr$CODIGO == codigo_estacion, ]
  
  if (nrow(estacion) == 0) {
    stop("Estación ", codigo_estacion, " no encontrada en la tabla de Idtr")
  }
  
  # Mapear TR a columna
  col_tr <- paste0("TR", TR)
  
  if (!(col_tr %in% colnames(tabla_idtr))) {
    # Si el TR no está en las columnas estándar, intentar interpolar
    trs_disponibles <- c(2, 5, 10, 25, 50, 100)
    
    if (TR < 2 || TR > 100) {
      stop("TR fuera de rango. Debe estar entre 2 y 100 años")
    }
    
    # Interpolar logarítmicamente
    idtr_vals <- c(estacion$TR2, estacion$TR5, estacion$TR10, 
                   estacion$TR25, estacion$TR50, estacion$TR100)
    
    idtr <- exp(approx(x = log(trs_disponibles), y = log(idtr_vals), 
                       xout = log(TR), method = "linear")$y)
    
    cat(sprintf("  Advertencia: TR=%d no está en tabla. Idtr interpolado = %.3f mm/h\n", 
                TR, idtr))
    
  } else {
    idtr <- estacion[[col_tr]]
  }
  
  return(idtr)
}

#' Seleccionar ecuación apropiada según duración y zona
#'
#' @param tabla_parametros Data frame con parámetros
#' @param zona Número de zona INAMHI
#' @param duracion_min Duración en minutos
#' @return Lista con K, n, y rango de aplicación
seleccionar_ecuacion_inamhi <- function(tabla_parametros, zona, duracion_min) {
  
  # Filtrar por zona
  params_zona <- tabla_parametros[tabla_parametros$ZONA == zona, ]
  
  if (nrow(params_zona) == 0) {
    stop("Zona ", zona, " no encontrada en la tabla de parámetros")
  }
  
  # Buscar ecuación que contenga la duración
  ecuacion <- params_zona[duracion_min >= params_zona$DURACION.INICIO & 
                          duracion_min <= params_zona$DURACION.FIN, ]
  
  if (nrow(ecuacion) == 0) {
    stop(sprintf("No hay ecuación disponible para zona %d y duración %.1f min", 
                 zona, duracion_min))
  }
  
  if (nrow(ecuacion) > 1) {
    cat("  Advertencia: Múltiples ecuaciones encontradas. Usando la primera.\n")
    ecuacion <- ecuacion[1, ]
  }
  
  return(list(
    K = ecuacion$K,
    n = ecuacion$n,
    R2 = ecuacion$R_CUADRADO,
    duracion_inicio = ecuacion$DURACION.INICIO,
    duracion_fin = ecuacion$DURACION.FIN,
    codigo = ecuacion$CODIGO,
    estacion = ecuacion$NOMBRE.ESTACION
  ))
}

#' Calcular intensidad usando método INAMHI
#'
#' @param duracion_min Duración en minutos
#' @param idtr Valor de Idtr (mm/h)
#' @param K Parámetro K de la ecuación
#' @param n Parámetro n de la ecuación
#' @return Intensidad (mm/h)
calcular_intensidad_inamhi <- function(duracion_min, idtr, K, n) {
  I <- K * idtr * (duracion_min ^ n)
  return(I)
}

#' Calcular tabla completa de precipitación vs duración usando método INAMHI
#'
#' @param zona Número de zona INAMHI
#' @param codigo_estacion Código de estación (o NULL si se usa idtr_vector)
#' @param TR Periodo de retorno
#' @param duracion_total_min Duración total en minutos
#' @param paso_min Paso de tiempo en minutos
#' @param tabla_parametros Data frame con parámetros (opcional)
#' @param tabla_idtr Data frame con Idtr (opcional, no se usa si idtr_vector está presente)
#' @param idtr_vector Vector con Idtr para cada TR (opcional, para Idtr ponderado)
#' @return Data frame con duracion_min y precip_acum_mm
calcular_tabla_precipitacion_inamhi <- function(zona, codigo_estacion = NULL, TR, 
                                                duracion_total_min, paso_min,
                                                tabla_parametros = NULL,
                                                tabla_idtr = NULL,
                                                idtr_vector = NULL) {
  
  # Validar duración
  if (duracion_total_min < 5) {
    stop("La duración total debe ser >= 5 minutos (límite del método INAMHI)")
  }
  
  if (duracion_total_min > 1440) {
    cat("  ⚠ ADVERTENCIA: Duración > 1440 min (24h). El método INAMHI no fue calibrado para estas duraciones.\n")
  }
  
  # Cargar tablas si no se proporcionan
  if (is.null(tabla_parametros)) {
    tabla_parametros <- cargar_parametros_inamhi()
  }
  
  # Obtener Idtr
  if (!is.null(idtr_vector)) {
    # Usar Idtr ponderado
    idtr <- idtr_vector[paste0("TR", TR)]
  } else {
    # Usar Idtr de estación
    if (is.null(tabla_idtr)) {
      tabla_idtr <- cargar_idtr_inamhi()
    }
    idtr <- obtener_idtr(tabla_idtr, codigo_estacion, TR)
  }
  
  # Crear vector de duraciones acumuladas
  duraciones <- seq(paso_min, duracion_total_min, by = paso_min)
  
  # Vectores para almacenar resultados
  intensidades <- numeric(length(duraciones))
  precip_acum <- numeric(length(duraciones))
  
  # Calcular para cada duración
  for (i in seq_along(duraciones)) {
    dur <- duraciones[i]
    
    # Seleccionar ecuación apropiada
    ec <- seleccionar_ecuacion_inamhi(tabla_parametros, zona, dur)
    
    # Calcular intensidad
    intensidades[i] <- calcular_intensidad_inamhi(dur, idtr, ec$K, ec$n)
    
    # Calcular precipitación acumulada
    precip_acum[i] <- intensidades[i] * (dur / 60)
  }
  
  # Crear data frame resultado
  tabla <- data.frame(
    duracion_min = duraciones,
    intensidad_mm_h = intensidades,
    precip_acum_mm = precip_acum
  )
  
  return(tabla)
}

#' Calcular precipitación total para duración específica usando INAMHI
#'
#' @param zona Número de zona INAMHI
#' @param codigo_estacion Código de estación (o NULL si se usa idtr_valor)
#' @param TR Periodo de retorno
#' @param duracion_horas Duración en horas
#' @param tabla_parametros Data frame con parámetros (opcional)
#' @param tabla_idtr Data frame con Idtr (opcional)
#' @param idtr_valor Valor específico de Idtr (opcional, para Idtr ponderado)
#' @return Precipitación total en mm
calcular_precipitacion_inamhi <- function(zona, codigo_estacion = NULL, TR, duracion_horas,
                                         tabla_parametros = NULL,
                                         tabla_idtr = NULL,
                                         idtr_valor = NULL) {
  
  duracion_min <- duracion_horas * 60
  
  # Cargar tablas si no se proporcionan
  if (is.null(tabla_parametros)) {
    tabla_parametros <- cargar_parametros_inamhi()
  }
  
  # Obtener Idtr
  if (!is.null(idtr_valor)) {
    # Usar Idtr ponderado
    idtr <- idtr_valor
  } else {
    # Usar Idtr de estación
    if (is.null(tabla_idtr)) {
      tabla_idtr <- cargar_idtr_inamhi()
    }
    idtr <- obtener_idtr(tabla_idtr, codigo_estacion, TR)
  }
  
  # Seleccionar ecuación
  ec <- seleccionar_ecuacion_inamhi(tabla_parametros, zona, duracion_min)
  
  # Calcular intensidad
  intensidad <- calcular_intensidad_inamhi(duracion_min, idtr, ec$K, ec$n)
  
  # Calcular precipitación total
  precip_total <- intensidad * duracion_horas
  
  return(precip_total)
}

#' Calcular precipitaciones para múltiples TRs usando INAMHI
#'
#' @param zona Número de zona INAMHI
#' @param codigo_estacion Código de estación
#' @param TR Vector de periodos de retorno
#' @param duracion_horas Duración en horas
#' @param tabla_parametros Data frame con parámetros (opcional)
#' @param tabla_idtr Data frame con Idtr (opcional)
#' @return Vector con precipitaciones totales por TR
calcular_precipitaciones_multiples_TR <- function(zona, codigo_estacion, TR, 
                                                  duracion_horas,
                                                  tabla_parametros = NULL,
                                                  tabla_idtr = NULL) {
  
  # Cargar tablas si no se proporcionan
  if (is.null(tabla_parametros)) {
    tabla_parametros <- cargar_parametros_inamhi()
  }
  
  if (is.null(tabla_idtr)) {
    tabla_idtr <- cargar_idtr_inamhi()
  }
  
  precip_totales <- numeric(length(TR))
  
  for (i in seq_along(TR)) {
    precip_totales[i] <- calcular_precipitacion_inamhi(
      zona = zona,
      codigo_estacion = codigo_estacion,
      TR = TR[i],
      duracion_horas = duracion_horas,
      tabla_parametros = tabla_parametros,
      tabla_idtr = tabla_idtr
    )
  }
  
  return(precip_totales)
}

#' Calcular tablas de precipitación para múltiples TRs (para Bloque Alterno)
#'
#' @param zona Número de zona INAMHI
#' @param codigo_estacion Código de estación
#' @param TR Vector de periodos de retorno
#' @param duracion_total_min Duración total en minutos
#' @param paso_min Paso de tiempo en minutos
#' @param tabla_parametros Data frame con parámetros (opcional)
#' @param tabla_idtr Data frame con Idtr (opcional)
#' @return Lista de data frames (uno por TR)
calcular_tablas_multiples_TR_inamhi <- function(zona, codigo_estacion, TR,
                                                duracion_total_min, paso_min,
                                                tabla_parametros = NULL,
                                                tabla_idtr = NULL) {
  
  # Cargar tablas si no se proporcionan
  if (is.null(tabla_parametros)) {
    tabla_parametros <- cargar_parametros_inamhi()
  }
  
  if (is.null(tabla_idtr)) {
    tabla_idtr <- cargar_idtr_inamhi()
  }
  
  lista_tablas <- list()
  
  for (i in seq_along(TR)) {
    cat(sprintf("Calculando tabla INAMHI para TR = %d años...\n", TR[i]))
    
    tabla <- calcular_tabla_precipitacion_inamhi(
      zona = zona,
      codigo_estacion = codigo_estacion,
      TR = TR[i],
      duracion_total_min = duracion_total_min,
      paso_min = paso_min,
      tabla_parametros = tabla_parametros,
      tabla_idtr = tabla_idtr
    )
    
    lista_tablas[[i]] <- tabla
  }
  
  return(lista_tablas)
}

#' Mostrar información de zona y estación
#'
#' @param zona Número de zona
#' @param codigo_estacion Código de estación
#' @param tabla_parametros Data frame con parámetros (opcional)
#' @param tabla_idtr Data frame con Idtr (opcional)
mostrar_info_inamhi <- function(zona, codigo_estacion, 
                               tabla_parametros = NULL,
                               tabla_idtr = NULL) {
  
  # Cargar tablas si no se proporcionan
  if (is.null(tabla_parametros)) {
    tabla_parametros <- cargar_parametros_inamhi()
  }
  
  if (is.null(tabla_idtr)) {
    tabla_idtr <- cargar_idtr_inamhi()
  }
  
  cat("\n=== INFORMACIÓN INAMHI ===\n\n")
  
  # Información de estación
  estacion <- tabla_idtr[tabla_idtr$CODIGO == codigo_estacion, ]
  if (nrow(estacion) > 0) {
    cat("ESTACIÓN:\n")
    cat("  Código:", estacion$CODIGO, "\n")
    cat("  Nombre:", estacion$ESTACION, "\n")
    cat("  Ubicación: X =", estacion$X, ", Y =", estacion$Y, "(UTM 17S)\n")
    cat("  Altitud:", estacion$Z, "m\n")
    if ("SERIE.DATOS" %in% colnames(estacion)) {
      cat("  Serie de datos:", estacion$SERIE.DATOS, "\n")
      cat("  Años de datos:", estacion$ANIOS, "\n")
    }
    cat("\n")
  }
  
  # Ecuaciones de la zona
  params_zona <- tabla_parametros[tabla_parametros$ZONA == zona, ]
  if (nrow(params_zona) > 0) {
    cat("ECUACIONES DE LA ZONA", zona, ":\n")
    for (i in 1:nrow(params_zona)) {
      ec <- params_zona[i, ]
      cat(sprintf("  %.1f ≤ t ≤ %.1f min: I = %.3f * Idtr * t^(%.3f)  [R² = %.4f]\n",
                  ec$DURACION.INICIO, ec$DURACION.FIN, ec$K, ec$n, ec$R_CUADRADO))
    }
    cat("\n")
  }
  
  # Valores de Idtr
  if (nrow(estacion) > 0) {
    cat("VALORES DE Idtr (mm/h para d=24h):\n")
    cat(sprintf("  TR=2:   %.2f mm/h\n", estacion$TR2))
    cat(sprintf("  TR=5:   %.2f mm/h\n", estacion$TR5))
    cat(sprintf("  TR=10:  %.2f mm/h\n", estacion$TR10))
    cat(sprintf("  TR=25:  %.2f mm/h\n", estacion$TR25))
    cat(sprintf("  TR=50:  %.2f mm/h\n", estacion$TR50))
    cat(sprintf("  TR=100: %.2f mm/h\n", estacion$TR100))
  }
  
  cat("\n==========================\n\n")
}
