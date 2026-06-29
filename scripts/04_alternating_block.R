# =============================================================================
# 04_alternating_block.R - Método de bloque alterno
# =============================================================================

#' Validar tabla de entrada para método de bloque alterno
#'
#' @param tabla Data frame con columnas duracion_min y precip_acum_mm
#' @return Lista con validación (valido, errores)
validar_tabla_bloque_alterno <- function(tabla) {
  errores <- character()
  
  # Verificar que existan las columnas necesarias
  if (!all(c("duracion_min", "precip_acum_mm") %in% colnames(tabla))) {
    errores <- c(errores, "La tabla debe contener las columnas: duracion_min, precip_acum_mm")
    return(list(valido = FALSE, errores = errores))
  }
  
  # Verificar que los valores sean numéricos
  if (!is.numeric(tabla$duracion_min) || !is.numeric(tabla$precip_acum_mm)) {
    errores <- c(errores, "Las columnas duracion_min y precip_acum_mm deben ser numéricas")
  }
  
  # Verificar que no haya NAs
  if (any(is.na(tabla$duracion_min)) || any(is.na(tabla$precip_acum_mm))) {
    errores <- c(errores, "No se permiten valores NA en la tabla")
  }
  
  # Verificar que sean valores positivos
  if (any(tabla$duracion_min <= 0) || any(tabla$precip_acum_mm < 0)) {
    errores <- c(errores, "Todos los valores deben ser positivos (duracion_min > 0, precip_acum_mm >= 0)")
  }
  
  # Verificar que sean incrementales (columna 1 - duración)
  if (any(diff(tabla$duracion_min) <= 0)) {
    errores <- c(errores, "La columna duracion_min debe ser estrictamente incremental (cada fila debe ser mayor que la anterior)")
  }
  
  # Verificar que sean incrementales (columna 2 - precipitación acumulada)
  if (any(diff(tabla$precip_acum_mm) < 0)) {
    errores <- c(errores, "La columna precip_acum_mm debe ser incremental (no decreciente)")
  }
  
  if (length(errores) > 0) {
    return(list(valido = FALSE, errores = errores))
  } else {
    return(list(valido = TRUE, errores = NULL))
  }
}

#' Calcular hietograma usando método de bloque alterno
#'
#' @param tabla_intensidad Data frame con duracion_min y precip_acum_mm
#' @param paso_minutos Paso de tiempo deseado (minutos) - opcional
#' @return Data frame con el hietograma
calcular_hietograma_bloque_alterno <- function(tabla_intensidad, paso_minutos = NULL) {
  
  # Validar tabla de entrada
  validacion <- validar_tabla_bloque_alterno(tabla_intensidad)
  if (!validacion$valido) {
    stop("Tabla de entrada inválida:\n  ", paste(validacion$errores, collapse = "\n  "))
  }
  
  # Si no se especifica paso, usar las duraciones de la tabla
  if (is.null(paso_minutos)) {
    # Calcular paso como las diferencias entre duraciones
    pasos <- diff(tabla_intensidad$duracion_min)
    
    # Verificar que los pasos sean consistentes
    if (length(unique(pasos)) > 1) {
      cat("  Advertencia: Los pasos de tiempo no son uniformes. Se usarán las duraciones tal como están.\n")
    }
    
    # Usar la tabla tal como está
    duraciones_acum <- tabla_intensidad$duracion_min
    precip_acum <- tabla_intensidad$precip_acum_mm
    
  } else {
    # Interpolar para obtener valores en el paso especificado
    duracion_total <- max(tabla_intensidad$duracion_min)
    duraciones_acum <- seq(paso_minutos, duracion_total, by = paso_minutos)
    
    # Interpolar precipitación acumulada
    precip_acum <- interpolar_lineal(
      x_known = tabla_intensidad$duracion_min,
      y_known = tabla_intensidad$precip_acum_mm,
      x_new = duraciones_acum
    )
  }
  
  # Calcular precipitación incremental para cada bloque
  n_bloques <- length(duraciones_acum)
  precip_incr <- c(precip_acum[1], diff(precip_acum))
  
  # Aplicar método de bloque alterno: ordenar bloques de manera alternada
  # El bloque con mayor precipitación va al centro
  precip_ordenada <- ordenar_bloques_alterno(precip_incr)
  
  # Calcular precipitación acumulada final
  precip_acum_final <- cumsum(precip_ordenada)
  
  # Calcular tiempo (agregando punto inicial en 0)
  if (is.null(paso_minutos)) {
    # Calcular pasos variables
    pasos_tiempo <- diff(c(0, duraciones_acum))
  } else {
    pasos_tiempo <- rep(paso_minutos, n_bloques)
  }
  
  tiempo_minutos <- c(0, cumsum(pasos_tiempo))
  tiempo_horas <- tiempo_minutos / 60
  duracion_total_horas <- max(tiempo_horas)
  tiempo_norm <- tiempo_horas / duracion_total_horas
  
  # Crear data frame resultado
  resultado <- data.frame(
    paso = seq_along(tiempo_horas) - 1,
    tiempo_norm = tiempo_norm,
    tiempo_horas = tiempo_horas,
    tiempo_minutos = tiempo_minutos,
    precip_incr_mm = c(0, precip_ordenada),
    precip_acum_mm = c(0, precip_acum_final),
    intensidad_mm_h = c(0, precip_ordenada) / (c(pasos_tiempo[1], pasos_tiempo) / 60)
  )
  
  # Agregar columna de fracción acumulada para consistencia con otros métodos
  resultado$precip_acum_frac <- resultado$precip_acum_mm / max(resultado$precip_acum_mm, na.rm = TRUE)
  
  return(resultado)
}

#' Ordenar bloques de manera alternada (algoritmo de bloque alterno)
#'
#' @param bloques Vector de precipitación incremental por bloque
#' @return Vector con bloques ordenados de manera alternada
ordenar_bloques_alterno <- function(bloques) {
  n <- length(bloques)
  
  # Ordenar de mayor a menor
  bloques_ordenados <- sort(bloques, decreasing = TRUE)
  
  # Crear vector resultado
  resultado <- numeric(n)
  
  # Encontrar posición central
  pos_central <- ceiling(n / 2)
  
  # Colocar el bloque más grande en el centro
  resultado[pos_central] <- bloques_ordenados[1]
  
  # Alternar bloques a derecha e izquierda del centro
  pos_izq <- pos_central - 1
  pos_der <- pos_central + 1
  
  for (i in 2:n) {
    if (i %% 2 == 0 && pos_der <= n) {
      # Bloques pares van a la derecha
      resultado[pos_der] <- bloques_ordenados[i]
      pos_der <- pos_der + 1
    } else if (pos_izq >= 1) {
      # Bloques impares van a la izquierda
      resultado[pos_izq] <- bloques_ordenados[i]
      pos_izq <- pos_izq - 1
    } else if (pos_der <= n) {
      # Si no hay más espacio a la izquierda
      resultado[pos_der] <- bloques_ordenados[i]
      pos_der <- pos_der + 1
    }
  }
  
  return(resultado)
}

#' Calcular hietograma de bloque alterno para múltiples TRs
#'
#' @param lista_tablas Lista de tablas intensidad-duración por TR
#' @param TR Vector de periodos de retorno
#' @param paso_minutos Paso de tiempo (opcional)
#' @return Lista de data frames, uno por cada TR
calcular_multiples_bloque_alterno <- function(lista_tablas, TR, paso_minutos = NULL) {
  
  if (length(lista_tablas) != length(TR)) {
    stop("El número de tablas debe coincidir con el número de TRs")
  }
  
  resultados <- list()
  
  for (i in seq_along(TR)) {
    cat(sprintf("Calculando hietograma Bloque Alterno para TR = %d años...\n", TR[i]))
    
    hietograma <- calcular_hietograma_bloque_alterno(
      tabla_intensidad = lista_tablas[[i]],
      paso_minutos = paso_minutos
    )
    
    hietograma$TR <- TR[i]
    resultados[[paste0("TR_", TR[i])]] <- hietograma
  }
  
  return(resultados)
}

#' Crear tabla de intensidad-duración de ejemplo desde curva IDF típica
#'
#' @param duracion_total_min Duración total en minutos
#' @param paso_min Paso de tiempo en minutos
#' @param precip_total_mm Precipitación total en mm
#' @param output_path Ruta donde guardar el archivo (opcional)
#' @return Data frame con tabla de ejemplo
crear_tabla_idf_ejemplo <- function(duracion_total_min = 360, 
                                    paso_min = 30,
                                    precip_total_mm = 100,
                                    output_path = NULL) {
  
  # Crear duraciones acumuladas
  duraciones <- seq(paso_min, duracion_total_min, by = paso_min)
  
  # Simular curva IDF típica usando modelo exponencial
  # P(t) = P_total * (1 - exp(-k*t))
  # Donde k controla la forma de la curva
  k <- 3 / duracion_total_min  # Parámetro de ajuste
  
  precip_acum <- precip_total_mm * (1 - exp(-k * duraciones))
  
  # Crear tabla
  tabla <- data.frame(
    duracion_min = duraciones,
    precip_acum_mm = round(precip_acum, 2)
  )
  
  # Guardar si se especifica ruta
  if (!is.null(output_path)) {
    write.csv(tabla, output_path, row.names = FALSE)
    cat("Tabla de ejemplo guardada en:", output_path, "\n")
  }
  
  return(tabla)
}

#' Crear múltiples tablas IDF para diferentes TRs
#'
#' @param TR Vector de periodos de retorno
#' @param precip_total Vector de precipitaciones totales por TR
#' @param duracion_total_min Duración total
#' @param paso_min Paso de tiempo
#' @param directorio Directorio donde guardar (opcional)
#' @return Lista de tablas por TR
crear_tablas_idf_multiples_TR <- function(TR, precip_total, duracion_total_min = 360,
                                          paso_min = 30, directorio = NULL) {
  
  if (length(TR) != length(precip_total)) {
    stop("TR y precip_total deben tener la misma longitud")
  }
  
  lista_tablas <- list()
  
  for (i in seq_along(TR)) {
    cat(sprintf("Creando tabla IDF para TR = %d años (P = %.1f mm)...\n", 
                TR[i], precip_total[i]))
    
    tabla <- crear_tabla_idf_ejemplo(
      duracion_total_min = duracion_total_min,
      paso_min = paso_min,
      precip_total_mm = precip_total[i],
      output_path = if (!is.null(directorio)) {
        file.path(directorio, paste0("idf_TR", TR[i], ".csv"))
      } else {
        NULL
      }
    )
    
    lista_tablas[[i]] <- tabla
  }
  
  cat("✓ Tablas IDF creadas para", length(TR), "periodos de retorno\n")
  
  return(lista_tablas)
}
