# =============================================================================
# 00c_funciones_soporte.R - Funciones de alto nivel para main.R
# =============================================================================

#' Exploración espacial INFORMATIVA (no selecciona, solo muestra)
#'
#' @param datos_sistema Lista con datos del sistema
#' @param cuenca_shp Ruta al shapefile de cuenca (opcional)
#' @param punto_xy Vector c(x, y) en UTM 17S (opcional)
#' @param p_cercanas Número de estaciones cercanas a mostrar
#' @return Mapa leaflet
mostrar_exploracion_espacial <- function(datos_sistema,
                                         cuenca_shp = NULL,
                                         punto_xy = NULL,
                                         p_cercanas = 10) {
  
  cat("\n==============================================================\n")
  cat("  EXPLORACIÓN ESPACIAL - INFORMACIÓN\n")
  cat("==============================================================\n\n")
  
  # Preparar objetos para mapa
  zonas_mapa <- if (!is.null(datos_sistema$zonas)) datos_sistema$zonas else NULL
  cuenca_mapa <- NULL
  punto_mapa <- NULL
  
  # Determinar qué se ingresó (priorizar cuenca si ambos están presentes)
  tiene_cuenca <- !is.null(cuenca_shp) && file.exists(cuenca_shp)
  tiene_punto <- !is.null(punto_xy) && length(punto_xy) == 2 && !tiene_cuenca
  
  # Cargar cuenca o crear punto
  if (tiene_cuenca) {
    cat("Cargando cuenca...\n")
    cuenca_mapa <- cargar_cuenca(cuenca_shp)
    cat("✓ Cuenca cargada\n\n")
    
  } else if (tiene_punto) {
    cat(sprintf("Punto: X=%.2f, Y=%.2f (UTM 17S)\n\n", punto_xy[1], punto_xy[2]))
    punto_mapa <- crear_punto(punto_xy[1], punto_xy[2])
  }
  
  # SIEMPRE crear mapa
  cat("Generando mapa interactivo...\n")
  mapa <- crear_mapa_exploracion(
    zonas = zonas_mapa,
    estaciones_sf = datos_sistema$estaciones_sf,
    cuenca = cuenca_mapa,
    punto = punto_mapa,
    output_html = "output/mapa_exploracion.html"
  )
  cat("✓ Mapa guardado en: output/mapa_exploracion.html\n\n")
  
  # Análisis informativo si hay cuenca o punto
  if (tiene_cuenca) {
    cat("=== ANÁLISIS DE CUENCA ===\n\n")
    
    # Zona recomendada
    if (!is.null(zonas_mapa)) {
      zona_info <- determinar_zona_cuenca(cuenca_mapa, zonas_mapa)
      cat("ZONA RECOMENDADA:", zona_info$zona_principal, "\n")
      cat(sprintf("  (%.1f%% del área de la cuenca)\n", zona_info$porcentaje_principal))
      
      if (nrow(zona_info$todas_zonas) > 1) {
        cat("\nOtras zonas presentes:\n")
        for (i in 2:min(3, nrow(zona_info$todas_zonas))) {
          cat(sprintf("  Zona %d: %.1f%%\n", 
                      zona_info$todas_zonas$ZONA[i],
                      zona_info$todas_zonas$porcentaje[i]))
        }
      }
      cat("\n")
    }
    
    # Distancias a estaciones
    distancias <- calcular_distancias_cuenca(cuenca_mapa, datos_sistema$estaciones_sf)
    estaciones_dentro <- estaciones_dentro_cuenca(cuenca_mapa, datos_sistema$estaciones_sf)
    
    estaciones_cercanas <- listar_estaciones_cercanas(
      distancias, datos_sistema$idtr, p_cercanas, estaciones_dentro
    )
    
    mostrar_resumen_exploracion(NULL, estaciones_cercanas, estaciones_dentro)
    
  } else if (tiene_punto) {
    cat("=== ANÁLISIS DE PUNTO ===\n\n")
    
    # Zona del punto
    if (!is.null(zonas_mapa)) {
      zona <- determinar_zona_punto(punto_mapa, zonas_mapa)
      cat("ZONA RECOMENDADA:", zona, "\n\n")
    }
    
    # Distancias a estaciones
    distancias <- calcular_distancias_punto(punto_mapa, datos_sistema$estaciones_sf)
    
    estaciones_cercanas <- listar_estaciones_cercanas(
      distancias, datos_sistema$idtr, p_cercanas
    )
    
    mostrar_resumen_exploracion(NULL, estaciones_cercanas, NULL)
    
  } else {
    cat("=== EXPLORACIÓN GENERAL ===\n\n")
    cat("Mapa generado con todas las zonas y estaciones.\n")
    cat("Consulte el mapa para seleccionar zona y estaciones.\n\n")
    
    # Listar todas las estaciones disponibles
    cat("ESTACIONES DISPONIBLES:\n\n")
    for (i in 1:nrow(datos_sistema$idtr)) {
      est <- datos_sistema$idtr[i, ]
      cat(sprintf("%2d. %s - %s (Zona aprox: cerca de %.0f, %.0f)\n",
                  i, est$CODIGO, est$ESTACION, est$X, est$Y))
    }
    cat("\n")
  }
  
  cat("NOTA: Esta información es solo RECOMENDADA.\n")
  cat("      Usted debe definir zona y estaciones manualmente.\n")
  cat("\n")
  
  return(mapa)
}

#' Calcular Idtr ponderado del usuario
#'
#' @param zona Zona definida por usuario
#' @param estaciones Vector de códigos definidos por usuario
#' @param datos_sistema Datos del sistema
#' @param cuenca_shp Shapefile de cuenca (para Thiessen, opcional)
#' @param punto_xy Coordenadas de punto (para IDW, opcional)
#' @param potencia_idw Potencia para IDW
#' @return Lista con idtr_ponderado, pesos, metodo_usado
calcular_idtr_usuario <- function(zona, estaciones, datos_sistema,
                                  cuenca_shp = NULL, punto_xy = NULL,
                                  potencia_idw = 2) {
  
  cat("\n==============================================================\n")
  cat("  CÁLCULO DE IDTR PONDERADO\n")
  cat("==============================================================\n\n")
  
  cat("Zona seleccionada:", zona, "\n")
  cat("Estaciones seleccionadas:", paste(estaciones, collapse = ", "), "\n\n")
  
  # Validar que estaciones existan
  for (cod in estaciones) {
    if (!(cod %in% datos_sistema$idtr$CODIGO)) {
      stop("Estación no encontrada: ", cod)
    }
  }
  
  # Determinar método de ponderación (priorizar cuenca)
  tiene_cuenca <- !is.null(cuenca_shp) && file.exists(cuenca_shp)
  tiene_punto <- !is.null(punto_xy) && length(punto_xy) == 2 && !tiene_cuenca
  
  if (tiene_cuenca && length(estaciones) > 1) {
    # THIESSEN
    cat("Método de ponderación: THIESSEN (polígonos)\n\n")
    
    cuenca <- cargar_cuenca(cuenca_shp)
    
    idtr_result <- calcular_idtr_ponderado(
      estaciones_seleccionadas = estaciones,
      tabla_idtr = datos_sistema$idtr,
      metodo = "thiessen",
      cuenca = cuenca,
      estaciones_sf = datos_sistema$estaciones_sf
    )
    
  } else if (tiene_punto && length(estaciones) > 1) {
    # IDW
    cat("Método de ponderación: IDW (inverso de distancia, potencia =", potencia_idw, ")\n\n")
    
    punto <- crear_punto(punto_xy[1], punto_xy[2])
    distancias <- calcular_distancias_punto(punto, datos_sistema$estaciones_sf)
    
    idtr_result <- calcular_idtr_ponderado(
      estaciones_seleccionadas = estaciones,
      tabla_idtr = datos_sistema$idtr,
      metodo = "idw",
      distancias = distancias,
      potencia = potencia_idw
    )
    
  } else {
    # SIMPLE o ÚNICA
    if (length(estaciones) == 1) {
      cat("Método de ponderación: ESTACIÓN ÚNICA\n\n")
    } else {
      cat("Método de ponderación: PROMEDIO SIMPLE\n\n")
    }
    
    idtr_result <- calcular_idtr_ponderado(
      estaciones_seleccionadas = estaciones,
      tabla_idtr = datos_sistema$idtr,
      metodo = "simple"
    )
  }
  
  # Mostrar resultados
  cat("Pesos calculados:\n")
  for (cod in names(idtr_result$pesos)) {
    cat(sprintf("  %s: %.4f\n", cod, idtr_result$pesos[cod]))
  }
  cat("\n")
  
  cat("Idtr ponderado (mm/h):\n")
  for (tr_name in names(idtr_result$idtr_ponderado)) {
    cat(sprintf("  %s: %.3f\n", tr_name, idtr_result$idtr_ponderado[tr_name]))
  }
  cat("\n")
  
  cat("✓ Idtr ponderado calculado\n")
  
  return(idtr_result)
}

#' Calcular precipitación con INAMHI (simplificado)
#'
#' @param zona Zona INAMHI
#' @param TR Vector de periodos de retorno
#' @param duracion_horas Duración en horas
#' @param paso_minutos Paso de tiempo en minutos
#' @param idtr_ponderado Lista con idtr_ponderado (de exploración)
#' @param datos_sistema Datos del sistema
#' @return Lista con precip_total y lista_tablas_idf
calcular_precipitacion_inamhi_completo <- function(zona, TR, duracion_horas, 
                                                   paso_minutos, idtr_ponderado,
                                                   datos_sistema) {
  
  cat("\n==============================================================\n")
  cat("  CÁLCULO DE PRECIPITACIÓN - MÉTODO INAMHI\n")
  cat("==============================================================\n\n")
  
  cat("Zona:", zona, "\n")
  cat("Duración:", duracion_horas, "horas\n")
  cat("Paso:", paso_minutos, "minutos\n\n")
  
  # Mostrar información de ponderación
  cat("Estaciones usadas:\n")
  for (cod in names(idtr_ponderado$pesos)) {
    cat(sprintf("  %s: Peso = %.3f\n", cod, idtr_ponderado$pesos[cod]))
  }
  cat("\n")
  
  cat("Idtr ponderado:\n")
  for (tr_name in names(idtr_ponderado$idtr_ponderado)) {
    cat(sprintf("  %s: %.3f mm/h\n", tr_name, idtr_ponderado$idtr_ponderado[tr_name]))
  }
  cat("\n")
  
  # Calcular precipitaciones totales
  cat("Calculando precipitaciones...\n")
  precip_total <- numeric(length(TR))
  
  for (i in seq_along(TR)) {
    idtr_tr <- idtr_ponderado$idtr_ponderado[paste0("TR", TR[i])]
    precip_total[i] <- calcular_precipitacion_inamhi(
      zona = zona,
      codigo_estacion = NULL,
      TR = TR[i],
      duracion_horas = duracion_horas,
      tabla_parametros = datos_sistema$parametros_inamhi,
      idtr_valor = idtr_tr
    )
  }
  
  cat("\nPrecipitaciones totales:\n")
  for (i in seq_along(TR)) {
    cat(sprintf("  TR = %3d años: P = %6.2f mm\n", TR[i], precip_total[i]))
  }
  
  # Calcular tablas IDF para Bloque Alterno
  cat("\nGenerando tablas IDF para Bloque Alterno...\n")
  lista_tablas_idf <- list()
  
  for (i in seq_along(TR)) {
    tabla_idf <- calcular_tabla_precipitacion_inamhi(
      zona = zona,
      codigo_estacion = NULL,
      TR = TR[i],
      duracion_total_min = duracion_horas * 60,
      paso_min = paso_minutos,
      tabla_parametros = datos_sistema$parametros_inamhi,
      idtr_vector = idtr_ponderado$idtr_ponderado
    )
    lista_tablas_idf[[i]] <- tabla_idf
  }
  
  cat("✓ Tablas IDF generadas\n")
  
  cat("\n✓ Precipitación calculada\n")
  
  return(list(
    precip_total = precip_total,
    lista_tablas_idf = lista_tablas_idf
  ))
}

#' Generar todos los hietogramas (función unificada)
#'
#' @param TR Vector de TR
#' @param precip_total Vector de precipitaciones totales
#' @param lista_tablas_idf Lista de tablas IDF
#' @param duracion_horas Duración
#' @param paso_minutos Paso
#' @param cuartil_huff Cuartil Huff (NULL para automático)
#' @param probabilidad_huff Probabilidad Huff (NULL para automático)
#' @param tipo_scs Tipo SCS (NULL para automático)
#' @return Lista con resultados de los 3 métodos
generar_hietogramas_completo <- function(TR, precip_total, lista_tablas_idf,
                                        duracion_horas, paso_minutos,
                                        cuartil_huff = NULL,
                                        probabilidad_huff = NULL,
                                        tipo_scs = NULL) {
  
  cat("\n==============================================================\n")
  cat("  GENERACIÓN DE HIETOGRAMAS\n")
  cat("==============================================================\n\n")
  
  # Configurar métodos (usar recomendaciones si NULL)
  cat("Configurando métodos...\n")
  parametros <- aplicar_recomendaciones(
    duracion_horas, cuartil_huff, probabilidad_huff, tipo_scs
  )
  
  # MÉTODO 1: HUFF
  cat("\n--- Método 1: HUFF ---\n")
  resultados_huff <- calcular_multiples_huff(
    precip_total, TR, duracion_horas, paso_minutos,
    parametros$cuartil_huff, parametros$probabilidad_huff
  )
  
  graficar_individuales_por_TR(resultados_huff, 
    paste0("Huff Q", parametros$cuartil_huff, " P", parametros$probabilidad_huff))
  
  plot_huff <- graficar_panel_multiples_TR(resultados_huff,
    paste0("Huff Q", parametros$cuartil_huff, " P", parametros$probabilidad_huff))
  guardar_grafico(plot_huff, "huff_panel.png", directorio = "output/plots", ancho = 12, alto = 8)
  
  # Curvas de masa Huff
  plot_masa_huff <- graficar_curvas_masa_multiples_TR(resultados_huff,
    paste0("Huff Q", parametros$cuartil_huff, " P", parametros$probabilidad_huff))
  guardar_grafico(plot_masa_huff, "huff_curvas_masa.png", directorio = "output/plots", ancho = 12, alto = 8)
  
  exportar_multiples_excel(resultados_huff, "hietogramas_huff.xlsx",
    directorio = "output/tables",
    metodo = paste0("Huff Q", parametros$cuartil_huff))
  
  cat("✓ Huff completado\n")
  
  # MÉTODO 2: SCS
  cat("\n--- Método 2: SCS ---\n")
  resultados_scs <- calcular_multiples_scs(
    precip_total, TR, duracion_horas, paso_minutos,
    parametros$tipo_scs
  )
  
  graficar_individuales_por_TR(resultados_scs, 
    paste0("SCS Tipo ", parametros$tipo_scs))
  
  plot_scs <- graficar_panel_multiples_TR(resultados_scs,
    paste0("SCS Tipo ", parametros$tipo_scs))
  guardar_grafico(plot_scs, "scs_panel.png", directorio = "output/plots", ancho = 12, alto = 8)
  
  # Curvas de masa SCS
  plot_masa_scs <- graficar_curvas_masa_multiples_TR(resultados_scs,
    paste0("SCS Tipo ", parametros$tipo_scs))
  guardar_grafico(plot_masa_scs, "scs_curvas_masa.png", directorio = "output/plots", ancho = 12, alto = 8)
  
  exportar_multiples_excel(resultados_scs, "hietogramas_scs.xlsx",
    directorio = "output/tables",
    metodo = paste0("SCS Tipo ", parametros$tipo_scs))
  
  cat("✓ SCS completado\n")
  
  # MÉTODO 3: BLOQUE ALTERNO
  cat("\n--- Método 3: BLOQUE ALTERNO ---\n")
  resultados_bloque <- calcular_multiples_bloque_alterno(
    lista_tablas_idf, TR, paso_minutos
  )
  
  graficar_individuales_por_TR(resultados_bloque, "Bloque Alterno")
  
  plot_bloque <- graficar_panel_multiples_TR(resultados_bloque, "Bloque Alterno")
  guardar_grafico(plot_bloque, "bloque_alterno_panel.png", directorio = "output/plots", ancho = 12, alto = 8)
  
  # Curvas de masa Bloque Alterno
  plot_masa_bloque <- graficar_curvas_masa_multiples_TR(resultados_bloque, "Bloque Alterno")
  guardar_grafico(plot_masa_bloque, "bloque_alterno_curvas_masa.png", directorio = "output/plots", ancho = 12, alto = 8)
  
  exportar_multiples_excel(resultados_bloque, "hietogramas_bloque_alterno.xlsx",
    directorio = "output/tables",
    metodo = "Bloque Alterno")
  
  cat("✓ Bloque Alterno completado\n")
  
  # COMPARACIÓN PARA TODOS LOS TR
  cat("\n--- Comparación de Métodos ---\n")
  
  for (i in seq_along(TR)) {
    plot_comp <- comparar_metodos(
      TR[i], resultados_huff, resultados_scs, resultados_bloque,
      c(paste0("Huff Q", parametros$cuartil_huff),
        paste0("SCS ", parametros$tipo_scs),
        "Bloque Alterno")
    )
    guardar_grafico(plot_comp, paste0("comparacion_TR", TR[i], ".png"), 
                    directorio = "output/plots", ancho = 14, alto = 6)
    cat(sprintf("  ✓ Comparación TR=%d años\n", TR[i]))
  }
  
  cat("✓ Comparaciones completadas\n")
  
  cat("\n✓ Todos los hietogramas generados\n")
  
  return(list(
    huff = resultados_huff,
    scs = resultados_scs,
    bloque = resultados_bloque,
    parametros = parametros
  ))
}
