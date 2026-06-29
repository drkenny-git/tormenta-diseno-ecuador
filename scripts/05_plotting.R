# =============================================================================
# 05_plotting.R - Funciones para crear gráficos de hietogramas
# =============================================================================

#' Calcular breaks inteligentes para el eje X
#'
#' @param tiempo_horas Vector con los tiempos en horas
#' @param max_labels Número máximo de labels deseados (default: 12)
#' @return Vector con los breaks seleccionados
calcular_breaks_inteligentes <- function(tiempo_horas, max_labels = 12) {
  tiempos_unicos <- sort(unique(tiempo_horas))
  n_puntos <- length(tiempos_unicos)


  # Si hay pocos puntos, mostrar todos
  if (n_puntos <= max_labels) {
    return(tiempos_unicos)
  }

  # Calcular el rango total
  rango <- max(tiempos_unicos) - min(tiempos_unicos)

  # Definir intervalos posibles (en horas)
  intervalos_posibles <- c(0.5, 1, 2, 3, 4, 6, 8, 12, 24)

  # Encontrar el intervalo que dé aproximadamente max_labels o menos

  for (intervalo in intervalos_posibles) {
    n_breaks <- floor(rango / intervalo) + 1
    if (n_breaks <= max_labels) {
      # Generar breaks en múltiplos del intervalo
      inicio <- ceiling(min(tiempos_unicos) / intervalo) * intervalo
      breaks <- seq(from = inicio, to = max(tiempos_unicos), by = intervalo)
      # Asegurar que incluimos el inicio si es 0
      if (min(tiempos_unicos) == 0 || min(tiempos_unicos) < inicio) {
        breaks <- c(min(tiempos_unicos), breaks)
      }
      # Asegurar que incluimos el final
      if (max(tiempos_unicos) > max(breaks)) {
        breaks <- c(breaks, max(tiempos_unicos))
      }
      return(unique(breaks))
    }
  }

  # Si ningún intervalo funciona, usar cuantiles
  indices <- round(seq(1, n_puntos, length.out = max_labels))
  return(tiempos_unicos[indices])
}

#' Graficar hietograma individual
#'
#' @param hietograma Data frame con el hietograma
#' @param TR Periodo de retorno
#' @param metodo Nombre del método usado
#' @param tipo Tipo de gráfico: "barras" o "linea"
#' @return Objeto ggplot
graficar_hietograma <- function(hietograma, TR, metodo, tipo = "barras") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete 'ggplot2' es necesario. Instálalo con: install.packages('ggplot2')")
  }
  
  library(ggplot2)
  
  # Remover el primer punto (tiempo = 0)
  datos <- hietograma[hietograma$paso > 0, ]
  
  # Calcular paso de tiempo
  paso_tiempo <- datos$tiempo_horas[1]
  
  # Ajustar posición de barras: centro debe estar en tiempo - paso/2
  datos$tiempo_centro <- datos$tiempo_horas - paso_tiempo / 2
  
  # Calcular breaks inteligentes
  breaks_x <- calcular_breaks_inteligentes(datos$tiempo_horas)

  if (tipo == "barras") {
    p <- ggplot(datos, aes(x = tiempo_centro, y = precip_incr_mm)) +
      geom_bar(stat = "identity", fill = "steelblue", color = "black", width = paso_tiempo * 0.95) +
      labs(
        title = paste("Hietograma de Diseño -", metodo),
        subtitle = paste("TR =", TR, "años | Ptotal =", round(max(datos$precip_acum_mm), 2), "mm"),
        x = "Tiempo (horas)",
        y = "Precipitación incremental (mm)"
      ) +
      scale_x_continuous(breaks = breaks_x, labels = function(x) round(x, 2)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 11),
        panel.grid.minor = element_blank()
      )
  } else {
    p <- ggplot(datos, aes(x = tiempo_horas, y = intensidad_mm_h)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "steelblue", size = 2) +
      labs(
        title = paste("Curva Intensidad-Tiempo -", metodo),
        subtitle = paste("TR =", TR, "años"),
        x = "Tiempo (horas)",
        y = "Intensidad (mm/h)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 11)
      )
  }
  
  return(p)
}

#' Graficar múltiples hietogramas (todos los TRs)
#'
#' @param lista_hietogramas Lista de data frames con hietogramas por TR
#' @param metodo Nombre del método usado
#' @param tipo Tipo de gráfico: "barras" o "linea"
#' @return Objeto ggplot
graficar_multiples_hietogramas <- function(lista_hietogramas, metodo, tipo = "barras") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete 'ggplot2' es necesario.")
  }
  
  library(ggplot2)
  
  # Combinar todos los hietogramas en un solo data frame
  datos_combinados <- do.call(rbind, lista_hietogramas)
  
  # Remover puntos con paso = 0
  datos_combinados <- datos_combinados[datos_combinados$paso > 0, ]
  
  # Calcular paso de tiempo
  paso_tiempo <- datos_combinados$tiempo_horas[1]
  
  # Ajustar posición de barras: centro debe estar en tiempo - paso/2
  datos_combinados$tiempo_centro <- datos_combinados$tiempo_horas - paso_tiempo / 2
  
  # Convertir TR a factor para mejor visualización
  datos_combinados$TR_factor <- factor(paste("TR =", datos_combinados$TR, "años"),
                                       levels = paste("TR =", sort(unique(datos_combinados$TR)), "años"))

  # Calcular breaks inteligentes
  breaks_x <- calcular_breaks_inteligentes(datos_combinados$tiempo_horas)

  if (tipo == "barras") {
    p <- ggplot(datos_combinados, aes(x = tiempo_centro, y = precip_incr_mm, fill = TR_factor)) +
      geom_bar(stat = "identity", color = "black", position = "dodge", width = paso_tiempo * 0.8) +
      labs(
        title = paste("Hietogramas de Diseño -", metodo),
        subtitle = "Comparación de diferentes periodos de retorno",
        x = "Tiempo (horas)",
        y = "Precipitación incremental (mm)",
        fill = "Periodo de Retorno"
      ) +
      scale_x_continuous(breaks = breaks_x, labels = function(x) round(x, 2)) +
      scale_fill_brewer(palette = "Set1") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 11),
        legend.position = "bottom"
      )
  } else {
    p <- ggplot(datos_combinados, aes(x = tiempo_horas, y = intensidad_mm_h, color = TR_factor, group = TR_factor)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = paste("Curvas Intensidad-Tiempo -", metodo),
        subtitle = "Comparación de diferentes periodos de retorno",
        x = "Tiempo (horas)",
        y = "Intensidad (mm/h)",
        color = "Periodo de Retorno"
      ) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 11),
        legend.position = "bottom"
      )
  }
  
  return(p)
}

#' Graficar curva de masa (precipitación acumulada)
#'
#' @param lista_hietogramas Lista de data frames con hietogramas
#' @param metodo Nombre del método
#' @return Objeto ggplot
graficar_curva_masa <- function(lista_hietogramas, metodo) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete 'ggplot2' es necesario.")
  }
  
  library(ggplot2)
  
  # Combinar todos los hietogramas
  datos_combinados <- do.call(rbind, lista_hietogramas)
  
  # Convertir TR a factor
  datos_combinados$TR_factor <- factor(paste("TR =", datos_combinados$TR, "años"),
                                       levels = paste("TR =", sort(unique(datos_combinados$TR)), "años"))
  
  p <- ggplot(datos_combinados, aes(x = tiempo_horas, y = precip_acum_mm, color = TR_factor, group = TR_factor)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = paste("Curva de Masa -", metodo),
      subtitle = "Precipitación acumulada vs Tiempo",
      x = "Tiempo (horas)",
      y = "Precipitación acumulada (mm)",
      color = "Periodo de Retorno"
    ) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 11),
      legend.position = "bottom"
    )
  
  return(p)
}

#' Graficar curvas de masa para múltiples TR (panel)
#'
#' @param lista_hietogramas Lista de data frames con hietogramas
#' @param metodo Nombre del método
#' @return Objeto ggplot
graficar_curvas_masa_multiples_TR <- function(lista_hietogramas, metodo) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete 'ggplot2' es necesario.")
  }
  
  library(ggplot2)
  
  # Combinar todos los hietogramas
  datos_combinados <- do.call(rbind, lista_hietogramas)
  
  # Convertir TR a factor
  datos_combinados$TR_factor <- factor(paste("TR =", datos_combinados$TR, "años"),
                                       levels = paste("TR =", sort(unique(datos_combinados$TR)), "años"))
  
  p <- ggplot(datos_combinados, aes(x = tiempo_horas, y = precip_acum_mm, color = TR_factor, group = TR_factor)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = paste("Curvas de Masa -", metodo),
      subtitle = "Precipitación acumulada vs Tiempo",
      x = "Tiempo (horas)",
      y = "Precipitación acumulada (mm)",
      color = "Periodo de Retorno"
    ) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 11),
      legend.position = "bottom"
    )
  
  return(p)
}

#' Guardar gráfico en archivo
#'
#' @param grafico Objeto ggplot
#' @param nombre_archivo Nombre del archivo (sin ruta)
#' @param directorio Directorio donde guardar (default: output/plots)
#' @param ancho Ancho en pulgadas
#' @param alto Alto en pulgadas
guardar_grafico <- function(grafico, nombre_archivo, directorio = "output/plots",
                           ancho = 10, alto = 6) {
  
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  ruta_completa <- file.path(directorio, nombre_archivo)
  
  ggplot2::ggsave(
    filename = ruta_completa,
    plot = grafico,
    width = ancho,
    height = alto,
    dpi = 300
  )
  
  cat("Gráfico guardado en:", ruta_completa, "\n")
}

#' Graficar hietogramas individuales por TR (un archivo PNG por TR)
#'
#' @param lista_hietogramas Lista de data frames con hietogramas
#' @param metodo Nombre del método
#' @param directorio Directorio donde guardar
graficar_individuales_por_TR <- function(lista_hietogramas, metodo, 
                                         directorio = "output/plots") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete 'ggplot2' es necesario.")
  }
  
  library(ggplot2)
  
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  cat("\nGenerando gráficos individuales por TR...\n")
  
  for (nombre_tr in names(lista_hietogramas)) {
    hiet <- lista_hietogramas[[nombre_tr]]
    hiet <- hiet[hiet$paso > 0, ]

    # Calcular paso de tiempo
    paso_tiempo <- hiet$tiempo_horas[1]

    # Ajustar posición de barras: centro debe estar en tiempo - paso/2
    hiet$tiempo_centro <- hiet$tiempo_horas - paso_tiempo / 2

    TR_valor <- unique(hiet$TR)[1]
    P_total <- round(max(hiet$precip_acum_mm), 2)

    # Calcular breaks inteligentes
    breaks_x <- calcular_breaks_inteligentes(hiet$tiempo_horas)

    p <- ggplot(hiet, aes(x = tiempo_centro, y = precip_incr_mm)) +
      geom_bar(stat = "identity", fill = "steelblue", color = "black",
               width = diff(hiet$tiempo_horas)[1] * 0.9) +
      labs(
        title = paste("Hietograma de Diseño -", metodo),
        subtitle = paste("TR =", TR_valor, "años | Precipitación total =", P_total, "mm"),
        x = "Tiempo (horas)",
        y = "Precipitación incremental (mm)"
      ) +
      scale_x_continuous(breaks = breaks_x, labels = function(x) round(x, 2)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 11),
        panel.grid.minor = element_blank()
      )
    
    nombre_archivo <- paste0(gsub(" ", "_", tolower(metodo)), "_", nombre_tr, ".png")
    guardar_grafico(p, nombre_archivo, directorio, ancho = 10, alto = 6)
  }
  
  cat("✓", length(lista_hietogramas), "gráficos individuales generados\n")
}

#' Graficar todos los TRs en un panel con misma escala Y
#'
#' @param lista_hietogramas Lista de data frames con hietogramas
#' @param metodo Nombre del método
#' @return Objeto ggplot
graficar_panel_multiples_TR <- function(lista_hietogramas, metodo) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete 'ggplot2' es necesario.")
  }
  
  library(ggplot2)
  
  # Combinar todos los datos
  datos_combinados <- do.call(rbind, lista_hietogramas)
  datos_combinados <- datos_combinados[datos_combinados$paso > 0, ]
  
  # Calcular paso de tiempo
  paso_tiempo <- datos_combinados$tiempo_horas[1]
  
  # Ajustar posición de barras: centro debe estar en tiempo - paso/2
  datos_combinados$tiempo_centro <- datos_combinados$tiempo_horas - paso_tiempo / 2
  
  # Crear etiquetas para facetas
  datos_combinados$TR_label <- paste("TR =", datos_combinados$TR, "años")
  
  # Ordenar por TR
  datos_combinados$TR_label <- factor(
    datos_combinados$TR_label,
    levels = paste("TR =", sort(unique(datos_combinados$TR)), "años")
  )
  
  # Determinar número de filas y columnas para el panel
  n_plots <- length(unique(datos_combinados$TR))
  n_cols <- min(3, n_plots)
  n_rows <- ceiling(n_plots / n_cols)

  # Calcular breaks inteligentes
  breaks_x <- calcular_breaks_inteligentes(datos_combinados$tiempo_horas)

  p <- ggplot(datos_combinados, aes(x = tiempo_centro, y = precip_incr_mm)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "black", width = paso_tiempo * 0.95) +
    facet_wrap(~ TR_label, scales = "free_x", ncol = n_cols) +
    scale_x_continuous(breaks = breaks_x, labels = function(x) round(x, 2)) +
    labs(
      title = paste("Hietogramas de Diseño -", metodo),
      subtitle = "Comparación de periodos de retorno",
      x = "Tiempo (horas)",
      y = "Precipitación incremental (mm)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 11),
      strip.text = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "lightblue", color = "black"),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

#' Crear panel de comparación de métodos (hasta 3 métodos)
#'
#' @param TR_comparar TR específico para comparar
#' @param lista_huff Lista de hietogramas Huff (opcional)
#' @param lista_scs Lista de hietogramas SCS (opcional)
#' @param lista_bloque Lista de hietogramas Bloque Alterno (opcional)
#' @param nombres_metodos Vector con nombres personalizados para cada método (opcional)
#' @return Objeto ggplot
comparar_metodos <- function(TR_comparar, 
                            lista_huff = NULL, 
                            lista_scs = NULL, 
                            lista_bloque = NULL,
                            nombres_metodos = NULL) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete 'ggplot2' es necesario.")
  }
  
  library(ggplot2)
  
  # Lista para almacenar datos
  datos_metodos <- list()
  metodos_usados <- character()
  
  # Procesar cada método si está disponible
  if (!is.null(lista_huff)) {
    huff <- lista_huff[[paste0("TR_", TR_comparar)]]
    if (!is.null(huff)) {
      huff_subset <- huff[huff$paso > 0, c("paso", "tiempo_horas", "precip_incr_mm", "precip_acum_mm", "TR")]
      huff_subset$Metodo <- if (!is.null(nombres_metodos) && length(nombres_metodos) >= 1) nombres_metodos[1] else "Huff"
      datos_metodos[[length(datos_metodos) + 1]] <- huff_subset
      metodos_usados <- c(metodos_usados, huff_subset$Metodo[1])
    }
  }
  
  if (!is.null(lista_scs)) {
    scs <- lista_scs[[paste0("TR_", TR_comparar)]]
    if (!is.null(scs)) {
      scs_subset <- scs[scs$paso > 0, c("paso", "tiempo_horas", "precip_incr_mm", "precip_acum_mm", "TR")]
      scs_subset$Metodo <- if (!is.null(nombres_metodos) && length(nombres_metodos) >= 2) nombres_metodos[2] else "SCS"
      datos_metodos[[length(datos_metodos) + 1]] <- scs_subset
      metodos_usados <- c(metodos_usados, scs_subset$Metodo[1])
    }
  }
  
  if (!is.null(lista_bloque)) {
    bloque <- lista_bloque[[paste0("TR_", TR_comparar)]]
    if (!is.null(bloque)) {
      bloque_subset <- bloque[bloque$paso > 0, c("paso", "tiempo_horas", "precip_incr_mm", "precip_acum_mm", "TR")]
      bloque_subset$Metodo <- if (!is.null(nombres_metodos) && length(nombres_metodos) >= 3) nombres_metodos[3] else "Bloque Alterno"
      datos_metodos[[length(datos_metodos) + 1]] <- bloque_subset
      metodos_usados <- c(metodos_usados, bloque_subset$Metodo[1])
    }
  }
  
  # Verificar que haya al menos un método
  if (length(datos_metodos) == 0) {
    stop("No se proporcionaron datos de ningún método para comparar")
  }
  
  # Combinar todos los datos
  datos <- do.call(rbind, datos_metodos)
  
  # Calcular paso de tiempo
  paso_tiempo <- datos$tiempo_horas[1]
  
  # Ajustar posición de barras: centro debe estar en tiempo - paso/2
  datos$tiempo_centro <- datos$tiempo_horas - paso_tiempo / 2
  
  # Crear factor para ordenar métodos
  datos$Metodo <- factor(datos$Metodo, levels = metodos_usados)
  
  # Definir colores para cada método (tonos azules)
  colores <- c("Huff" = "steelblue", 
               "SCS" = "dodgerblue3", 
               "Bloque Alterno" = "deepskyblue3")
  
  # Si hay nombres personalizados, usar los primeros 3 colores
  if (!is.null(nombres_metodos) && length(metodos_usados) > 0) {
    colores_azules <- c("steelblue", "dodgerblue3", "deepskyblue3")
    colores_usados <- setNames(colores_azules[1:length(metodos_usados)], metodos_usados)
  } else {
    # Usar solo los colores de los métodos presentes
    colores_usados <- colores[names(colores) %in% metodos_usados]
  }
  
  # Determinar número de columnas (máximo 3)
  n_metodos <- length(metodos_usados)
  n_cols <- min(3, n_metodos)

  # Calcular breaks inteligentes
  breaks_x <- calcular_breaks_inteligentes(datos$tiempo_horas)

  p <- ggplot(datos, aes(x = tiempo_centro, y = precip_incr_mm, fill = Metodo)) +
    geom_bar(stat = "identity", color = "black", width = paso_tiempo * 0.95) +
    facet_wrap(~ Metodo, scales = "free_x", ncol = n_cols) +
    scale_x_continuous(breaks = breaks_x, labels = function(x) round(x, 2)) +
    labs(
      title = paste("Comparación de Métodos - TR =", TR_comparar, "años"),
      subtitle = " ",
      x = "Tiempo (horas)",
      y = "Precipitación incremental (mm)"
    ) +
    scale_fill_manual(values = colores_usados) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.title = element_text(size = 11),
      strip.text = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "lightyellow", color = "black"),
      legend.position = "none",  # No necesitamos leyenda con facetas
      panel.grid.minor = element_blank()
    )
  
  return(p)
}
