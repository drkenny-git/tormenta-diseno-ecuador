# =============================================================================
# 00b_exploracion_espacial.R - Exploración espacial y ponderación de estaciones
# =============================================================================

#' Verificar e instalar paquetes espaciales necesarios
#'
verificar_paquetes_espaciales <- function() {
  paquetes <- c("sf", "leaflet", "htmlwidgets", "lwgeom")
  
  for (pkg in paquetes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(paste("Instalando paquete:", pkg, "\n"))
      install.packages(pkg)
    }
  }
  
  library(sf)
  library(leaflet)
  library(htmlwidgets)
}

#' Cargar shapefile de zonas de intensidad INAMHI
#'
#' @param ruta_shapefile Ruta al shapefile de zonas
#' @return Objeto sf con zonas
cargar_zonas_inamhi <- function(ruta_shapefile = "data/zonas_intensidad.shp") {
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("El paquete 'sf' es necesario. Instálalo con: install.packages('sf')")
  }
  
  if (!file.exists(ruta_shapefile)) {
    stop("No se encuentra el archivo de zonas: ", ruta_shapefile)
  }
  
  zonas <- sf::st_read(ruta_shapefile, quiet = TRUE)
  
  # Asegurar que tenga CRS (UTM 17S)
  if (is.na(sf::st_crs(zonas))) {
    sf::st_crs(zonas) <- 32717  # EPSG:32717 = WGS84 / UTM zone 17S
  }
  
  return(zonas)
}

#' Crear objeto sf de estaciones desde tabla Idtr
#'
#' @param tabla_idtr Data frame con columnas X, Y, CODIGO, ESTACION, TR*
#' @return Objeto sf con puntos de estaciones
crear_estaciones_sf <- function(tabla_idtr) {
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("El paquete 'sf' es necesario")
  }
  
  estaciones_sf <- sf::st_as_sf(
    tabla_idtr,
    coords = c("X", "Y"),
    crs = 32717  # UTM 17S
  )
  
  return(estaciones_sf)
}

#' Cargar cuenca desde shapefile
#'
#' @param ruta_shapefile Ruta al shapefile de cuenca
#' @return Objeto sf con polígono de cuenca
cargar_cuenca <- function(ruta_shapefile) {
  
  if (!file.exists(ruta_shapefile)) {
    stop("No se encuentra el archivo de cuenca: ", ruta_shapefile)
  }
  
  cuenca <- sf::st_read(ruta_shapefile, quiet = TRUE)
  
  # Transformar a UTM 17S si no está en ese CRS
  if (sf::st_crs(cuenca)$epsg != 32717) {
    cuenca <- sf::st_transform(cuenca, 32717)
  }
  
  return(cuenca)
}

#' Crear punto desde coordenadas UTM
#'
#' @param x Coordenada X (UTM 17S)
#' @param y Coordenada Y (UTM 17S)
#' @return Objeto sf con punto
crear_punto <- function(x, y) {
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("El paquete 'sf' es necesario")
  }
  
  punto <- sf::st_sfc(sf::st_point(c(x, y)), crs = 32717)
  punto_sf <- sf::st_sf(geometry = punto)
  
  return(punto_sf)
}

#' Determinar zona de una cuenca (zona con mayor área)
#'
#' @param cuenca Objeto sf de cuenca
#' @param zonas Objeto sf de zonas
#' @return Lista con zona principal y porcentajes
determinar_zona_cuenca <- function(cuenca, zonas) {
  
  # Calcular intersección
  interseccion <- sf::st_intersection(cuenca, zonas)
  
  # Calcular áreas
  interseccion$area_intersect <- sf::st_area(interseccion)
  
  # Agrupar por zona y sumar áreas
  areas_por_zona <- aggregate(
    area_intersect ~ ZONA,  # Ajustar nombre de columna según shapefile
    data = interseccion,
    FUN = sum
  )
  
  # Calcular porcentajes
  area_total <- sum(areas_por_zona$area_intersect)
  areas_por_zona$porcentaje <- as.numeric(areas_por_zona$area_intersect / area_total * 100)
  
  # Ordenar por área
  areas_por_zona <- areas_por_zona[order(-areas_por_zona$porcentaje), ]
  
  # Zona principal
  zona_principal <- areas_por_zona$ZONA[1]
  porcentaje_principal <- areas_por_zona$porcentaje[1]
  
  return(list(
    zona_principal = zona_principal,
    porcentaje_principal = porcentaje_principal,
    todas_zonas = areas_por_zona
  ))
}

#' Determinar zona de un punto
#'
#' @param punto Objeto sf de punto
#' @param zonas Objeto sf de zonas
#' @return Número de zona
determinar_zona_punto <- function(punto, zonas) {
  
  # Encontrar en qué zona está el punto
  interseccion <- sf::st_intersection(punto, zonas)
  
  if (nrow(interseccion) == 0) {
    stop("El punto no cae dentro de ninguna zona")
  }
  
  zona <- interseccion$ZONA[1]
  
  return(zona)
}

#' Calcular distancias desde cuenca (centroide) a estaciones
#'
#' @param cuenca Objeto sf de cuenca
#' @param estaciones_sf Objeto sf de estaciones
#' @return Data frame con CODIGO y distancia_m
calcular_distancias_cuenca <- function(cuenca, estaciones_sf) {
  
  # Calcular centroide de la cuenca
  centroide <- sf::st_centroid(sf::st_union(cuenca))
  
  # Calcular distancias
  distancias <- sf::st_distance(centroide, estaciones_sf)
  
  # Crear data frame
  resultado <- data.frame(
    CODIGO = estaciones_sf$CODIGO,
    distancia_m = as.numeric(distancias[1, ])
  )
  
  return(resultado)
}

#' Calcular distancias desde punto a estaciones
#'
#' @param punto Objeto sf de punto
#' @param estaciones_sf Objeto sf de estaciones
#' @return Data frame con CODIGO y distancia_m
calcular_distancias_punto <- function(punto, estaciones_sf) {
  
  # Calcular distancias
  distancias <- sf::st_distance(punto, estaciones_sf)
  
  # Crear data frame
  resultado <- data.frame(
    CODIGO = estaciones_sf$CODIGO,
    distancia_m = as.numeric(distancias[1, ])
  )
  
  return(resultado)
}

#' Identificar estaciones dentro de la cuenca
#'
#' @param cuenca Objeto sf de cuenca
#' @param estaciones_sf Objeto sf de estaciones
#' @return Vector de códigos de estaciones dentro
estaciones_dentro_cuenca <- function(cuenca, estaciones_sf) {
  
  # Verificar qué estaciones están dentro
  dentro <- sf::st_intersects(estaciones_sf, cuenca, sparse = FALSE)
  
  # Extraer códigos
  codigos_dentro <- estaciones_sf$CODIGO[dentro[, 1]]
  
  return(codigos_dentro)
}

#' Listar estaciones más cercanas
#'
#' @param distancias Data frame con CODIGO y distancia_m
#' @param tabla_idtr Data frame completo de Idtr
#' @param p Número de estaciones a retornar
#' @param estaciones_dentro Vector de códigos dentro de cuenca (opcional)
#' @return Data frame con estaciones ordenadas
listar_estaciones_cercanas <- function(distancias, tabla_idtr, p = 10, 
                                      estaciones_dentro = NULL) {
  
  # Ordenar por distancia
  distancias <- distancias[order(distancias$distancia_m), ]
  
  # Tomar las p más cercanas
  distancias_top <- distancias[1:min(p, nrow(distancias)), ]
  
  # Unir con información de estaciones
  resultado <- merge(distancias_top, tabla_idtr, by = "CODIGO")
  
  # Agregar columna de si está dentro
  if (!is.null(estaciones_dentro)) {
    resultado$dentro_cuenca <- resultado$CODIGO %in% estaciones_dentro
  } else {
    resultado$dentro_cuenca <- FALSE
  }
  
  # Ordenar por distancia
  resultado <- resultado[order(resultado$distancia_m), ]
  
  # Redondear distancia a km
  resultado$distancia_km <- round(resultado$distancia_m / 1000, 2)
  
  return(resultado)
}

#' Calcular polígonos de Thiessen
#'
#' @param estaciones_seleccionadas Vector de códigos de estaciones
#' @param estaciones_sf Objeto sf con todas las estaciones
#' @param cuenca Objeto sf de cuenca
#' @return Lista con polígonos y pesos
calcular_thiessen <- function(estaciones_seleccionadas, estaciones_sf, cuenca) {
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("El paquete 'sf' es necesario")
  }
  
  # Filtrar estaciones seleccionadas
  estaciones_sel_sf <- estaciones_sf[estaciones_sf$CODIGO %in% estaciones_seleccionadas, ]
  
  if (nrow(estaciones_sel_sf) < 2) {
    stop("Se necesitan al menos 2 estaciones para calcular polígonos de Thiessen")
  }
  
  # Crear polígonos de Thiessen usando st_voronoi
  # Primero unir todos los puntos
  puntos_union <- sf::st_union(estaciones_sel_sf)
  
  # Crear polígonos de Voronoi (Thiessen)
  voronoi <- sf::st_voronoi(puntos_union)
  
  # Convertir a sf y separar polígonos
  voronoi_sf <- sf::st_collection_extract(sf::st_sf(geometry = voronoi))
  
  # Asignar cada polígono a su estación correspondiente
  # Encontrar qué estación está en cada polígono
  estaciones_en_voronoi <- sf::st_intersects(voronoi_sf, estaciones_sel_sf)
  
  # Crear data frame con polígonos y códigos
  voronoi_con_codigo <- voronoi_sf
  voronoi_con_codigo$CODIGO <- sapply(estaciones_en_voronoi, function(x) {
    if (length(x) > 0) estaciones_sel_sf$CODIGO[x[1]] else NA
  })
  
  # Eliminar polígonos sin estación
  voronoi_con_codigo <- voronoi_con_codigo[!is.na(voronoi_con_codigo$CODIGO), ]
  
  # Intersectar con cuenca
  thiessen_cuenca <- sf::st_intersection(voronoi_con_codigo, cuenca)
  
  # Calcular áreas
  thiessen_cuenca$area <- sf::st_area(thiessen_cuenca)
  
  # Calcular pesos
  area_total <- sum(thiessen_cuenca$area)
  thiessen_cuenca$peso <- as.numeric(thiessen_cuenca$area / area_total)
  
  # Crear vector de pesos por código
  pesos <- setNames(thiessen_cuenca$peso, thiessen_cuenca$CODIGO)
  
  return(list(
    poligonos = thiessen_cuenca,
    pesos = pesos
  ))
}

#' Calcular pesos por Inverso de la Distancia (IDW)
#'
#' @param estaciones_seleccionadas Vector de códigos
#' @param distancias Data frame con CODIGO y distancia_m
#' @param potencia Potencia para IDW (default = 2)
#' @return Vector nombrado con pesos
calcular_pesos_idw <- function(estaciones_seleccionadas, distancias, potencia = 2) {
  
  # Filtrar distancias de estaciones seleccionadas
  dist_sel <- distancias[distancias$CODIGO %in% estaciones_seleccionadas, ]
  
  # Calcular inverso de distancia
  inv_dist <- 1 / (dist_sel$distancia_m ^ potencia)
  
  # Normalizar (pesos suman 1)
  pesos <- inv_dist / sum(inv_dist)
  
  # Crear vector nombrado
  pesos_vector <- setNames(pesos, dist_sel$CODIGO)
  
  return(pesos_vector)
}

#' Calcular Idtr ponderado (FUNCIÓN PRINCIPAL)
#'
#' @param estaciones_seleccionadas Vector de códigos de estaciones
#' @param tabla_idtr Data frame con Idtr por estación
#' @param metodo "thiessen", "idw", o "simple"
#' @param cuenca Objeto sf de cuenca (para thiessen)
#' @param estaciones_sf Objeto sf de estaciones (para thiessen)
#' @param distancias Data frame con distancias (para idw)
#' @param potencia Potencia para IDW (default = 2)
#' @return Lista con idtr_ponderado, pesos, metodo_usado
calcular_idtr_ponderado <- function(estaciones_seleccionadas,
                                   tabla_idtr,
                                   metodo = "simple",
                                   cuenca = NULL,
                                   estaciones_sf = NULL,
                                   distancias = NULL,
                                   potencia = 2) {
  
  n_estaciones <- length(estaciones_seleccionadas)
  
  # Caso 1: Una sola estación
  if (n_estaciones == 1) {
    estacion <- tabla_idtr[tabla_idtr$CODIGO == estaciones_seleccionadas[1], ]
    
    idtr_pond <- c(
      TR2 = estacion$TR2,
      TR5 = estacion$TR5,
      TR10 = estacion$TR10,
      TR25 = estacion$TR25,
      TR50 = estacion$TR50,
      TR100 = estacion$TR100
    )
    
    pesos <- setNames(1, estaciones_seleccionadas[1])
    
    return(list(
      idtr_ponderado = idtr_pond,
      pesos = pesos,
      metodo_usado = "unica_estacion"
    ))
  }
  
  # Caso 2: Múltiples estaciones
  if (metodo == "thiessen") {
    # Validar inputs
    if (is.null(cuenca) || is.null(estaciones_sf)) {
      stop("Para método 'thiessen' se requieren cuenca y estaciones_sf")
    }
    
    # Calcular polígonos y pesos
    thiessen_result <- calcular_thiessen(estaciones_seleccionadas, estaciones_sf, cuenca)
    pesos <- thiessen_result$pesos
    
  } else if (metodo == "idw") {
    # Validar inputs
    if (is.null(distancias)) {
      stop("Para método 'idw' se requiere data frame de distancias")
    }
    
    # Calcular pesos IDW
    pesos <- calcular_pesos_idw(estaciones_seleccionadas, distancias, potencia)
    
  } else if (metodo == "simple") {
    # Promedio simple: pesos iguales
    pesos <- setNames(rep(1/n_estaciones, n_estaciones), estaciones_seleccionadas)
    
  } else {
    stop("Método no reconocido. Use 'thiessen', 'idw', o 'simple'")
  }
  
  # Calcular Idtr ponderado para cada TR
  TRs <- c("TR2", "TR5", "TR10", "TR25", "TR50", "TR100")
  idtr_pond <- numeric(length(TRs))
  names(idtr_pond) <- TRs
  
  # Obtener códigos con peso (solo los que intersectan)
  codigos_con_peso <- names(pesos)
  n_estaciones_usadas <- length(codigos_con_peso)
  
  if (n_estaciones_usadas == 0) {
    stop("Ninguna estación seleccionada intersecta la cuenca")
  }
  
  # Advertir si algunas estaciones no tienen peso
  if (n_estaciones_usadas < n_estaciones) {
    estaciones_sin_peso <- setdiff(estaciones_seleccionadas, codigos_con_peso)
    cat("\n⚠ ADVERTENCIA: Las siguientes estaciones NO intersectan la cuenca y serán ignoradas:\n")
    for (cod in estaciones_sin_peso) {
      cat(sprintf("  - %s\n", cod))
    }
    cat("\n")
  }
  
  for (tr in TRs) {
    valores <- numeric(n_estaciones_usadas)
    for (i in seq_along(codigos_con_peso)) {
      cod <- codigos_con_peso[i]
      valores[i] <- tabla_idtr[tabla_idtr$CODIGO == cod, tr]
    }
    idtr_pond[tr] <- sum(pesos * valores)
  }
  
  return(list(
    idtr_ponderado = idtr_pond,
    pesos = pesos,
    metodo_usado = metodo
  ))
}

#' Crear mapa interactivo con Leaflet
#'
#' @param zonas Objeto sf de zonas
#' @param estaciones_sf Objeto sf de estaciones
#' @param cuenca Objeto sf de cuenca (opcional)
#' @param punto Objeto sf de punto (opcional)
#' @param output_html Nombre del archivo HTML de salida
#' @return Objeto leaflet
crear_mapa_exploracion <- function(zonas, estaciones_sf, cuenca = NULL, 
                                   punto = NULL, output_html = "mapa_exploracion.html") {
  
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("El paquete 'leaflet' es necesario")
  }
  
  library(leaflet)
  
  # Transformar a WGS84 (EPSG:4326) para Leaflet
  zonas_wgs84 <- sf::st_transform(zonas, 4326)
  estaciones_wgs84 <- sf::st_transform(estaciones_sf, 4326)
  
  # Crear mapa base
  mapa <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
    addProviderTiles(providers$OpenStreetMap, group = "Calles") %>%
    setView(lng = -78.5, lat = -1.4, zoom = 7)
  
  # Agregar cuenca PRIMERO (al fondo)
  if (!is.null(cuenca)) {
    cuenca_wgs84 <- sf::st_transform(cuenca, 4326)
    mapa <- mapa %>%
      addPolygons(
        data = cuenca_wgs84,
        fillColor = "green",
        fillOpacity = 0.3,
        color = "darkgreen",
        weight = 3,
        label = "Cuenca",
        group = "Cuenca"
      )
  }
  
  # Agregar zonas SEGUNDO (encima de cuenca)
  if (!is.null(zonas)) {
    mapa <- mapa %>%
      addPolygons(
        data = zonas_wgs84,
        fillColor = "lightblue",
        fillOpacity = 0.3,
        color = "blue",
        weight = 2,
        label = ~paste("Zona", ZONA),
        group = "Zonas"
      )
  }
  
  # Agregar estaciones AL FINAL (encima de todo)
  mapa <- mapa %>%
    addCircleMarkers(
      data = estaciones_wgs84,
      radius = 6,
      fillColor = "red",
      fillOpacity = 0.8,
      color = "darkred",
      weight = 2,
      popup = ~paste0(
        "<b>", ESTACION, "</b><br>",
        "Código: ", CODIGO, "<br>",
        "TR10: ", TR10, " mm/h<br>",
        "TR50: ", TR50, " mm/h"
      ),
      label = ~CODIGO,
      group = "Estaciones"
    )
  
  # Agregar punto si existe (encima de todo)
  if (!is.null(punto)) {
    punto_wgs84 <- sf::st_transform(punto, 4326)
    mapa <- mapa %>%
      addMarkers(
        data = punto_wgs84,
        label = "Punto de interés",
        group = "Punto"
      )
  }
  
  # Agregar control de capas
  mapa <- mapa %>%
    addLayersControl(
      baseGroups = c("Calles", "Satélite"),
      overlayGroups = c("Zonas", "Estaciones", "Cuenca", "Punto"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # Guardar y mostrar mapa
  if (!is.null(output_html)) {
    # htmlwidgets::saveWidget(mapa, output_html, selfcontained = TRUE)
    # cat("Mapa guardado en:", output_html, "\n")
  }
  
  # Retornar mapa para mostrarlo en viewer
  return(mapa)
}

#' Mostrar resumen de exploración
#'
#' @param zona Número de zona
#' @param estaciones_cercanas Data frame con estaciones
#' @param estaciones_dentro Vector de códigos dentro (opcional)
mostrar_resumen_exploracion <- function(zona, estaciones_cercanas, estaciones_dentro = NULL) {
  
  cat("\n")
  cat("==============================================================\n")
  cat("  RESUMEN DE EXPLORACIÓN ESPACIAL\n")
  cat("==============================================================\n")
  cat("\n")
  
  if (!is.null(zona)) {
    cat("ZONA IDENTIFICADA:", zona, "\n\n")
  }
  
  if (!is.null(estaciones_dentro) && length(estaciones_dentro) > 0) {
    cat("ESTACIONES DENTRO DE LA CUENCA:", length(estaciones_dentro), "\n")
    for (cod in estaciones_dentro) {
      est <- estaciones_cercanas[estaciones_cercanas$CODIGO == cod, ]
      cat(sprintf("  • %s - %s\n", cod, est$ESTACION))
    }
    cat("\n")
  }
  
  cat("ESTACIONES MÁS CERCANAS:\n\n")
  for (i in 1:nrow(estaciones_cercanas)) {
    est <- estaciones_cercanas[i, ]
    dentro_txt <- if (est$dentro_cuenca) "✓ DENTRO" else ""
    cat(sprintf("%2d. %s - %s (%.2f km) %s\n", 
                i, est$CODIGO, est$ESTACION, est$distancia_km, dentro_txt))
  }
  
  cat("\n==============================================================\n\n")
}
