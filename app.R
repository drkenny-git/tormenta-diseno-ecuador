# =============================================================================
# app.R - Generador de Hietogramas de Diseño — Ecuador (INAMHI)
# =============================================================================

source("global.R")

# ── Tema ------------------------------------------------------------------
tema_app <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = "#2c7bb6",
  base_font  = font_google("Inter")
)

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- page_sidebar(
  title    = "Hietogramas de Diseño — Ecuador",
  theme    = tema_app,
  lang     = "es",
  fillable = FALSE,   # permite que la página haga scroll normal

  useShinyjs(),

  # ── Sidebar ──────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 320,

    # Toggle modo
    div(
      class = "d-flex align-items-center gap-2 mb-3",
      span("Modo guiado", class = "small fw-semibold text-primary"),
      input_switch("modo_experto", label = NULL, value = FALSE),
      span("Modo experto", class = "text-muted small")
    ),

    hr(),

    # Cuenca de estudio — accordion colapsado por defecto (opcional)
    accordion(
      id   = "cuenca_accordion",
      open = FALSE,
      accordion_panel(
        title = "0. Cuenca de estudio (opcional)",
        value = "cuenca_panel",
        p(class = "text-muted small",
          "Sube el shapefile de tu cuenca comprimido como .zip (debe incluir ",
          ".shp, .dbf, .shx). Se acepta cualquier CRS — se transforma ",
          "automáticamente a UTM 17S (EPSG:32717). Si falta el .prj se asume WGS84."),
        fileInput(
          "cuenca_zip", NULL,
          accept      = ".zip",
          buttonLabel = "Buscar .zip…",
          placeholder = "shapefile + archivos complementarios"
        ),
        uiOutput("cuenca_status_ui")
      )
    ),

    # Zona INAMHI
    card(
      card_header("1. Zona INAMHI"),
      card_body(
        conditionalPanel(
          "!input.modo_experto",
          p(class = "text-muted small",
            "Selecciona la zona de intensidad que corresponde a tu proyecto. ",
            "Puedes identificarla en el mapa o usar la lista.")
        ),
        selectInput("zona", "Zona (1–72):", choices = ZONAS_CHOICES, selected = 1)
      )
    ),

    # Estaciones
    card(
      card_header("2. Estaciones pluviométricas"),
      card_body(
        conditionalPanel(
          "!input.modo_experto",
          p(class = "text-muted small",
            "Selecciona una o más estaciones del INAMHI cercanas a tu cuenca. ",
            "Si seleccionas varias, se ponderará el Idtr automáticamente.")
        ),
        selectizeInput(
          "estaciones", "Estaciones:",
          choices  = setNames(IDTR$CODIGO, paste0(IDTR$CODIGO, " — ", IDTR$ESTACION)),
          multiple = TRUE,
          options  = list(placeholder = "Selecciona estación(es)…")
        ),
        conditionalPanel(
          "input.estaciones.length > 1",
          uiOutput("ponderacion_ui")
        )
      )
    ),

    # Punto de referencia IDW (se muestra dinámicamente desde server)
    uiOutput("idw_punto_card_ui"),

    # Parámetros de tormenta
    card(
      card_header("3. Parámetros de tormenta"),
      card_body(
        conditionalPanel(
          "!input.modo_experto",
          p(class = "text-muted small",
            "Define la duración y el paso de tiempo del hietograma de diseño.")
        ),
        numericInput("duracion_horas", "Duración (horas):",
                     value = 6, min = 0.5, max = 24, step = 0.5),
        numericInput("paso_minutos", "Paso de tiempo (minutos):",
                     value = 10, min = 5, max = 60, step = 5),
        checkboxGroupInput(
          "TR", "Períodos de retorno (años):",
          choices  = setNames(TR_OPCIONES, paste0(TR_OPCIONES, " años")),
          selected = c(10, 25, 100),
          inline   = TRUE
        )
      )
    ),

    # Método de distribución temporal
    card(
      card_header("4. Método"),
      card_body(
        conditionalPanel(
          "!input.modo_experto",
          uiOutput("ayuda_metodo")
        ),
        radioButtons("metodo", NULL, choices = METODOS_CHOICES, selected = "huff"),

        # Opciones Huff
        conditionalPanel(
          "input.metodo == 'huff'",
          conditionalPanel(
            "!input.modo_experto",
            div(class = "text-muted small mb-2",
              "El cuartil se asigna según la duración de la tormenta:",
              tags$ul(class = "mb-1 ps-3",
                tags$li("Q1: t < 6 h"),
                tags$li("Q2: 6 h ≤ t < 12 h"),
                tags$li("Q3: 12 h ≤ t < 24 h"),
                tags$li("Q4: t ≥ 24 h")
              )
            )
          ),
          selectInput("huff_cuartil", "Cuartil:",
                      choices  = HUFF_CUARTILES,
                      selected = huff_cuartil_para_duracion(6)),
          uiOutput("huff_cuartil_aviso"),
          selectInput("huff_prob", "Probabilidad:", choices = HUFF_PROB, selected = 50)
        ),

        # Opciones SCS
        conditionalPanel(
          "input.metodo == 'scs'",
          selectInput("scs_tipo", "Tipo SCS:", choices = SCS_TIPOS, selected = "II"),
          conditionalPanel(
            "!input.modo_experto",
            p(class = "text-warning small",
              "Para duraciones distintas de 24 h se aplica una adaptación propia ",
              "(ventana centrada en el pivote). No existe referencia publicada para este caso.")
          )
        ),

        # Opciones curva personalizada
        conditionalPanel(
          "input.metodo == 'personalizado'",
          fileInput(
            "curva_csv", "Sube tu curva (CSV):",
            accept = ".csv",
            buttonLabel = "Buscar…",
            placeholder = "X, Y (fracciones)"
          ),
          conditionalPanel(
            "!input.modo_experto",
            p(class = "text-muted small",
              "El CSV debe tener columnas X (fracción de tiempo) e Y (fracción de ",
              "precipitación acumulada). Primer punto (0,0), último punto (1,1).")
          ),
          uiOutput("validacion_curva_ui")
        )
      )
    ),

    # Botón calcular
    div(
      class = "d-grid mt-2",
      actionButton("calcular", "Calcular hietograma",
                   class = "btn-primary btn-lg",
                   icon  = icon("calculator"))
    )
  ),

  # ── Panel principal ───────────────────────────────────────────────────────
  layout_columns(
    col_widths = c(12),

    # Mapa
    card(
      card_header("Mapa — Zonas y estaciones INAMHI"),
      card_body(
        padding = 0,
        leafletOutput("mapa", height = "420px")
      )
    ),

    # Resultados (se muestran solo tras calcular)
    uiOutput("panel_resultados"),

    # Bibliografía
    accordion(
      open = FALSE,
      accordion_panel(
        title = "Bibliografía",
        value = "biblio_panel",
        tags$ol(
          class = "small lh-lg mb-0",
          tags$li(
            "Huff, F. A. (1990). ",
            tags$em("Time distributions of heavy rainstorms in Illinois"),
            ". Illinois State Water Survey. ",
            tags$a("Semantic Scholar", href = "https://api.semanticscholar.org/CorpusID:130612777",
                   target = "_blank", rel = "noopener")
          ),
          tags$li(
            "Keifer, C. J., & Chu, H. H. (1957). Synthetic storm pattern for drainage design. ",
            tags$em("Journal of the Hydraulics Division"),
            ", 83(4), 1332-1–1332-25. ",
            tags$a("https://doi.org/10.1061/JYCEAJ.0000104",
                   href = "https://doi.org/10.1061/JYCEAJ.0000104",
                   target = "_blank", rel = "noopener")
          ),
          tags$li(
            "U.S. Department of Agriculture, Soil Conservation Service (SCS). (1986). ",
            tags$em("Urban Hydrology for Small Watersheds"),
            " (Technical Release No. 55, TR-55). Washington, D.C."
          ),
          tags$li(
            "Chow, V. T., Maidment, D. R., & Mays, L. W. (1988). ",
            tags$em("Applied Hydrology"),
            ". McGraw-Hill."
          ),
          tags$li(
            "INAMHI. (2019). ",
            tags$em("Determinación de ecuaciones para el cálculo de intensidades máximas de precipitación"),
            ". Versión 2. Instituto Nacional de Meteorología e Hidrología, Ecuador."
          )
        )
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Estado reactivo -------------------------------------------------------
  rv <- reactiveValues(
    resultado           = NULL,
    curva_valid         = NULL,
    error_calc          = NULL,
    cuenca              = NULL,
    cuenca_error        = NULL,
    zona_sugerida       = NULL,
    estaciones_cercanas = NULL,
    idw_punto_x         = NA_real_,
    idw_punto_y         = NA_real_
  )

  # ── Mapa Leaflet ----------------------------------------------------------
  output$mapa <- renderLeaflet({
    mapa_base <- leaflet() |>
      # Panes con z-index: zonas (390) debajo de markers (overlayPane=400, markerPane=600)
      addMapPane("zonas_pane",   zIndex = 390) |>
      addMapPane("cuenca_pane",  zIndex = 395) |>
      addMapPane("idw_pane",     zIndex = 610) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -78.2, lat = -1.8, zoom = 7)

    if (!is.null(ZONAS)) {
      zonas_wgs84 <- sf::st_transform(ZONAS, 4326)
      mapa_base <- mapa_base |>
        addPolygons(
          data        = zonas_wgs84,
          layerId     = ~ZONA,
          fillColor   = "#2c7bb6",
          fillOpacity = 0.15,
          color       = "#2c7bb6",
          weight      = 1,
          label       = ~paste("Zona", ZONA),
          options     = pathOptions(pane = "zonas_pane"),
          highlightOptions = highlightOptions(
            weight      = 2,
            fillOpacity = 0.35,
            bringToFront = FALSE   # no bringToFront para no saltar sobre markers
          )
        )
    }

    mapa_base |>
      addCircleMarkers(
        data        = sf::st_transform(ESTACIONES_SF, 4326),
        layerId     = ~CODIGO,
        radius      = 5,
        color       = "#d7191c",
        fillColor   = "#d7191c",
        fillOpacity = 0.8,
        stroke      = FALSE,
        label       = ~paste0(CODIGO, " — ", ESTACION),
        clusterOptions = markerClusterOptions(
          polygonOptions = list(
            stroke      = TRUE,
            color       = "#e6550d",
            fillColor   = "#fdae6b",
            fillOpacity = 0.12,
            weight      = 1.5
          )
        )
      ) |>
      addControl(
        html = HTML(paste0(
          '<div style="background:white;padding:8px 10px;border-radius:6px;',
          'font-size:12px;line-height:1.6;box-shadow:0 1px 4px rgba(0,0,0,.25)">',
          '<b>Referencias</b><br>',
          '<span style="display:inline-block;width:14px;height:14px;',
          'background:#1a9641;opacity:.4;border:2px solid #1a9641;',
          'vertical-align:middle;margin-right:5px"></span>Cuenca de estudio<br>',
          '<span style="display:inline-block;width:14px;height:14px;',
          'background:#2c7bb6;opacity:.4;border:1px solid #2c7bb6;',
          'vertical-align:middle;margin-right:5px"></span>Zona de intensidad INAMHI<br>',
          '<span style="display:inline-block;width:14px;height:14px;',
          'background:#fdae61;opacity:.7;border:1.5px solid #f46d43;',
          'vertical-align:middle;margin-right:5px"></span>Zona seleccionada<br>',
          '<span style="display:inline-block;width:12px;height:12px;border-radius:50%;',
          'background:#d7191c;vertical-align:middle;margin-right:5px"></span>',
          'Estación pluviométrica<br>',
          '<span style="display:inline-block;width:10px;height:10px;border-radius:50%;',
          'background:#9933cc;border:2px solid #6600aa;',
          'vertical-align:middle;margin-right:5px"></span>',
          'Punto de referencia IDW<br>',
          '<span style="display:inline-block;width:20px;height:14px;',
          'background:#fdae6b;border:1.5px solid #e6550d;border-radius:3px;',
          'opacity:.7;vertical-align:middle;margin-right:5px"></span>',
          'Contorno de grupo de estaciones',
          '</div>'
        )),
        position = "bottomleft"
      )
  })

  # Clic en zona del mapa → actualiza selector
  observeEvent(input$mapa_shape_click, {
    zona_id <- input$mapa_shape_click$id
    if (!is.null(zona_id)) {
      updateSelectInput(session, "zona", selected = zona_id)
    }
  })

  # Clic en marcador de estación → agrega a la selección
  observeEvent(input$mapa_marker_click, {
    codigo <- input$mapa_marker_click$id
    if (!is.null(codigo)) {
      actual <- input$estaciones
      if (!codigo %in% actual) {
        updateSelectizeInput(session, "estaciones", selected = c(actual, codigo))
      }
    }
  })

  # Clic en el fondo del mapa → captura punto IDW si está en ese modo
  observeEvent(input$mapa_click, {
    req(!is.null(input$idw_modo_punto) && input$idw_modo_punto == "mapa")
    lat <- input$mapa_click$lat
    lng <- input$mapa_click$lng
    tryCatch({
      pto_wgs84 <- sf::st_sfc(sf::st_point(c(lng, lat)), crs = 4326)
      pto_utm   <- sf::st_transform(pto_wgs84, 32717)
      coords    <- sf::st_coordinates(pto_utm)
      rv$idw_punto_x <- round(coords[1, "X"])
      rv$idw_punto_y <- round(coords[1, "Y"])
      updateNumericInput(session, "idw_x", value = rv$idw_punto_x)
      updateNumericInput(session, "idw_y", value = rv$idw_punto_y)
    }, error = function(e) NULL)
  })

  # Sincronizar coords manuales → rv
  observeEvent(list(input$idw_x, input$idw_y), {
    req(!is.null(input$idw_modo_punto) && input$idw_modo_punto == "manual")
    if (!is.na(input$idw_x) && !is.na(input$idw_y)) {
      rv$idw_punto_x <- input$idw_x
      rv$idw_punto_y <- input$idw_y
    }
  }, ignoreInit = TRUE)

  # Mostrar punto IDW en mapa
  observe({
    req(!is.na(rv$idw_punto_x), !is.na(rv$idw_punto_y))
    pto_utm   <- crear_punto(rv$idw_punto_x, rv$idw_punto_y)
    pto_wgs84 <- sf::st_transform(pto_utm, 4326)
    coords    <- sf::st_coordinates(pto_wgs84)
    leafletProxy("mapa") |>
      clearGroup("idw_punto") |>
      addCircleMarkers(
        lng        = coords[1, "X"],
        lat        = coords[1, "Y"],
        group      = "idw_punto",
        radius     = 8,
        color      = "#6600aa",
        fillColor  = "#9933cc",
        fillOpacity = 0.85,
        weight     = 2,
        label      = HTML(sprintf(
          "<b>Punto IDW</b><br>X: %.0f m<br>Y: %.0f m",
          rv$idw_punto_x, rv$idw_punto_y
        )),
        options = pathOptions(pane = "idw_pane")
      )
  })

  # Limpiar punto IDW del mapa cuando se cambia modo
  observeEvent(input$idw_modo_punto, {
    if (!is.null(input$idw_modo_punto) && input$idw_modo_punto == "centroide") {
      rv$idw_punto_x <- NA_real_
      rv$idw_punto_y <- NA_real_
      leafletProxy("mapa") |> clearGroup("idw_punto")
    }
  })

  # Resaltar zona seleccionada en el mapa
  observeEvent(input$zona, {
    leafletProxy("mapa") |>
      clearGroup("zona_seleccionada")

    if (!is.null(ZONAS)) {
      zona_sel <- ZONAS[ZONAS$ZONA == as.integer(input$zona), ]
      if (nrow(zona_sel) > 0) {
        zona_wgs84 <- sf::st_transform(zona_sel, 4326)
        leafletProxy("mapa") |>
          addPolygons(
            data        = zona_wgs84,
            group       = "zona_seleccionada",
            fillColor   = "#fdae61",
            fillOpacity = 0.45,
            color       = "#f46d43",
            weight      = 2,
            options     = pathOptions(pane = "zonas_pane")
          )
      }
    }
  })

  # ── Cuenca: carga del shapefile desde ZIP --------------------------------
  observeEvent(input$cuenca_zip, {
    req(input$cuenca_zip)
    rv$cuenca_error        <- NULL
    rv$cuenca              <- NULL
    rv$zona_sugerida       <- NULL
    rv$estaciones_cercanas <- NULL

    tmp_dir <- file.path(tempdir(), paste0("cuenca_", as.integer(Sys.time())))
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

    tryCatch({
      utils::unzip(input$cuenca_zip$datapath, exdir = tmp_dir)
      shp_files <- list.files(tmp_dir, pattern = "\\.shp$", recursive = TRUE,
                              full.names = TRUE)
      if (length(shp_files) == 0) {
        rv$cuenca_error <- "El ZIP no contiene ningún archivo .shp."
        return()
      }
      cuenca <- sf::st_read(shp_files[1], quiet = TRUE)
      geom_types <- unique(as.character(sf::st_geometry_type(cuenca)))
      if (!any(grepl("POLYGON", geom_types, ignore.case = TRUE))) {
        rv$cuenca_error <- paste0("Se esperaba un polígono; se encontró: ",
                                  paste(geom_types, collapse = ", "), ".")
        return()
      }
      if (is.na(sf::st_crs(cuenca))) sf::st_crs(cuenca) <- 4326
      if (!identical(sf::st_crs(cuenca), sf::st_crs(32717))) {
        cuenca <- sf::st_transform(cuenca, 32717)
      }
      rv$cuenca <- cuenca

    }, error = function(e) {
      rv$cuenca_error <- paste("Error al cargar el shapefile:", conditionMessage(e))
    })
  })

  # ── Cuenca: sugerencias y actualización del mapa -------------------------
  observeEvent(rv$cuenca, {
    req(!is.null(rv$cuenca))

    if (!is.null(ZONAS)) {
      tryCatch({
        rv$zona_sugerida <- determinar_zona_cuenca(rv$cuenca, ZONAS)
        updateSelectInput(session, "zona",
                          selected = rv$zona_sugerida$zona_principal)
      }, error = function(e) NULL)
    }

    tryCatch({
      dists  <- calcular_distancias_cuenca(rv$cuenca, ESTACIONES_SF)
      dentro <- estaciones_dentro_cuenca(rv$cuenca, ESTACIONES_SF)
      rv$estaciones_cercanas <- listar_estaciones_cercanas(
        dists, IDTR, p = 8, estaciones_dentro = dentro
      )
    }, error = function(e) NULL)

    cuenca_wgs84 <- sf::st_transform(rv$cuenca, 4326)
    bb <- sf::st_bbox(cuenca_wgs84)
    leafletProxy("mapa") |>
      clearGroup("cuenca") |>
      addPolygons(
        data        = cuenca_wgs84,
        group       = "cuenca",
        fillColor   = "#1a9641",
        fillOpacity = 0.25,
        color       = "#1a9641",
        weight      = 3,
        label       = "Cuenca de estudio",
        options     = pathOptions(pane = "cuenca_pane")
      ) |>
      fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
  })

  # ── Cuenca: limpiar ------------------------------------------------------
  observeEvent(input$limpiar_cuenca, {
    rv$cuenca              <- NULL
    rv$cuenca_error        <- NULL
    rv$zona_sugerida       <- NULL
    rv$estaciones_cercanas <- NULL
    leafletProxy("mapa") |> clearGroup("cuenca")
  })

  # ── Cuenca: agregar estación cercana al hacer clic en la lista -----------
  observeEvent(input$agregar_estacion_cercana, {
    req(input$agregar_estacion_cercana)
    codigo <- input$agregar_estacion_cercana
    actual <- input$estaciones
    if (!codigo %in% actual) {
      updateSelectizeInput(session, "estaciones", selected = c(actual, codigo))
    }
  })

  # ── Cuenca: agregar las 5 más cercanas de golpe --------------------------
  observeEvent(input$agregar_cercanas_btn, {
    req(rv$estaciones_cercanas)
    top5   <- head(rv$estaciones_cercanas$CODIGO, 5)
    nuevas <- unique(c(input$estaciones, top5))
    updateSelectizeInput(session, "estaciones", selected = nuevas)
  })

  # ── Cuenca: UI de estado y sugerencias -----------------------------------
  output$cuenca_status_ui <- renderUI({
    if (!is.null(rv$cuenca_error)) {
      return(div(class = "alert alert-danger py-1 small mt-1",
                 icon("triangle-exclamation"), " ", rv$cuenca_error))
    }
    if (is.null(rv$cuenca)) return(NULL)

    area_km2 <- round(
      as.numeric(sf::st_area(sf::st_union(rv$cuenca))) / 1e6, 2
    )

    tagList(
      div(
        class = "d-flex align-items-center gap-2 mt-1",
        div(class = "alert alert-success py-1 px-2 small mb-0 flex-grow-1",
            icon("check"),
            sprintf(" Cuenca cargada — %.2f km²", area_km2)),
        actionLink("limpiar_cuenca", "× Limpiar", class = "small text-muted")
      ),

      if (!is.null(rv$zona_sugerida)) {
        nz  <- nrow(rv$zona_sugerida$todas_zonas)
        msg <- if (nz > 1) {
          sprintf("Zona dominante: %d (%.0f%% de la cuenca; %d zonas en total)",
                  rv$zona_sugerida$zona_principal,
                  rv$zona_sugerida$porcentaje_principal, nz)
        } else {
          sprintf("Zona: %d (100%% de la cuenca)",
                  rv$zona_sugerida$zona_principal)
        }
        div(class = "alert alert-info py-1 px-2 small mt-1 mb-0",
            icon("location-crosshairs"), " ", msg)
      },

      if (!is.null(rv$estaciones_cercanas)) {
        tagList(
          div(class = "small fw-semibold mt-2 mb-1",
              "Estaciones cercanas (clic para agregar):"),
          tags$div(
            class = "list-group list-group-flush border rounded",
            lapply(seq_len(nrow(rv$estaciones_cercanas)), function(i) {
              est <- rv$estaciones_cercanas[i, ]
              badge <- if (isTRUE(est$dentro_cuenca)) {
                tags$span(class = "badge bg-success fw-normal ms-1", "dentro")
              }
              tags$button(
                type    = "button",
                class   = "list-group-item list-group-item-action py-1 px-2 small",
                onclick = sprintf(
                  "Shiny.setInputValue('agregar_estacion_cercana','%s',{priority:'event'})",
                  est$CODIGO
                ),
                div(
                  class = "d-flex justify-content-between align-items-center",
                  span(paste0(est$CODIGO, " — ", est$ESTACION), badge),
                  span(class = "text-muted ms-2 text-nowrap",
                       paste0(est$distancia_km, " km"))
                )
              )
            })
          ),
          div(
            class = "mt-1",
            actionButton(
              "agregar_cercanas_btn", "Agregar las 5 más cercanas",
              icon  = icon("plus"),
              class = "btn-sm btn-outline-secondary w-100"
            )
          )
        )
      }
    )
  })

  # ── Ponderación: opciones dinámicas (Thiessen solo si hay cuenca) --------
  output$ponderacion_ui <- renderUI({
    choices <- c("IDW" = "idw", "Promedio simple" = "promedio")
    if (!is.null(rv$cuenca)) choices <- c("Thiessen" = "thiessen", choices)
    curr <- if (!is.null(input$ponderacion) && input$ponderacion %in% choices) {
      input$ponderacion
    } else "idw"
    radioButtons("ponderacion", "Método de ponderación:",
                 choices = choices, selected = curr, inline = TRUE)
  })

  # ── IDW punto de referencia (card dinámico) ------------------------------
  output$idw_punto_card_ui <- renderUI({
    n_est      <- length(input$estaciones)
    metodo_p   <- input$ponderacion
    tiene_cuenca <- !is.null(rv$cuenca)

    # Solo mostrar cuando IDW + más de 1 estación + sin cuenca
    if (is.null(metodo_p) || metodo_p != "idw") return(NULL)
    if (n_est <= 1) return(NULL)
    if (tiene_cuenca) return(NULL)

    curr_modo <- {
      m <- isolate(input$idw_modo_punto)
      if (is.null(m)) "centroide" else m
    }
    curr_x <- isolate(rv$idw_punto_x)
    curr_y <- isolate(rv$idw_punto_y)

    card(
      card_header("Punto de referencia IDW"),
      card_body(
        conditionalPanel(
          "!input.modo_experto",
          p(class = "text-muted small",
            "IDW necesita un punto de referencia para calcular las distancias. ",
            "Por defecto usa el centroide geométrico de las estaciones seleccionadas.")
        ),
        radioButtons(
          "idw_modo_punto", NULL,
          choices  = c(
            "Centroide de estaciones" = "centroide",
            "Clic en el mapa"        = "mapa",
            "Coordenadas manuales"   = "manual"
          ),
          selected = curr_modo
        ),
        conditionalPanel(
          "input.idw_modo_punto == 'mapa'",
          div(class = "alert alert-info py-1 px-2 small",
              icon("hand-pointer"),
              " Haz clic en el mapa para ubicar el punto de referencia.")
        ),
        conditionalPanel(
          "input.idw_modo_punto != 'centroide'",
          div(
            class = "row g-1 mt-1",
            div(class = "col-6",
                numericInput("idw_x", "X Este (m):", value = curr_x, step = 1000)),
            div(class = "col-6",
                numericInput("idw_y", "Y Norte (m):", value = curr_y, step = 1000))
          ),
          p(class = "text-muted small mb-0", "UTM Zona 17S — EPSG:32717")
        )
      )
    )
  })

  # ── Validación de curva personalizada ------------------------------------
  curva_df <- reactive({
    req(input$curva_csv)
    tryCatch(
      read.csv(input$curva_csv$datapath, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  })

  observeEvent(curva_df(), {
    df <- curva_df()
    if (!is.null(df)) {
      rv$curva_valid <- validar_curva_personalizada(df)
    }
  })

  output$validacion_curva_ui <- renderUI({
    if (is.null(rv$curva_valid)) return(NULL)
    v <- rv$curva_valid
    if (v$valido) {
      div(class = "alert alert-success py-1 small mt-2",
          icon("check"), " Curva válida —", nrow(v$df), "puntos cargados")
    } else {
      div(class = "alert alert-danger py-1 small mt-2",
          icon("xmark"), " Curva inválida:",
          tags$ul(lapply(v$errores, tags$li)))
    }
  })

  # ── Huff: cuartil recomendado según duración --------------------------------
  cuartil_recomendado <- reactive({
    req(input$duracion_horas)
    req(!is.na(input$duracion_horas))
    huff_cuartil_para_duracion(input$duracion_horas)
  })

  observeEvent(input$duracion_horas, {
    req(!is.na(input$duracion_horas))
    updateSelectInput(session, "huff_cuartil",
                      selected = huff_cuartil_para_duracion(input$duracion_horas))
  }, ignoreInit = TRUE)

  output$huff_cuartil_aviso <- renderUI({
    req(input$metodo == "huff", input$huff_cuartil)
    req(!is.null(input$duracion_horas), !is.na(input$duracion_horas))
    sel <- as.integer(input$huff_cuartil)
    rec <- cuartil_recomendado()
    if (!is.null(rec) && sel != rec) {
      div(class = "alert alert-warning py-1 small mt-1",
          icon("triangle-exclamation"),
          sprintf(" Para %.0f h se recomienda Q%d. Estás usando Q%d.",
                  input$duracion_horas, rec, sel))
    }
  })

  # ── Ayuda de método (modo guiado) ----------------------------------------
  output$ayuda_metodo <- renderUI({
    textos <- list(
      huff = "Huff distribuye la lluvia con base en curvas de masa empíricas por cuartil. ",
      scs  = paste0(
        "SCS (también conocido como NRCS) define 4 tipos de distribución (I, IA, II, III). ",
        "El Tipo II es el más conservador y el más usado en Ecuador."
      ),
      bloque_alterno = paste0(
        "El Bloque Alterno ordena los bloques de intensidad de mayor a menor, ",
        "ubicando el bloque máximo en el centro del hietograma."
      ),
      personalizado = "Usa tu propia curva de distribución temporal (CSV X,Y en fracciones)."
    )
    req(input$metodo)
    p(class = "text-muted small", textos[[input$metodo]])
  })

  # ── Cálculo principal ----------------------------------------------------
  observeEvent(input$calcular, {
    rv$resultado  <- NULL
    rv$error_calc <- NULL

    # Validaciones previas
    if (is.null(input$estaciones) || length(input$estaciones) == 0) {
      rv$error_calc <- "Debes seleccionar al menos una estación pluviométrica."
      return()
    }
    if (is.null(input$TR) || length(input$TR) == 0) {
      rv$error_calc <- "Selecciona al menos un período de retorno."
      return()
    }
    dur_horas <- input$duracion_horas
    paso_min  <- input$paso_minutos
    if (is.null(dur_horas) || is.na(dur_horas) || dur_horas <= 0) {
      rv$error_calc <- "Ingresa un valor válido para la duración (> 0 horas)."
      return()
    }
    if (is.null(paso_min) || is.na(paso_min) || paso_min <= 0) {
      rv$error_calc <- "Ingresa un valor válido para el paso de tiempo (> 0 minutos)."
      return()
    }
    if (input$metodo == "personalizado") {
      if (is.null(rv$curva_valid) || !rv$curva_valid$valido) {
        rv$error_calc <- "La curva personalizada no es válida o no ha sido cargada."
        return()
      }
    }

    TR_sel      <- as.numeric(input$TR)
    zona_sel    <- as.integer(input$zona)
    codigos_sel <- input$estaciones

    withProgress(message = "Calculando…", value = 0, {

      # Paso 1: Idtr ponderado
      incProgress(0.2, detail = "Ponderando estaciones")
      idtr_pond <- tryCatch({
        metodo_pond <- if (length(codigos_sel) > 1 && !is.null(input$ponderacion)) {
          input$ponderacion
        } else "simple"

        if (metodo_pond == "thiessen") {
          if (is.null(rv$cuenca)) stop("Se requiere una cuenca cargada para usar Thiessen.")
          calcular_idtr_ponderado(
            estaciones_seleccionadas = codigos_sel,
            tabla_idtr               = IDTR,
            metodo                   = "thiessen",
            cuenca                   = rv$cuenca,
            estaciones_sf            = ESTACIONES_SF
          )
        } else if (metodo_pond == "idw") {
          # Determinar punto de referencia
          punto_ref <- if (!is.null(rv$cuenca)) {
            # Cuenca disponible: usar centroide de cuenca
            ctrd  <- sf::st_centroid(sf::st_union(rv$cuenca))
            cxy   <- sf::st_coordinates(ctrd)
            crear_punto(cxy[1, "X"], cxy[1, "Y"])
          } else if (!is.null(input$idw_modo_punto) &&
                     input$idw_modo_punto != "centroide" &&
                     !is.na(rv$idw_punto_x) && !is.na(rv$idw_punto_y)) {
            # Punto seleccionado por el usuario (mapa o manual)
            crear_punto(rv$idw_punto_x, rv$idw_punto_y)
          } else {
            # Por defecto: centroide de estaciones seleccionadas
            crear_punto(
              mean(IDTR$X[IDTR$CODIGO %in% codigos_sel]),
              mean(IDTR$Y[IDTR$CODIGO %in% codigos_sel])
            )
          }
          dists <- calcular_distancias_punto(
            punto_ref,
            ESTACIONES_SF[ESTACIONES_SF$CODIGO %in% codigos_sel, ]
          )
          calcular_idtr_ponderado(
            estaciones_seleccionadas = codigos_sel,
            tabla_idtr               = IDTR,
            metodo                   = "idw",
            distancias               = dists
          )
        } else {
          calcular_idtr_ponderado(
            estaciones_seleccionadas = codigos_sel,
            tabla_idtr               = IDTR,
            metodo                   = "simple"
          )
        }
      }, error = function(e) {
        rv$error_calc <<- paste("Error al calcular Idtr:", conditionMessage(e))
        NULL
      })
      if (is.null(idtr_pond)) return()

      # Paso 2: Precipitación INAMHI
      incProgress(0.4, detail = "Aplicando fórmula INAMHI")
      resultado_inamhi <- tryCatch(
        calcular_precipitacion_inamhi_completo(
          zona           = zona_sel,
          TR             = TR_sel,
          duracion_horas = dur_horas,
          paso_minutos   = paso_min,
          idtr_ponderado = idtr_pond,
          datos_sistema  = list(
            parametros_inamhi = PARAMETROS_INAMHI,
            idtr              = IDTR
          )
        ),
        error = function(e) {
          rv$error_calc <<- paste("Error INAMHI:", conditionMessage(e))
          NULL
        }
      )
      if (is.null(resultado_inamhi)) return()

      precip_total <- resultado_inamhi$precip_total
      lista_tablas <- resultado_inamhi$lista_tablas_idf

      # Paso 3: Hietograma según método
      incProgress(0.3, detail = paste("Método:", input$metodo))
      hietogramas <- tryCatch({
        switch(input$metodo,
          huff = calcular_multiples_huff(
            precip_total   = precip_total,
            TR             = TR_sel,
            duracion_horas = dur_horas,
            paso_minutos   = paso_min,
            cuartil        = as.integer(input$huff_cuartil),
            probabilidad   = as.integer(input$huff_prob)
          ),
          scs = calcular_multiples_scs(
            precip_total   = precip_total,
            TR             = TR_sel,
            duracion_horas = dur_horas,
            paso_minutos   = paso_min,
            tipo_scs       = input$scs_tipo
          ),
          bloque_alterno = calcular_multiples_bloque_alterno(
            lista_tablas = lista_tablas,
            TR           = TR_sel,
            paso_minutos = paso_min
          ),
          personalizado = calcular_multiples_curva_personalizada(
            precip_total   = precip_total,
            TR             = TR_sel,
            duracion_horas = dur_horas,
            paso_minutos   = paso_min,
            curva_df       = rv$curva_valid$df
          )
        )
      }, error = function(e) {
        rv$error_calc <<- paste("Error en hietograma:", conditionMessage(e))
        NULL
      })
      if (is.null(hietogramas)) return()

      incProgress(0.1, detail = "Listo")
      rv$resultado <- list(
        hietogramas  = hietogramas,
        precip_total = precip_total,
        TR           = TR_sel,
        metodo       = input$metodo,
        zona         = zona_sel,
        dur_horas    = dur_horas,
        paso_minutos = paso_min
      )
    })
  })

  # ── Panel de resultados ---------------------------------------------------
  output$panel_resultados <- renderUI({
    if (!is.null(rv$error_calc)) {
      return(
        card(
          card_body(
            div(class = "alert alert-danger",
                icon("triangle-exclamation"), " ", rv$error_calc)
          )
        )
      )
    }

    req(rv$resultado)

    tagList(
      # Tabla resumen de precipitación total
      card(
        card_header("Precipitación total por período de retorno"),
        card_body(tableOutput("tabla_precip"))
      ),

      # Gráfico de hietogramas
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center w-100",
            span("Hietogramas de diseño"),
            div(
              class = "btn-group btn-group-sm",
              downloadButton("dl_png",   "PNG",   class = "btn-outline-primary"),
              downloadButton("dl_excel", "Excel", class = "btn-outline-success")
            )
          )
        ),
        card_body(plotOutput("plot_hietogramas", height = "420px"))
      ),

      # Curvas de masa
      card(
        card_header("Curvas de masa"),
        card_body(plotOutput("plot_curvas_masa", height = "360px"))
      )
    )
  })

  # ── Tabla precipitación --------------------------------------------------
  output$tabla_precip <- renderTable({
    req(rv$resultado)
    data.frame(
      `TR (años)`    = rv$resultado$TR,
      `P total (mm)` = round(rv$resultado$precip_total, 2),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "r")

  # ── Gráfico hietogramas --------------------------------------------------
  output$plot_hietogramas <- renderPlot({
    req(rv$resultado)
    graficar_multiples_hietogramas(
      lista_hietogramas = rv$resultado$hietogramas,
      metodo            = rv$resultado$metodo
    )
  }, res = 120)

  # ── Curvas de masa -------------------------------------------------------
  output$plot_curvas_masa <- renderPlot({
    req(rv$resultado)
    graficar_curvas_masa_multiples_TR(
      lista_hietogramas = rv$resultado$hietogramas,
      metodo            = rv$resultado$metodo
    )
  }, res = 120)

  # ── Descargas ------------------------------------------------------------
  output$dl_png <- downloadHandler(
    filename = function() {
      paste0("hietograma_zona", rv$resultado$zona,
             "_", rv$resultado$metodo, ".png")
    },
    content = function(file) {
      p <- graficar_multiples_hietogramas(
        lista_hietogramas = rv$resultado$hietogramas,
        metodo            = rv$resultado$metodo
      )
      ggplot2::ggsave(file, plot = p, width = 12, height = 7, dpi = 150)
    }
  )

  output$dl_excel <- downloadHandler(
    filename = function() {
      paste0("hietograma_zona", rv$resultado$zona,
             "_", rv$resultado$metodo, ".xlsx")
    },
    content = function(file) {
      exportar_multiples_excel(
        lista_hietogramas = rv$resultado$hietogramas,
        nombre_archivo    = basename(file),   # FIX: era 'file' completo
        directorio        = dirname(file),
        metodo            = rv$resultado$metodo
      )
    }
  )
}

# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui, server)
