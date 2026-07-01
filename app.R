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

# ── HTML de leyenda con toggle ─────────────────────────────────────────────
leyenda_html <- HTML(paste0(
  '<div style="background:white;padding:8px 10px;border-radius:6px;',
  'font-size:12px;line-height:1.7;box-shadow:0 1px 4px rgba(0,0,0,.25);min-width:200px">',

  # Encabezado clicable
  '<div style="display:flex;justify-content:space-between;align-items:center;',
  'cursor:pointer;margin-bottom:2px" ',
  'onclick="var x=this.nextElementSibling;var b=this.querySelector(\'.ltgl\');',
  'if(x.style.display===\'none\'){x.style.display=\'\';b.textContent=\'▲\';}',
  'else{x.style.display=\'none\';b.textContent=\'▼\';}">',
  '<b>Referencias</b>',
  '<span class="ltgl" style="font-size:11px;color:#888;margin-left:8px">▲</span>',
  '</div>',

  # Contenido colapsable
  '<div>',
  '<span style="display:inline-block;width:14px;height:14px;',
  'background:#1a9641;opacity:.4;border:2px solid #1a9641;',
  'vertical-align:middle;margin-right:5px"></span>Cuenca de estudio<br>',
  '<span style="display:inline-block;width:14px;height:14px;',
  'background:#2c7bb6;opacity:.4;border:1px solid #2c7bb6;',
  'vertical-align:middle;margin-right:5px"></span>Zona INAMHI<br>',
  '<span style="display:inline-block;width:14px;height:14px;',
  'background:#fdae61;opacity:.7;border:1.5px solid #f46d43;',
  'vertical-align:middle;margin-right:5px"></span>Zona seleccionada<br>',
  '<span style="display:inline-block;width:12px;height:12px;border-radius:50%;',
  'background:#d7191c;vertical-align:middle;margin-right:5px"></span>',
  'Estación pluviométrica<br>',
  '<span style="display:inline-block;width:10px;height:10px;border-radius:50%;',
  'background:#9933cc;border:2px solid #6600aa;',
  'vertical-align:middle;margin-right:5px"></span>',
  'Sitio / punto de referencia<br>',
  '<span style="display:inline-block;width:20px;height:14px;',
  'background:#fdae6b;border:1.5px solid #e6550d;border-radius:3px;',
  'opacity:.7;vertical-align:middle;margin-right:5px"></span>',
  'Contorno de grupo de estaciones',
  '</div>',
  '</div>'
))

# Helper para construir lista de estaciones sugeridas (reutilizada en cuenca y punto)
lista_estaciones_ui <- function(estaciones_df, btn_id = "agregar_cercanas_btn") {
  tagList(
    div(class = "small fw-semibold mt-2 mb-1",
        "Estaciones cercanas (clic para agregar):"),
    tags$div(
      class = "list-group list-group-flush border rounded",
      style = "max-height:200px;overflow-y:auto",
      lapply(seq_len(nrow(estaciones_df)), function(i) {
        est   <- estaciones_df[i, ]
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
        btn_id, "Agregar las 5 más cercanas",
        icon  = icon("plus"),
        class = "btn-sm btn-outline-secondary w-100"
      )
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- page_sidebar(
  title    = "Hietogramas de Diseño — Ecuador",
  theme    = tema_app,
  lang     = "es",
  fillable = FALSE,

  useShinyjs(),
  withMathJax(),

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

    # ── 1. Sitio de estudio ───────────────────────────────────────────────
    card(
      card_header("1. Sitio de estudio"),
      card_body(
        conditionalPanel(
          "!input.modo_experto",
          p(class = "text-muted small mb-2",
            "Comienza definiendo el área de interés. La app sugerirá automáticamente ",
            "la zona INAMHI y las estaciones más cercanas.")
        ),
        radioButtons(
          "sitio_tipo", NULL,
          choices  = c("Cuenca hidrográfica" = "cuenca",
                       "Punto de referencia" = "punto"),
          selected = "cuenca",
          inline   = TRUE
        ),

        # ── Cuenca
        conditionalPanel(
          "input.sitio_tipo == 'cuenca'",
          fileInput(
            "cuenca_zip", NULL,
            accept      = ".zip",
            buttonLabel = "Buscar .zip…",
            placeholder = "shapefile + archivos complementarios"
          ),
          p(class = "text-muted small",
            "ZIP con .shp, .dbf, .shx. Sin .prj se asume WGS84."),
          uiOutput("cuenca_status_ui")
        ),

        # ── Punto de referencia
        conditionalPanel(
          "input.sitio_tipo == 'punto'",
          uiOutput("punto_pick_btn_ui"),
          div(
            class = "row g-1 mt-1",
            div(class = "col-6",
                numericInput("sitio_x", "X Este (m):",  value = NA, step = 1000)),
            div(class = "col-6",
                numericInput("sitio_y", "Y Norte (m):", value = NA, step = 1000))
          ),
          p(class = "text-muted small mb-1", "UTM Zona 17S — EPSG:32717"),
          uiOutput("punto_status_ui")
        )
      )
    ),

    # ── 2. Zona INAMHI ───────────────────────────────────────────────────────
    card(
      card_header("2. Zona INAMHI"),
      card_body(
        conditionalPanel(
          "!input.modo_experto",
          p(class = "text-muted small",
            "Zona de intensidad INAMHI. Se sugiere según el sitio de estudio. ",
            "También puedes elegirla haciendo clic en el mapa.")
        ),
        selectizeInput(
          "zona", "Zona (1–72):",
          choices  = ZONAS_CHOICES,
          selected = 1,
          options  = list(dropdownParent = "body")
        )
      )
    ),

    # ── 3. Estaciones pluviométricas ──────────────────────────────────────────
    card(
      card_header("3. Estaciones pluviométricas"),
      card_body(
        conditionalPanel(
          "!input.modo_experto",
          p(class = "text-muted small",
            "Selecciona estaciones INAMHI cercanas. Haz clic en las sugerencias ",
            "de abajo para agregarlas, o elige manualmente.")
        ),
        uiOutput("estaciones_sugeridas_ui"),
        selectizeInput(
          "estaciones", "Estaciones:",
          choices  = setNames(IDTR$CODIGO,
                              paste0(IDTR$CODIGO, " — ", IDTR$ESTACION)),
          multiple = TRUE,
          options  = list(
            placeholder    = "Selecciona estación(es)…",
            dropdownParent = "body"
          )
        ),
        conditionalPanel(
          "input.estaciones.length > 1",
          uiOutput("ponderacion_ui")
        )
      )
    ),

    # ── 4. Parámetros de tormenta ─────────────────────────────────────────────
    card(
      card_header("4. Parámetros de tormenta"),
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

    # ── 5. Método ─────────────────────────────────────────────────────────────
    card(
      card_header("5. Método"),
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
              "(ventana centrada en el pivote). Sin referencia publicada para este caso.")
          )
        ),

        # Opciones curva personalizada
        conditionalPanel(
          "input.metodo == 'personalizado'",
          fileInput(
            "curva_csv", "Sube tu curva (CSV):",
            accept      = ".csv",
            buttonLabel = "Buscar…",
            placeholder = "X, Y (fracciones)"
          ),
          conditionalPanel(
            "!input.modo_experto",
            p(class = "text-muted small",
              "Columnas X (fracción tiempo) e Y (fracción precipitación acumulada). ",
              "Primer punto (0,0), último punto (1,1).")
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
        leafletOutput("mapa", height = "650px")
      )
    ),

    # Resultados
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
            tags$a("Semantic Scholar",
                   href   = "https://api.semanticscholar.org/CorpusID:130612777",
                   target = "_blank", rel = "noopener")
          ),
          tags$li(
            "Keifer, C. J., & Chu, H. H. (1957). Synthetic storm pattern for drainage design. ",
            tags$em("Journal of the Hydraulics Division"),
            ", 83(4), 1332-1–1332-25. ",
            tags$a("https://doi.org/10.1061/JYCEAJ.0000104",
                   href   = "https://doi.org/10.1061/JYCEAJ.0000104",
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
    resultado          = NULL,
    curva_valid        = NULL,
    error_calc         = NULL,
    # Cuenca
    cuenca             = NULL,
    cuenca_error       = NULL,
    cuenca_estaciones  = NULL,   # data.frame de estaciones cercanas (desde cuenca)
    cuenca_zona        = NULL,   # list con zona_principal etc.
    # Punto de referencia
    punto_estaciones   = NULL,   # data.frame de estaciones cercanas (desde punto)
    punto_zona         = NULL,   # zona sugerida desde punto (integer)
    picking_punto      = FALSE   # TRUE mientras se espera el próximo clic en el mapa
  )

  # ── Mapa Leaflet ----------------------------------------------------------
  output$mapa <- renderLeaflet({
    mapa_base <- leaflet() |>
      addMapPane("zonas_pane",  zIndex = 390) |>
      addMapPane("cuenca_pane", zIndex = 395) |>
      addMapPane("idw_pane",    zIndex = 610) |>
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
            bringToFront = FALSE
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
      addControl(html = leyenda_html, position = "bottomleft")
  })

  # Helper: fija sitio_x/sitio_y a partir de un lat/lng WGS84 del mapa y
  # desarma el modo de selección (un solo clic fija el punto).
  fijar_punto_desde_mapa <- function(lat, lng) {
    tryCatch({
      pto_wgs84 <- sf::st_sfc(sf::st_point(c(lng, lat)), crs = 4326)
      pto_utm   <- sf::st_transform(pto_wgs84, 32717)
      coords    <- sf::st_coordinates(pto_utm)
      # unname(): con una sola fila, coords[1, "X"] devuelve un vector CON
      # NOMBRE ("X" = ...), que jsonlite serializa como objeto {"X": ...} en
      # vez de un número plano — el cliente descarta ese mensaje en silencio.
      updateNumericInput(session, "sitio_x", value = unname(round(coords[1, "X"])))
      updateNumericInput(session, "sitio_y", value = unname(round(coords[1, "Y"])))
    }, error = function(e) NULL)
    rv$picking_punto <- FALSE
  }

  # Botón "Elegir en el mapa" → arma el modo de selección de punto
  observeEvent(input$punto_pick_btn, {
    rv$picking_punto <- TRUE
  })
  observeEvent(input$punto_pick_cancelar, {
    rv$picking_punto <- FALSE
  })

  output$punto_pick_btn_ui <- renderUI({
    if (isTRUE(rv$picking_punto)) {
      div(
        class = "alert alert-info py-1 px-2 small d-flex align-items-center justify-content-between gap-2 mb-0",
        tagList(icon("hand-pointer"), " Haz clic en el mapa para fijar el punto…"),
        actionLink("punto_pick_cancelar", "Cancelar", class = "small")
      )
    } else {
      actionButton("punto_pick_btn", "Elegir punto en el mapa",
                   icon = icon("crosshairs"), class = "btn-outline-primary btn-sm")
    }
  })

  # Clic en zona → si estamos eligiendo el punto de estudio (modo armado), el
  # clic fija el punto (las zonas cubren todo el mapa y absorben el clic, por
  # lo que input$mapa_click casi nunca se dispara). Si no, selecciona la zona.
  observeEvent(input$mapa_shape_click, {
    if (isTRUE(rv$picking_punto)) {
      fijar_punto_desde_mapa(input$mapa_shape_click$lat, input$mapa_shape_click$lng)
      return()
    }
    zona_id <- input$mapa_shape_click$id
    if (!is.null(zona_id)) {
      updateSelectizeInput(session, "zona", selected = zona_id)
    }
  })

  # Clic en marcador de estación → agrega a la selección (o fija el punto si
  # el modo de selección está armado)
  observeEvent(input$mapa_marker_click, {
    if (isTRUE(rv$picking_punto)) {
      fijar_punto_desde_mapa(input$mapa_marker_click$lat, input$mapa_marker_click$lng)
      return()
    }
    codigo <- input$mapa_marker_click$id
    if (!is.null(codigo)) {
      actual <- input$estaciones
      if (!codigo %in% actual) {
        updateSelectizeInput(session, "estaciones", selected = c(actual, codigo))
      }
    }
  })

  # Clic en el fondo del mapa (fuera de cualquier zona) → sitio de estudio
  observeEvent(input$mapa_click, {
    req(isTRUE(rv$picking_punto))
    fijar_punto_desde_mapa(input$mapa_click$lat, input$mapa_click$lng)
  })

  # Resaltar zona seleccionada en el mapa
  observeEvent(input$zona, {
    leafletProxy("mapa") |> clearGroup("zona_seleccionada")
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

  # ── Cuenca: carga del shapefile ──────────────────────────────────────────
  observeEvent(input$cuenca_zip, {
    req(input$cuenca_zip)
    rv$cuenca_error       <- NULL
    rv$cuenca             <- NULL
    rv$cuenca_zona        <- NULL
    rv$cuenca_estaciones  <- NULL

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

  # ── Cuenca: sugerencias de zona y estaciones ────────────────────────────
  observe({
    req(!is.null(rv$cuenca))

    if (!is.null(ZONAS)) {
      tryCatch({
        rv$cuenca_zona <- determinar_zona_cuenca(rv$cuenca, ZONAS)
        if (!is.null(input$sitio_tipo) && input$sitio_tipo == "cuenca") {
          updateSelectizeInput(session, "zona",
                               selected = rv$cuenca_zona$zona_principal)
        }
      }, error = function(e) NULL)
    }

    tryCatch({
      dists  <- calcular_distancias_cuenca(rv$cuenca, ESTACIONES_SF)
      dentro <- estaciones_dentro_cuenca(rv$cuenca, ESTACIONES_SF)
      rv$cuenca_estaciones <- listar_estaciones_cercanas(
        dists, IDTR, p = 8, estaciones_dentro = dentro
      )
    }, error = function(e) NULL)

    # Mostrar polígono en mapa
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

  # ── Punto de referencia: sugerencias de zona y estaciones ───────────────
  observe({
    req(!is.null(input$sitio_tipo) && input$sitio_tipo == "punto")
    req(!is.na(input$sitio_x), !is.na(input$sitio_y))
    req(is.numeric(input$sitio_x), is.numeric(input$sitio_y))
    req(input$sitio_x > 0, input$sitio_y > 0)

    tryCatch({
      pto <- crear_punto(input$sitio_x, input$sitio_y)

      # Sugerir zona
      if (!is.null(ZONAS)) {
        zona_pto <- tryCatch(
          determinar_zona_punto(pto, ZONAS),
          error = function(e) NULL
        )
        rv$punto_zona <- zona_pto
        if (!is.null(zona_pto)) {
          updateSelectizeInput(session, "zona", selected = zona_pto)
        }
      }

      # Sugerir estaciones
      dists <- calcular_distancias_punto(pto, ESTACIONES_SF)
      rv$punto_estaciones <- listar_estaciones_cercanas(dists, IDTR, p = 8)

      # Mostrar marcador en mapa
      pto_wgs84 <- sf::st_transform(pto, 4326)
      coords    <- sf::st_coordinates(pto_wgs84)
      leafletProxy("mapa") |>
        clearGroup("sitio_punto") |>
        addCircleMarkers(
          lng         = unname(coords[1, "X"]),
          lat         = unname(coords[1, "Y"]),
          group       = "sitio_punto",
          radius      = 8,
          color       = "#6600aa",
          fillColor   = "#9933cc",
          fillOpacity = 0.85,
          weight      = 2,
          label       = HTML(sprintf(
            "<b>Sitio de referencia</b><br>X: %.0f m<br>Y: %.0f m",
            input$sitio_x, input$sitio_y
          )),
          options = pathOptions(pane = "idw_pane")
        )

    }, error = function(e) NULL)
  })

  # Limpiar marcador cuando se cambia a modo cuenca
  observeEvent(input$sitio_tipo, {
    if (!is.null(input$sitio_tipo) && input$sitio_tipo == "cuenca") {
      leafletProxy("mapa") |> clearGroup("sitio_punto")
      rv$punto_zona       <- NULL
      rv$punto_estaciones <- NULL
      rv$picking_punto    <- FALSE
    }
    if (!is.null(input$sitio_tipo) && input$sitio_tipo == "punto") {
      rv$cuenca_zona       <- NULL
      rv$cuenca_estaciones <- NULL
    }
  }, ignoreInit = TRUE)

  # ── Cuenca: limpiar ──────────────────────────────────────────────────────
  observeEvent(input$limpiar_cuenca, {
    rv$cuenca            <- NULL
    rv$cuenca_error      <- NULL
    rv$cuenca_zona       <- NULL
    rv$cuenca_estaciones <- NULL
    leafletProxy("mapa") |> clearGroup("cuenca")
  })

  # ── Cuenca: UI de estado y sugerencias ───────────────────────────────────
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

      if (!is.null(rv$cuenca_zona)) {
        nz  <- nrow(rv$cuenca_zona$todas_zonas)
        msg <- if (nz > 1) {
          sprintf("Zona dominante: %d (%.0f%% de la cuenca; %d zonas en total)",
                  rv$cuenca_zona$zona_principal,
                  rv$cuenca_zona$porcentaje_principal, nz)
        } else {
          sprintf("Zona: %d (100%% de la cuenca)",
                  rv$cuenca_zona$zona_principal)
        }
        div(class = "alert alert-info py-1 px-2 small mt-1 mb-0",
            icon("location-crosshairs"), " ", msg)
      }
    )
  })

  # ── Punto: UI de estado y sugerencias ───────────────────────────────────
  output$punto_status_ui <- renderUI({
    req(!is.null(rv$punto_zona))
    div(class = "alert alert-info py-1 px-2 small mt-2 mb-0",
        icon("location-crosshairs"),
        sprintf(" Zona sugerida: %d", rv$punto_zona))
  })

  # ── Estaciones cercanas sugeridas (cuenca o punto) — mostrado justo encima
  # de la sección "3. Estaciones pluviométricas" para que se vea claramente
  # que alimenta esa selección ────────────────────────────────────────────
  output$estaciones_sugeridas_ui <- renderUI({
    est_list <- if (!is.null(input$sitio_tipo) && input$sitio_tipo == "punto") {
      rv$punto_estaciones
    } else {
      rv$cuenca_estaciones
    }
    req(est_list)
    lista_estaciones_ui(est_list)
  })

  # ── Agregar estación cercana al clicar en la lista ───────────────────────
  observeEvent(input$agregar_estacion_cercana, {
    req(input$agregar_estacion_cercana)
    actual <- input$estaciones
    if (!input$agregar_estacion_cercana %in% actual) {
      updateSelectizeInput(session, "estaciones",
                           selected = c(actual, input$agregar_estacion_cercana))
      showNotification(
        sprintf("✓ %s agregada a \"Estaciones pluviométricas\" (sección 3)",
                input$agregar_estacion_cercana),
        type = "message", duration = 3
      )
    }
  })

  # ── Agregar las 5 más cercanas ───────────────────────────────────────────
  observeEvent(input$agregar_cercanas_btn, {
    est_list <- if (!is.null(input$sitio_tipo) && input$sitio_tipo == "punto") {
      rv$punto_estaciones
    } else {
      rv$cuenca_estaciones
    }
    req(est_list)
    top5    <- head(est_list$CODIGO, 5)
    faltan  <- setdiff(top5, input$estaciones)
    nuevas  <- unique(c(input$estaciones, top5))
    updateSelectizeInput(session, "estaciones", selected = nuevas)
    if (length(faltan) > 0) {
      showNotification(
        sprintf("✓ %d estación(es) agregada(s) a \"Estaciones pluviométricas\" (sección 3)",
                length(faltan)),
        type = "message", duration = 3
      )
    } else {
      showNotification(
        "Las estaciones más cercanas ya estaban seleccionadas.",
        type = "warning", duration = 3
      )
    }
  })

  # ── Ponderación: opciones dinámicas ──────────────────────────────────────
  output$ponderacion_ui <- renderUI({
    choices      <- c("IDW" = "idw", "Promedio simple" = "promedio")
    tiene_cuenca <- !is.null(rv$cuenca) &&
                    !is.null(input$sitio_tipo) &&
                    input$sitio_tipo == "cuenca"
    if (tiene_cuenca) choices <- c("Thiessen" = "thiessen", choices)
    curr <- if (!is.null(input$ponderacion) && input$ponderacion %in% choices) {
      input$ponderacion
    } else "idw"
    radioButtons("ponderacion", "Método de ponderación:",
                 choices = choices, selected = curr, inline = TRUE)
  })

  # ── Validación de curva personalizada ───────────────────────────────────
  curva_df <- reactive({
    req(input$curva_csv)
    tryCatch(
      read.csv(input$curva_csv$datapath, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  })

  observeEvent(curva_df(), {
    df <- curva_df()
    if (!is.null(df)) rv$curva_valid <- validar_curva_personalizada(df)
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

  # ── Huff: cuartil recomendado ────────────────────────────────────────────
  cuartil_recomendado <- reactive({
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
    req(!is.na(input$duracion_horas))
    sel <- as.integer(input$huff_cuartil)
    rec <- cuartil_recomendado()
    if (!is.null(rec) && sel != rec) {
      div(class = "alert alert-warning py-1 small mt-1",
          icon("triangle-exclamation"),
          sprintf(" Para %.0f h se recomienda Q%d. Estás usando Q%d.",
                  input$duracion_horas, rec, sel))
    }
  })

  # ── Ayuda de método (modo guiado) ─────────────────────────────────────────
  output$ayuda_metodo <- renderUI({
    textos <- list(
      huff = "Huff distribuye la lluvia con base en curvas de masa empíricas por cuartil.",
      scs  = paste0(
        "SCS (NRCS) define 4 tipos de distribución (I, IA, II, III). ",
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

  # ── Cálculo principal ────────────────────────────────────────────────────
  observeEvent(input$calcular, {
    rv$resultado  <- NULL
    rv$error_calc <- NULL

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
        n_est      <- length(codigos_sel)
        metodo_pond <- if (n_est > 1 && !is.null(input$ponderacion)) {
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
          # Punto de referencia según sitio de estudio
          punto_ref <- if (!is.null(input$sitio_tipo) &&
                           input$sitio_tipo == "cuenca" &&
                           !is.null(rv$cuenca)) {
            ctrd <- sf::st_centroid(sf::st_union(rv$cuenca))
            cxy  <- sf::st_coordinates(ctrd)
            crear_punto(cxy[1, "X"], cxy[1, "Y"])
          } else if (!is.null(input$sitio_tipo) &&
                     input$sitio_tipo == "punto" &&
                     !is.na(input$sitio_x) && !is.na(input$sitio_y)) {
            crear_punto(input$sitio_x, input$sitio_y)
          } else {
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
        paso_minutos = paso_min,
        # Datos adicionales para mostrar en resultados
        idtr_pond    = idtr_pond,
        zona_params  = PARAMETROS_INAMHI[PARAMETROS_INAMHI$ZONA == zona_sel, ],
        codigos_sel  = codigos_sel,
        nombres_est  = IDTR$ESTACION[match(codigos_sel, IDTR$CODIGO)]
      )
    })
  })

  # ── Panel de resultados ──────────────────────────────────────────────────
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
      # Pesos de estaciones
      card(
        card_header("Ponderación de estaciones"),
        card_body(
          p(class = "small text-muted mb-2", textOutput("tabla_pesos_metodo", inline = TRUE)),
          tableOutput("tabla_pesos")
        )
      ),

      # Idtr ponderado
      card(
        card_header("Idtr ponderado (mm/h)"),
        card_body(tableOutput("tabla_idtr_pond"))
      ),

      # Ecuaciones INAMHI de la zona
      card(
        card_header(textOutput("zona_params_header", inline = TRUE)),
        card_body(uiOutput("tabla_ecuaciones"))
      ),

      # Precipitación total por TR
      card(
        card_header("Precipitación total por período de retorno"),
        card_body(tableOutput("tabla_precip"))
      ),

      # Hietogramas
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

  # ── Tabla pesos de estaciones ────────────────────────────────────────────
  # Etiqueta del método de ponderación (se muestra aparte de la tabla: pasar
  # un reactive/función como `caption=` a renderTable rompe xtable con
  # "the condition has length > 1", ya que xtable espera un string plano).
  output$tabla_pesos_metodo <- renderText({
    req(rv$resultado)
    paste0("Método de ponderación: ", switch(rv$resultado$idtr_pond$metodo_usado,
      thiessen = "Thiessen", idw = "IDW", simple = "Promedio simple",
      unica_estacion = "Estación única", rv$resultado$idtr_pond$metodo_usado))
  })

  output$tabla_pesos <- renderTable({
    req(rv$resultado)
    p     <- rv$resultado$idtr_pond
    noms  <- rv$resultado$nombres_est
    cods  <- rv$resultado$codigos_sel
    pesos <- p$pesos[cods]
    data.frame(
      Código     = cods,
      Estación   = noms,
      `Peso (%)` = round(as.numeric(pesos) * 100, 2),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ── Tabla Idtr ponderado ─────────────────────────────────────────────────
  output$tabla_idtr_pond <- renderTable({
    req(rv$resultado)
    idtr   <- rv$resultado$idtr_pond$idtr_ponderado
    TR_sel <- rv$resultado$TR
    idx    <- match(paste0("TR", TR_sel), names(idtr))
    data.frame(
      `TR (años)` = TR_sel,
      `Idtr (mm/h)` = round(as.numeric(idtr[idx]), 3),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "r")

  # ── Encabezado dinámico de ecuaciones ───────────────────────────────────
  output$zona_params_header <- renderText({
    req(rv$resultado)
    paste0("Ecuaciones INAMHI — Zona ", rv$resultado$zona,
           " (I = K · Idtr · t^n)")
  })

  # ── Ecuaciones INAMHI de la zona (renderizadas como fórmula LaTeX/MathJax) ─
  output$tabla_ecuaciones <- renderUI({
    req(rv$resultado)
    zp <- rv$resultado$zona_params
    if (is.null(zp) || nrow(zp) == 0) return(NULL)

    filas <- lapply(seq_len(nrow(zp)), function(i) {
      fila   <- zp[i, ]
      cierre <- if (i == nrow(zp)) "≤" else "<"
      formula_tex <- sprintf(
        "$$I = %.4f \\cdot Idtr \\cdot t^{%.4f}$$",
        fila$K, fila$n
      )
      div(
        class = "mb-3",
        p(class = "small text-muted mb-1",
          sprintf("Para %.0f min ≤ t %s %.2f min (R² = %.4f):",
                  fila$DURACION.INICIO, cierre, fila$DURACION.FIN,
                  fila$R_CUADRADO)),
        formula_tex
      )
    })

    withMathJax(tagList(filas))
  })

  # ── Tabla precipitación total ────────────────────────────────────────────
  output$tabla_precip <- renderTable({
    req(rv$resultado)
    data.frame(
      `TR (años)`    = rv$resultado$TR,
      `P total (mm)` = round(rv$resultado$precip_total, 2),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "r")

  # ── Gráfico hietogramas ──────────────────────────────────────────────────
  output$plot_hietogramas <- renderPlot({
    req(rv$resultado)
    graficar_multiples_hietogramas(
      lista_hietogramas = rv$resultado$hietogramas,
      metodo            = rv$resultado$metodo
    )
  }, res = 120)

  # ── Curvas de masa ───────────────────────────────────────────────────────
  output$plot_curvas_masa <- renderPlot({
    req(rv$resultado)
    graficar_curvas_masa_multiples_TR(
      lista_hietogramas = rv$resultado$hietogramas,
      metodo            = rv$resultado$metodo
    )
  }, res = 120)

  # ── Descargas ────────────────────────────────────────────────────────────
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
        nombre_archivo    = basename(file),
        directorio        = dirname(file),
        metodo            = rv$resultado$metodo
      )
    }
  )
}

# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui, server)
