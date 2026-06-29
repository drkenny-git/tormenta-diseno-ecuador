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
  title  = "Hietogramas de Diseño — Ecuador",
  theme  = tema_app,
  lang   = "es",

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
          radioButtons(
            "ponderacion", "Método de ponderación:",
            choices  = c("IDW" = "idw", "Promedio simple" = "promedio"),
            inline   = TRUE
          )
        )
      )
    ),

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
          selectInput("huff_cuartil", "Cuartil:", choices = HUFF_CUARTILES),
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
        leafletOutput("mapa", height = "380px")
      )
    ),

    # Resultados (se muestran solo tras calcular)
    uiOutput("panel_resultados")
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Estado reactivo -------------------------------------------------------
  rv <- reactiveValues(
    resultado   = NULL,   # lista con hietogramas calculados
    curva_valid = NULL,   # resultado de validar_curva_personalizada()
    error_calc  = NULL
  )

  # ── Mapa Leaflet ----------------------------------------------------------
  output$mapa <- renderLeaflet({
    mapa_base <- leaflet() |>
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
          highlightOptions = highlightOptions(
            weight      = 2,
            fillOpacity = 0.35,
            bringToFront = TRUE
          )
        )
    }

    mapa_base |>
      addCircleMarkers(
        data        = ESTACIONES_SF,
        layerId     = ~CODIGO,
        radius      = 5,
        color       = "#d7191c",
        fillColor   = "#d7191c",
        fillOpacity = 0.8,
        stroke      = FALSE,
        label       = ~paste0(CODIGO, " — ", ESTACION),
        clusterOptions = markerClusterOptions()
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
            weight      = 2
          )
      }
    }
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
    if (input$metodo == "personalizado") {
      if (is.null(rv$curva_valid) || !rv$curva_valid$valido) {
        rv$error_calc <- "La curva personalizada no es válida o no ha sido cargada."
        return()
      }
    }

    TR_sel       <- as.numeric(input$TR)
    zona_sel     <- as.integer(input$zona)
    codigos_sel  <- input$estaciones
    dur_horas    <- input$duracion_horas
    paso_min     <- input$paso_minutos

    withProgress(message = "Calculando…", value = 0, {

      # Paso 1: Idtr ponderado
      incProgress(0.2, detail = "Ponderando estaciones")
      idtr_pond <- tryCatch({
        if (length(codigos_sel) == 1) {
          obtener_idtr(IDTR, codigos_sel, TR_sel)
        } else {
          metodo_pond <- input$ponderacion
          if (metodo_pond == "idw") {
            dists <- calcular_distancias_punto(
              crear_punto(
                mean(IDTR$X[IDTR$CODIGO %in% codigos_sel]),
                mean(IDTR$Y[IDTR$CODIGO %in% codigos_sel])
              ),
              ESTACIONES_SF[ESTACIONES_SF$CODIGO %in% codigos_sel, ]
            )
            pesos <- calcular_pesos_idw(codigos_sel, dists)
            calcular_idtr_ponderado(
              codigos_sel, IDTR, TR_sel,
              metodo = "idw", pesos = pesos
            )
          } else {
            calcular_idtr_ponderado(codigos_sel, IDTR, TR_sel, metodo = "promedio")
          }
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
          zona     = zona_sel,
          TR       = TR_sel,
          duracion_horas = dur_horas,
          paso_minutos   = paso_min,
          idtr     = idtr_pond,
          datos_sistema  = list(
            parametros_inamhi = PARAMETROS_INAMHI,
            idtr              = IDTR,
            curvas_huff       = CURVAS_HUFF,
            curvas_scs        = CURVAS_SCS
          )
        ),
        error = function(e) {
          rv$error_calc <<- paste("Error INAMHI:", conditionMessage(e))
          NULL
        }
      )
      if (is.null(resultado_inamhi)) return()

      precip_total  <- resultado_inamhi$precip_total
      lista_tablas  <- resultado_inamhi$lista_tablas

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
            probabilidad   = as.integer(input$huff_prob),
            curvas_huff    = CURVAS_HUFF
          ),
          scs = calcular_multiples_scs(
            precip_total   = precip_total,
            TR             = TR_sel,
            duracion_horas = dur_horas,
            paso_minutos   = paso_min,
            tipo_scs       = input$scs_tipo,
            curvas_scs     = CURVAS_SCS
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
        hietogramas   = hietogramas,
        precip_total  = precip_total,
        TR            = TR_sel,
        metodo        = input$metodo,
        zona          = zona_sel,
        dur_horas     = dur_horas,
        paso_minutos  = paso_min
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
      `TR (años)`       = rv$resultado$TR,
      `P total (mm)`    = round(rv$resultado$precip_total, 2),
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
        nombre_archivo    = file,
        directorio        = dirname(file),
        metodo            = rv$resultado$metodo
      )
    }
  )
}

# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui, server)
