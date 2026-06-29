# =============================================================================
# test_inamhi_idf.R - Tests para el motor de cálculo INAMHI
# =============================================================================

source(file.path(test_root, "scripts/utils.R"))
source(file.path(test_root, "scripts/00_inamhi_idf.R"))

# Cargar datos una sola vez para todos los tests del archivo
params <- read.csv(file.path(test_root, "data_sistema/inamhi_parametros.csv"))
idtr   <- read.csv(file.path(test_root, "data_sistema/inamhi_idtr.csv"))

# ---------------------------------------------------------------------------
# calcular_intensidad_inamhi
# ---------------------------------------------------------------------------

test_that("calcular_intensidad_inamhi: fórmula I = K * Idtr * t^n", {
  # Caso base: K=1, n=0, Idtr=1 → I = 1 para cualquier t
  expect_equal(calcular_intensidad_inamhi(60, 1, 1, 0), 1)

  # Zona 2, rango corto (5-42.99 min): K=104.44, n=-0.43
  # t=20 min, Idtr=2.77 (M0003, TR10)
  I_corto <- calcular_intensidad_inamhi(20, 2.77, 104.44, -0.43)
  expect_equal(round(I_corto, 4), round(104.44 * 2.77 * 20^(-0.43), 4))

  # Zona 2, rango largo (42.99-1440 min): K=514.56, n=-0.86
  # t=120 min, Idtr=2.39 (M0003 TR10 corregido) → valor de referencia: 20.0326 mm/h
  I_largo <- calcular_intensidad_inamhi(120, 2.39, 514.56, -0.86)
  expect_equal(round(I_largo, 4), 20.0326)

  # La precipitación para 2 horas (t=120 min) debe ser ~40.07 mm
  P_2h <- I_largo * (120 / 60)
  expect_equal(round(P_2h, 2), 40.07)
})

# ---------------------------------------------------------------------------
# seleccionar_ecuacion_inamhi
# ---------------------------------------------------------------------------

test_that("seleccionar_ecuacion_inamhi: rango corto (5-42.99 min) - Zona 2", {
  ec <- seleccionar_ecuacion_inamhi(params, zona = 2, duracion_min = 20)
  expect_equal(round(ec$K, 2), 104.44)
  expect_equal(round(ec$n, 2), -0.43)
  expect_equal(ec$duracion_inicio, 5)
})

test_that("seleccionar_ecuacion_inamhi: rango largo (42.99-1440 min) - Zona 2", {
  ec <- seleccionar_ecuacion_inamhi(params, zona = 2, duracion_min = 120)
  expect_equal(round(ec$K, 2), 514.56)
  expect_equal(round(ec$n, 2), -0.86)
})

test_that("seleccionar_ecuacion_inamhi: zona inválida lanza error", {
  expect_error(seleccionar_ecuacion_inamhi(params, zona = 99, duracion_min = 60))
})

test_that("seleccionar_ecuacion_inamhi: todas las zonas (1-72) tienen al menos 1 ecuación", {
  for (z in 1:72) {
    ec <- tryCatch(
      seleccionar_ecuacion_inamhi(params, zona = z, duracion_min = 60),
      error = function(e) NULL
    )
    expect_false(is.null(ec), info = paste("Zona", z, "sin ecuación para 60 min"))
  }
})

# ---------------------------------------------------------------------------
# obtener_idtr
# ---------------------------------------------------------------------------

test_that("obtener_idtr: valores exactos de tabla para TRs estándar", {
  expect_equal(obtener_idtr(idtr, "M0003", 2),   1.67)
  expect_equal(obtener_idtr(idtr, "M0003", 5),   2.10)
  expect_equal(obtener_idtr(idtr, "M0003", 10),  2.39)
  expect_equal(obtener_idtr(idtr, "M0003", 25),  2.75)
  expect_equal(obtener_idtr(idtr, "M0003", 50),  3.02)
  expect_equal(obtener_idtr(idtr, "M0003", 100), 3.29)
})

test_that("obtener_idtr: estación no existente lanza error", {
  expect_error(obtener_idtr(idtr, "M9999", 10))
})

test_that("obtener_idtr: TR fuera de rango lanza error", {
  expect_error(obtener_idtr(idtr, "M0003", 1))    # TR=1 < mínimo (2)
  expect_error(obtener_idtr(idtr, "M0003", 200))  # TR=200 > máximo (100)
})

test_that("obtener_idtr: TR=15 interpolado da valor monótono para M0003 y M0002", {
  # M0003 corregido: TR10=2.39, TR25=2.75 → TR=15 debe estar entre ellos
  idtr_15_m3 <- suppressWarnings(obtener_idtr(idtr, "M0003", 15))
  expect_true(idtr_15_m3 > 2.39 && idtr_15_m3 < 2.75)
  expect_equal(round(idtr_15_m3, 3), 2.543)  # valor calculado de referencia

  # M0002 (monótona): TR=15 debe estar entre TR10=2.62 y TR25=2.99
  idtr_15_m2 <- suppressWarnings(obtener_idtr(idtr, "M0002", 15))
  expect_true(idtr_15_m2 > 2.62 && idtr_15_m2 < 2.99)
})

# ---------------------------------------------------------------------------
# calcular_precipitacion_inamhi (función de alto nivel)
# ---------------------------------------------------------------------------

test_that("calcular_precipitacion_inamhi: Zona 2, M0003 (corregido), TR=10, dur=2h", {
  # M0003 TR10 corregido = 2.39 → I=20.0326 mm/h → P=40.07 mm
  P <- calcular_precipitacion_inamhi(
    zona = 2, codigo_estacion = "M0003",
    TR = 10, duracion_horas = 2,
    tabla_parametros = params, tabla_idtr = idtr
  )
  expect_equal(round(P, 2), 40.07)
})

test_that("calcular_precipitacion_inamhi: duración < 5 min (en horas) lanza error", {
  expect_error(
    calcular_precipitacion_inamhi(2, "M0003", 10, duracion_horas = 4/60,
                                  tabla_parametros = params, tabla_idtr = idtr)
  )
})

test_that("calcular_precipitacion_inamhi: precipitación aumenta con TR (M0002, monótona)", {
  # M0002 tiene Idtr monótona: TR2 < TR5 < TR10 < TR25 < TR50 < TR100
  precips <- sapply(c(2, 5, 10, 25, 50, 100), function(tr) {
    calcular_precipitacion_inamhi(1, "M0002", tr, 6,
                                  tabla_parametros = params, tabla_idtr = idtr)
  })
  expect_true(all(diff(precips) > 0), info = "P debe aumentar con TR para M0002")
})

test_that("calcular_precipitacion_inamhi con idtr_valor ignora estacion", {
  # Usando idtr_valor directo: Idtr=2.39 (M0003 TR10 corregido)
  P_directo <- calcular_precipitacion_inamhi(
    zona = 2, codigo_estacion = NULL,
    TR = 10, duracion_horas = 2,
    tabla_parametros = params, idtr_valor = 2.39
  )
  expect_equal(round(P_directo, 2), 40.07)
})
