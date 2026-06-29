# =============================================================================
# test_custom_method.R - Tests para la curva de precipitación personalizada
# =============================================================================

source(file.path(test_root, "scripts/utils.R"))
source(file.path(test_root, "scripts/07_custom_method.R"))

# ---------------------------------------------------------------------------
# Helpers: curvas de test
# ---------------------------------------------------------------------------

curva_lineal_valida <- function() {
  # Curva lineal perfecta: Y = X
  data.frame(X = c(0, 0.25, 0.5, 0.75, 1),
             Y = c(0, 0.25, 0.5, 0.75, 1))
}

curva_convexa_valida <- function() {
  # Curva convexa (concentra lluvia al inicio)
  data.frame(X = c(0, 0.2, 0.5, 0.8, 1),
             Y = c(0, 0.6, 0.85, 0.95, 1))
}

# ---------------------------------------------------------------------------
# normalizar_columnas_curva
# ---------------------------------------------------------------------------

test_that("normalizar_columnas_curva: acepta X e Y en cualquier capitalización", {
  df_upper <- data.frame(X = c(0, 1), Y = c(0, 1))
  df_lower <- data.frame(x = c(0, 1), y = c(0, 1))
  df_mixed <- data.frame(x = c(0, 1), Y = c(0, 1))

  expect_equal(colnames(normalizar_columnas_curva(df_upper)$df), c("X", "Y"))
  expect_equal(colnames(normalizar_columnas_curva(df_lower)$df), c("X", "Y"))
  expect_equal(colnames(normalizar_columnas_curva(df_mixed)$df), c("X", "Y"))
})

test_that("normalizar_columnas_curva: renombra CSV de 2 columnas con nombres arbitrarios", {
  df_arb <- data.frame(tiempo = c(0, 0.5, 1), precip = c(0, 0.4, 1))
  res <- normalizar_columnas_curva(df_arb)
  expect_equal(colnames(res$df), c("X", "Y"))
  expect_false(is.null(res$mensaje))  # debe indicar el renombrado
})

test_that("normalizar_columnas_curva: falla con más de 2 columnas sin X/Y", {
  df_multi <- data.frame(a = 1:3, b = 1:3, c = 1:3)
  res <- normalizar_columnas_curva(df_multi)
  expect_null(res$df)
})

# ---------------------------------------------------------------------------
# validar_curva_personalizada — casos válidos
# ---------------------------------------------------------------------------

test_that("validar_curva_personalizada: curva lineal perfecta es válida", {
  res <- validar_curva_personalizada(curva_lineal_valida())
  expect_true(res$valido)
  expect_length(res$errores, 0)
  expect_false(is.null(res$df))
})

test_that("validar_curva_personalizada: curva convexa es válida", {
  res <- validar_curva_personalizada(curva_convexa_valida())
  expect_true(res$valido)
})

test_that("validar_curva_personalizada: acepta exactamente 3 puntos (mínimo)", {
  df <- data.frame(X = c(0, 0.5, 1), Y = c(0, 0.4, 1))
  expect_true(validar_curva_personalizada(df)$valido)
})

test_that("validar_curva_personalizada: Y con puntos planos (iguales) es válida", {
  # Y puede tener valores repetidos (no decreciente, pero no estrictamente creciente)
  df <- data.frame(X = c(0, 0.3, 0.6, 0.7, 1),
                   Y = c(0, 0.2, 0.2, 0.8, 1))  # Y[2] == Y[3]
  expect_true(validar_curva_personalizada(df)$valido)
})

# ---------------------------------------------------------------------------
# validar_curva_personalizada — errores bloqueantes
# ---------------------------------------------------------------------------

test_that("validar_curva_personalizada: menos de 3 puntos", {
  df2 <- data.frame(X = c(0, 1), Y = c(0, 1))
  res <- validar_curva_personalizada(df2)
  expect_false(res$valido)
  expect_true(any(grepl("3 puntos", res$errores)))
})

test_that("validar_curva_personalizada: primer punto no es (0, 0)", {
  df_bad_start <- data.frame(X = c(0.1, 0.5, 1), Y = c(0.05, 0.5, 1))
  res <- validar_curva_personalizada(df_bad_start)
  expect_false(res$valido)
  expect_true(any(grepl("primer punto", res$errores)))
})

test_that("validar_curva_personalizada: último punto no es (1, 1)", {
  df_bad_end_x <- data.frame(X = c(0, 0.5, 0.9), Y = c(0, 0.5, 1))
  df_bad_end_y <- data.frame(X = c(0, 0.5, 1.0), Y = c(0, 0.5, 0.95))

  expect_false(validar_curva_personalizada(df_bad_end_x)$valido)
  expect_true(any(grepl("último punto", validar_curva_personalizada(df_bad_end_x)$errores)))

  expect_false(validar_curva_personalizada(df_bad_end_y)$valido)
  expect_true(any(grepl("último punto", validar_curva_personalizada(df_bad_end_y)$errores)))
})

test_that("validar_curva_personalizada: X no estrictamente creciente", {
  # Dos filas con el mismo X
  df_dup_x <- data.frame(X = c(0, 0.5, 0.5, 1), Y = c(0, 0.4, 0.6, 1))
  res <- validar_curva_personalizada(df_dup_x)
  expect_false(res$valido)
  expect_true(any(grepl("estrictamente creciente", res$errores)))

  # X decrece
  df_dec_x <- data.frame(X = c(0, 0.8, 0.4, 1), Y = c(0, 0.5, 0.6, 1))
  expect_false(validar_curva_personalizada(df_dec_x)$valido)
})

test_that("validar_curva_personalizada: Y decreciente", {
  df_dec_y <- data.frame(X = c(0, 0.3, 0.6, 1), Y = c(0, 0.6, 0.4, 1))
  res <- validar_curva_personalizada(df_dec_y)
  expect_false(res$valido)
  expect_true(any(grepl("no decreciente", res$errores)))
})

test_that("validar_curva_personalizada: valores fuera de [0, 1]", {
  df_x_neg <- data.frame(X = c(-0.1, 0.5, 1), Y = c(0, 0.5, 1))
  df_y_gt1  <- data.frame(X = c(0, 0.5, 1),   Y = c(0, 0.5, 1.1))
  df_x_gt1  <- data.frame(X = c(0, 0.5, 1.2), Y = c(0, 0.5, 1))

  expect_false(validar_curva_personalizada(df_x_neg)$valido)
  expect_false(validar_curva_personalizada(df_y_gt1)$valido)
  expect_false(validar_curva_personalizada(df_x_gt1)$valido)

  expect_true(any(grepl("\\[0, 1\\]", validar_curva_personalizada(df_x_neg)$errores)))
})

test_that("validar_curva_personalizada: columnas no numéricas", {
  df_char <- data.frame(X = c("0", "0.5", "1"), Y = c("0", "0.5", "1"),
                        stringsAsFactors = FALSE)
  res <- validar_curva_personalizada(df_char)
  expect_false(res$valido)
  expect_true(any(grepl("numérica", res$errores)))
})

test_that("validar_curva_personalizada: informa múltiples errores a la vez", {
  # Primer punto no es (0,0) Y último no es (1,1) → debe reportar ambos errores
  df_doble <- data.frame(X = c(0.1, 0.5, 0.9), Y = c(0.05, 0.5, 0.95))
  res <- validar_curva_personalizada(df_doble)
  expect_false(res$valido)
  expect_gte(length(res$errores), 2)
})

# ---------------------------------------------------------------------------
# validar_curva_personalizada — advertencias no bloqueantes
# ---------------------------------------------------------------------------

test_that("validar_curva_personalizada: NAs que dejan < 3 puntos → inválido con advertencia", {
  # (0,0), (NA,0.3), (0.5,NA), (1,1): al eliminar filas con NA quedan solo (0,0) y (1,1) = 2 puntos
  df_na <- data.frame(X = c(0, NA, 0.5, 1), Y = c(0, 0.3, NA, 1))
  res <- validar_curva_personalizada(df_na)
  expect_false(res$valido)                              # 2 puntos < mínimo de 3
  expect_true(any(grepl("NA", res$advertencias)))       # advierte sobre los NAs removidos
  expect_true(any(grepl("3 puntos", res$errores)))      # error por puntos insuficientes
  expect_null(res$df)                                   # df es NULL cuando no es válido
})

test_that("validar_curva_personalizada: NA removidos, ≥3 puntos restantes son válidos", {
  df_na_ok <- data.frame(X = c(0, NA, 0.4, 0.7, 1), Y = c(0, 0.3, 0.5, 0.8, 1))
  res <- validar_curva_personalizada(df_na_ok)
  # Después de eliminar NA: (0,0), (0.4,0.5), (0.7,0.8), (1,1) → 4 puntos válidos
  expect_true(res$valido)
  expect_equal(nrow(res$df), 4)
  expect_true(any(grepl("NA", res$advertencias)))
})

test_that("validar_curva_personalizada: renombrado de columnas genera advertencia", {
  df_alt <- data.frame(tiempo = c(0, 0.5, 1), fraccion = c(0, 0.4, 1))
  res <- validar_curva_personalizada(df_alt)
  expect_true(res$valido)
  expect_true(any(grepl("renombrad", res$advertencias)))
})

# ---------------------------------------------------------------------------
# calcular_hietograma_curva_personalizada
# ---------------------------------------------------------------------------

test_that("hietograma personalizado: estructura de salida correcta", {
  hiet <- calcular_hietograma_curva_personalizada(80, 6, 30, curva_lineal_valida())

  # Columnas mínimas
  cols <- c("paso", "tiempo_horas", "precip_incr_mm", "precip_acum_mm",
            "precip_acum_frac", "intensidad_mm_h")
  expect_true(all(cols %in% names(hiet)))

  # Número de pasos: 6h*60/30 + 1 = 13
  expect_equal(nrow(hiet), 13)

  # Tiempo
  expect_equal(hiet$tiempo_horas[1], 0)
  expect_equal(max(hiet$tiempo_horas), 6)
})

test_that("hietograma personalizado: suma de incrementales = precip_total", {
  precip <- 120
  hiet <- calcular_hietograma_curva_personalizada(precip, 6, 30, curva_lineal_valida())
  expect_equal(round(sum(hiet$precip_incr_mm), 6), precip)
  expect_equal(round(max(hiet$precip_acum_mm), 6), precip)
})

test_that("hietograma personalizado: incrementos no negativos", {
  hiet <- calcular_hietograma_curva_personalizada(100, 12, 60, curva_convexa_valida())
  expect_true(all(hiet$precip_incr_mm >= 0))
})

test_that("hietograma personalizado: curva lineal → distribución uniforme", {
  # Curva lineal Y=X genera lluvia uniforme (mismo incremento en cada paso)
  hiet <- calcular_hietograma_curva_personalizada(100, 4, 60, curva_lineal_valida())
  incr_sin_cero <- hiet$precip_incr_mm[hiet$paso > 0]
  # Todos los incrementos deben ser iguales (± tolerancia numérica)
  expect_true(max(incr_sin_cero) - min(incr_sin_cero) < 1e-6)
})

test_that("hietograma personalizado: curva convexa concentra lluvia al inicio", {
  hiet <- calcular_hietograma_curva_personalizada(100, 6, 30, curva_convexa_valida())
  mitad <- ceiling(nrow(hiet) / 2)
  frac_primera_mitad <- sum(hiet$precip_incr_mm[1:mitad]) / 100
  expect_true(frac_primera_mitad > 0.5)
})

test_that("hietograma personalizado: lanza error con curva inválida", {
  curva_mala <- data.frame(X = c(0.1, 0.5, 1), Y = c(0.05, 0.5, 1))
  expect_error(
    calcular_hietograma_curva_personalizada(100, 6, 30, curva_mala)
  )
})

test_that("hietograma personalizado: fracción acumulada empieza en 0 y termina en 1", {
  hiet <- calcular_hietograma_curva_personalizada(100, 6, 30, curva_lineal_valida())
  expect_equal(hiet$precip_acum_frac[1], 0)
  expect_equal(round(hiet$precip_acum_frac[nrow(hiet)], 6), 1)
})

# ---------------------------------------------------------------------------
# calcular_multiples_curva_personalizada
# ---------------------------------------------------------------------------

test_that("multiples TR personalizado: una lista con un resultado por TR", {
  precip <- c(50, 80, 110)
  TRs    <- c(10, 50, 100)
  res <- calcular_multiples_curva_personalizada(precip, TRs, 6, 30, curva_lineal_valida())

  expect_equal(length(res), 3)
  expect_equal(names(res), c("TR_10", "TR_50", "TR_100"))
})

test_that("multiples TR personalizado: cada TR conserva su precipitación total", {
  precip <- c(50, 80, 110)
  TRs    <- c(10, 50, 100)
  res <- calcular_multiples_curva_personalizada(precip, TRs, 6, 30, curva_lineal_valida())

  for (i in seq_along(TRs)) {
    hiet <- res[[paste0("TR_", TRs[i])]]
    expect_equal(round(max(hiet$precip_acum_mm), 6), precip[i],
                 info = paste("TR =", TRs[i]))
  }
})

test_that("multiples TR personalizado: vectores de distinto tamaño dan error", {
  expect_error(
    calcular_multiples_curva_personalizada(c(50, 80), c(10), 6, 30, curva_lineal_valida())
  )
})
