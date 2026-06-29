# =============================================================================
# test_utils.R - Tests para funciones auxiliares (utils.R)
# =============================================================================

source(file.path(test_root, "scripts/utils.R"))

test_that("interpolar_lineal: interpolación lineal básica", {
  expect_equal(interpolar_lineal(c(0, 1), c(0, 100), c(0.5)), 50)
  expect_equal(interpolar_lineal(c(0, 1), c(0, 100), c(0)),   0)
  expect_equal(interpolar_lineal(c(0, 1), c(0, 100), c(1)),   100)
  # rule=2: extrapolación toma el valor del borde
  expect_equal(interpolar_lineal(c(0, 1), c(0, 100), c(-0.5)), 0)
  expect_equal(interpolar_lineal(c(0, 1), c(0, 100), c(1.5)),  100)
})

test_that("interpolar_lineal: punto medio entre tres puntos conocidos", {
  expect_equal(interpolar_lineal(c(0, 1, 2), c(0, 10, 30), c(1.5)), 20)
})

test_that("crear_tiempo_normalizado: longitud y extremos correctos", {
  tnorm <- crear_tiempo_normalizado(6, 60)
  expect_equal(length(tnorm), 7)       # 6*60/60 + 1 = 7
  expect_equal(tnorm[1], 0)
  expect_equal(tnorm[length(tnorm)], 1)

  tnorm2 <- crear_tiempo_normalizado(6, 30)
  expect_equal(length(tnorm2), 13)     # 6*60/30 + 1 = 13

  tnorm3 <- crear_tiempo_normalizado(24, 60)
  expect_equal(length(tnorm3), 25)     # 24*60/60 + 1 = 25
})

test_that("acumulada_a_incremental: diferencias correctas", {
  incr <- acumulada_a_incremental(c(0, 10, 18, 24))
  expect_equal(incr, c(0, 10, 8, 6))

  # La suma de incrementales debe ser igual al valor final
  expect_equal(sum(acumulada_a_incremental(c(5, 15, 22, 30))), 30)
  expect_equal(acumulada_a_incremental(c(5, 15, 22, 30))[1], 5)
})

test_that("tiempo_norm_a_horas: conversión correcta", {
  tnorm <- c(0, 0.5, 1)
  expect_equal(tiempo_norm_a_horas(tnorm, 6), c(0, 3, 6))
  expect_equal(tiempo_norm_a_horas(tnorm, 24), c(0, 12, 24))
})
