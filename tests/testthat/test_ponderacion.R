# =============================================================================
# test_ponderacion.R - Tests para ponderación de estaciones (Thiessen/IDW/Simple)
# =============================================================================

source(file.path(test_root, "scripts/utils.R"))
source(file.path(test_root, "scripts/00_inamhi_idf.R"))

# Necesitamos solo la parte no-espacial de exploracion_espacial
# Cargamos las funciones de ponderación directamente
source(file.path(test_root, "scripts/00b_exploracion_espacial.R"))

idtr <- read.csv(file.path(test_root, "data_sistema/inamhi_idtr.csv"))

# ---------------------------------------------------------------------------
# calcular_idtr_ponderado - método "simple"
# ---------------------------------------------------------------------------

test_that("calcular_idtr_ponderado simple: una sola estación → peso = 1", {
  res <- calcular_idtr_ponderado(
    estaciones_seleccionadas = c("M0003"),
    tabla_idtr = idtr, metodo = "simple"
  )
  expect_equal(res$metodo_usado, "unica_estacion")
  expect_equal(unname(res$pesos["M0003"]), 1)
  expect_equal(unname(res$idtr_ponderado["TR10"]), 2.39)  # M0003 corregido
  expect_equal(unname(res$idtr_ponderado["TR2"]),  1.67)
})

test_that("calcular_idtr_ponderado simple: dos estaciones → promedio aritmético", {
  # M0002 TR10 = 2.62, M0003 TR10 = 2.39 (corregido) → promedio = 2.505
  res <- calcular_idtr_ponderado(
    estaciones_seleccionadas = c("M0002", "M0003"),
    tabla_idtr = idtr, metodo = "simple"
  )
  expect_equal(unname(res$pesos["M0002"]), 0.5)
  expect_equal(unname(res$pesos["M0003"]), 0.5)
  expect_equal(round(unname(res$idtr_ponderado["TR10"]), 4),
               round((2.62 + 2.39) / 2, 4))
})

test_that("calcular_idtr_ponderado simple: pesos suman 1", {
  res <- calcular_idtr_ponderado(
    estaciones_seleccionadas = c("M0001", "M0002", "M0003"),
    tabla_idtr = idtr, metodo = "simple"
  )
  expect_equal(round(sum(res$pesos), 10), 1)
})

# ---------------------------------------------------------------------------
# calcular_pesos_idw
# ---------------------------------------------------------------------------

test_that("calcular_pesos_idw: pesos suman 1 y son proporcionales a 1/d^p", {
  # Crear distancias ficticias controladas
  distancias <- data.frame(
    CODIGO = c("M0001", "M0002", "M0003"),
    distancia_m = c(10, 20, 40)
  )

  pesos <- calcular_pesos_idw(c("M0001", "M0002", "M0003"), distancias, potencia = 2)

  # Los pesos deben sumar 1
  expect_equal(round(sum(pesos), 10), 1)

  # M0001 está más cerca → debe tener el peso mayor
  expect_true(pesos["M0001"] > pesos["M0002"])
  expect_true(pesos["M0002"] > pesos["M0003"])

  # Verificar proporciones: 1/10^2=0.01, 1/20^2=0.0025, 1/40^2=0.000625
  # suma = 0.013125; pesos = 0.762, 0.190, 0.048
  total_inv <- sum(1 / distancias$distancia_m^2)
  expect_equal(round(unname(pesos["M0001"]), 4), round(1/10^2 / total_inv, 4))
})

test_that("calcular_pesos_idw: potencia mayor aumenta contraste de pesos", {
  distancias <- data.frame(
    CODIGO = c("M0001", "M0002"),
    distancia_m = c(10, 30)
  )

  pesos_p1 <- calcular_pesos_idw(c("M0001", "M0002"), distancias, potencia = 1)
  pesos_p3 <- calcular_pesos_idw(c("M0001", "M0002"), distancias, potencia = 3)

  # Con p=3 el contraste es mayor: la diferencia de pesos debe ser más grande
  diff_p1 <- abs(pesos_p1["M0001"] - pesos_p1["M0002"])
  diff_p3 <- abs(pesos_p3["M0001"] - pesos_p3["M0002"])
  expect_true(diff_p3 > diff_p1)
})

test_that("calcular_idtr_ponderado IDW: usa distancias correctamente", {
  distancias <- data.frame(
    CODIGO = c("M0002", "M0003"),
    distancia_m = c(10, 10)  # misma distancia → pesos iguales → promedio simple
  )

  res_idw <- calcular_idtr_ponderado(
    estaciones_seleccionadas = c("M0002", "M0003"),
    tabla_idtr = idtr,
    metodo = "idw",
    distancias = distancias,
    potencia = 2
  )

  res_simple <- calcular_idtr_ponderado(
    estaciones_seleccionadas = c("M0002", "M0003"),
    tabla_idtr = idtr, metodo = "simple"
  )

  # Con distancias iguales, IDW debe dar el mismo resultado que simple
  expect_equal(round(res_idw$idtr_ponderado["TR10"], 6),
               round(res_simple$idtr_ponderado["TR10"], 6))
})
