# =============================================================================
# test_hietogramas.R - Tests para los 3 métodos de hietograma y validación
# =============================================================================

source(file.path(test_root, "scripts/utils.R"))
source(file.path(test_root, "scripts/01_validation.R"))
source(file.path(test_root, "scripts/02_huff_method.R"))
source(file.path(test_root, "scripts/03_scs_method.R"))
source(file.path(test_root, "scripts/04_alternating_block.R"))

# ---------------------------------------------------------------------------
# Validación de parámetros (01_validation.R)
# ---------------------------------------------------------------------------

test_that("validar_parametros: caso válido", {
  v <- validar_parametros(c(50, 80), c(10, 100), 6, 30)
  expect_true(v$valido)
})

test_that("validar_parametros: longitudes distintas en precip_total y TR", {
  v <- validar_parametros(c(50), c(10, 100), 6, 30)
  expect_false(v$valido)
})

test_that("validar_parametros: precipitación negativa o cero", {
  expect_false(validar_parametros(c(-10, 80), c(10, 100), 6, 30)$valido)
  expect_false(validar_parametros(c(0, 80),  c(10, 100), 6, 30)$valido)
})

test_that("validar_parametros: paso de tiempo >= duración total", {
  # 60 min de paso, 1 hora de duración → paso = duración → inválido
  expect_false(validar_parametros(c(50), c(10), 1, 60)$valido)
  # 90 min de paso, 1 hora → mayor que duración → inválido
  expect_false(validar_parametros(c(50), c(10), 1, 90)$valido)
})

test_that("validar_parametros: TR negativo o cero", {
  expect_false(validar_parametros(c(50), c(-10), 6, 30)$valido)
  expect_false(validar_parametros(c(50), c(0),  6, 30)$valido)
})

test_that("validar_parametros_huff: cuartiles y probabilidades", {
  expect_true(validar_parametros_huff(1, 10)$valido)
  expect_true(validar_parametros_huff(4, 90)$valido)
  expect_false(validar_parametros_huff(0, 50)$valido)  # cuartil 0 no existe
  expect_false(validar_parametros_huff(5, 50)$valido)  # cuartil 5 no existe
  expect_false(validar_parametros_huff(2, 25)$valido)  # probabilidad 25 no existe
  expect_false(validar_parametros_huff(2, 0)$valido)   # probabilidad 0 no existe
})

test_that("validar_parametros_scs: tipos válidos e inválidos", {
  for (tipo in c("I", "IA", "II", "III")) {
    expect_true(validar_parametros_scs(tipo)$valido, info = paste("Tipo", tipo, "debe ser válido"))
  }
  expect_false(validar_parametros_scs("IV")$valido)
  expect_false(validar_parametros_scs("2")$valido)
  expect_false(validar_parametros_scs("")$valido)
})

# ---------------------------------------------------------------------------
# Huff (02_huff_method.R)
# ---------------------------------------------------------------------------

test_that("recomendar_cuartil_huff: reglas de duración", {
  expect_equal(recomendar_cuartil_huff(6)$cuartil,    1)  # límite inferior Q1
  expect_equal(recomendar_cuartil_huff(6.1)$cuartil,  2)  # cruce Q1→Q2
  expect_equal(recomendar_cuartil_huff(12)$cuartil,   2)  # límite superior Q2
  expect_equal(recomendar_cuartil_huff(12.1)$cuartil, 3)  # cruce Q2→Q3
  expect_equal(recomendar_cuartil_huff(24)$cuartil,   3)  # límite superior Q3
  expect_equal(recomendar_cuartil_huff(24.1)$cuartil, 4)  # cruce Q3→Q4
  expect_equal(recomendar_cuartil_huff(1)$cuartil,    1)  # evento muy corto → Q1
})

test_that("calcular_hietograma_huff: estructura, tamaño y totales", {
  hiet <- calcular_hietograma_huff(100, 6, 30, 1, 50,
            curvas_huff = read.csv(file.path(test_root, "data_sistema/huff_curves_standard.csv")))

  # Columnas mínimas requeridas
  expect_true(all(c("paso", "tiempo_horas", "precip_incr_mm",
                     "precip_acum_mm", "intensidad_mm_h") %in% names(hiet)))

  # Número de pasos: 6h * 60 / 30min + 1 = 13
  expect_equal(nrow(hiet), 13)

  # Tiempo inicia en 0, termina en duración
  expect_equal(hiet$tiempo_horas[1], 0)
  expect_equal(max(hiet$tiempo_horas), 6)

  # La precipitación acumulada final debe ser igual a precip_total (100 mm)
  expect_equal(round(max(hiet$precip_acum_mm), 6), 100)

  # La suma de incrementales debe igualar el total
  expect_equal(round(sum(hiet$precip_incr_mm), 6), 100)

  # No hay incrementos negativos (curvas Huff son monótonas crecientes)
  expect_true(all(hiet$precip_incr_mm >= 0))
})

test_that("calcular_hietograma_huff: Q1 concentra lluvia al inicio", {
  curvas <- read.csv(file.path(test_root, "data_sistema/huff_curves_standard.csv"))
  hiet_q1 <- calcular_hietograma_huff(100, 6, 30, 1, 50, curvas_huff = curvas)
  hiet_q4 <- calcular_hietograma_huff(100, 6, 30, 4, 50, curvas_huff = curvas)

  # En Q1 P50, al menos 50% de la lluvia debe caer en la primera mitad
  mitad <- nrow(hiet_q1) %/% 2
  frac_q1_primera_mitad <- sum(hiet_q1$precip_incr_mm[1:mitad]) / 100
  frac_q4_primera_mitad <- sum(hiet_q4$precip_incr_mm[1:mitad]) / 100
  expect_true(frac_q1_primera_mitad > frac_q4_primera_mitad,
              info = "Q1 debe concentrar más lluvia al inicio que Q4")
})

test_that("calcular_hietograma_huff: P10 más concentrado que P90", {
  curvas <- read.csv(file.path(test_root, "data_sistema/huff_curves_standard.csv"))
  hiet_p10 <- calcular_hietograma_huff(100, 6, 30, 1, 10, curvas_huff = curvas)
  hiet_p90 <- calcular_hietograma_huff(100, 6, 30, 1, 90, curvas_huff = curvas)

  # P10 (10% de tormentas) debe tener mayor intensidad máxima
  expect_true(max(hiet_p10$precip_incr_mm) > max(hiet_p90$precip_incr_mm))
})

# ---------------------------------------------------------------------------
# SCS (03_scs_method.R)
# ---------------------------------------------------------------------------

test_that("calcular_hietograma_scs: estructura y total para dur=24h", {
  curvas <- read.csv(file.path(test_root, "data_sistema/scs_curves_standard.csv"))
  hiet <- calcular_hietograma_scs(100, 24, 60, "II", curvas_scs = curvas)

  expect_equal(nrow(hiet), 25)  # 24*60/60 + 1 = 25
  expect_equal(round(max(hiet$precip_acum_mm), 6), 100)
  expect_equal(round(sum(hiet$precip_incr_mm), 6), 100)
  expect_equal(max(hiet$tiempo_horas), 24)
  expect_true(all(hiet$precip_incr_mm >= 0))
})

test_that("calcular_hietograma_scs Tipo II: pico de intensidad ≈ t=12h", {
  curvas <- read.csv(file.path(test_root, "data_sistema/scs_curves_standard.csv"))
  hiet <- calcular_hietograma_scs(100, 24, 60, "II", curvas_scs = curvas)

  t_pico <- hiet$tiempo_horas[which.max(hiet$precip_incr_mm)]
  # El pivote de Type_II está en t_norm ≈ 0.4948 → t ≈ 11.875h (paso 60 min → t=12h)
  expect_true(t_pico >= 11 && t_pico <= 13,
              info = paste("Pico esperado ~12h, obtenido:", t_pico, "h"))
})

test_that("calcular_hietograma_scs Tipo IA: pico más temprano que Tipo II", {
  curvas <- read.csv(file.path(test_root, "data_sistema/scs_curves_standard.csv"))
  hiet_ia <- calcular_hietograma_scs(100, 24, 60, "IA", curvas_scs = curvas)
  hiet_ii <- calcular_hietograma_scs(100, 24, 60, "II", curvas_scs = curvas)

  t_pico_ia <- hiet_ia$tiempo_horas[which.max(hiet_ia$precip_incr_mm)]
  t_pico_ii <- hiet_ii$tiempo_horas[which.max(hiet_ii$precip_incr_mm)]

  expect_true(t_pico_ia < t_pico_ii,
              info = "Tipo IA (costero) debe tener pico antes que Tipo II")
})

test_that("calcular_hietograma_scs: funciona para duraciones menores a 24h", {
  curvas <- read.csv(file.path(test_root, "data_sistema/scs_curves_standard.csv"))
  hiet <- calcular_hietograma_scs(80, 6, 30, "II", curvas_scs = curvas)

  expect_equal(nrow(hiet), 13)
  expect_equal(round(max(hiet$precip_acum_mm), 6), 80)
  expect_equal(round(sum(hiet$precip_incr_mm), 6), 80)
})

# ---------------------------------------------------------------------------
# Bloque Alterno (04_alternating_block.R)
# ---------------------------------------------------------------------------

test_that("ordenar_bloques_alterno: bloque mayor va al centro, suma preservada", {
  # n=5: expected c(1, 5, 10, 8, 3)
  resultado <- ordenar_bloques_alterno(c(10, 8, 5, 3, 1))
  expect_equal(resultado, c(1, 5, 10, 8, 3))
  expect_equal(sum(resultado), sum(c(10, 8, 5, 3, 1)))
  expect_equal(which.max(resultado), ceiling(5 / 2))

  # n=4: expected c(6, 20, 10, 4)
  resultado4 <- ordenar_bloques_alterno(c(20, 10, 6, 4))
  expect_equal(resultado4, c(6, 20, 10, 4))
  expect_equal(sum(resultado4), sum(c(20, 10, 6, 4)))
  expect_equal(which.max(resultado4), ceiling(4 / 2))

  # n=1: solo un bloque
  resultado1 <- ordenar_bloques_alterno(c(15))
  expect_equal(resultado1, 15)
})

test_that("calcular_hietograma_bloque_alterno: preserva precipitación total", {
  tabla <- data.frame(
    duracion_min   = c(30, 60, 90, 120),
    precip_acum_mm = c(20, 30, 36, 40)
  )
  hiet <- calcular_hietograma_bloque_alterno(tabla)

  # Suma de incrementales = precipitación total (40 mm)
  expect_equal(round(sum(hiet$precip_incr_mm), 6), 40)

  # El paso 0 tiene precip incremental = 0
  expect_equal(hiet$precip_incr_mm[hiet$paso == 0], 0)

  # El bloque mayor debe estar al centro del hietograma (excluyendo paso 0)
  incr_sin_cero <- hiet$precip_incr_mm[hiet$paso > 0]
  expect_equal(which.max(incr_sin_cero), ceiling(length(incr_sin_cero) / 2))
})

test_that("calcular_hietograma_bloque_alterno: precipitación acumulada es no decreciente", {
  tabla <- data.frame(
    duracion_min   = c(30, 60, 90, 120, 150, 180),
    precip_acum_mm = c(25, 38, 47, 53, 57, 60)
  )
  hiet <- calcular_hietograma_bloque_alterno(tabla)
  expect_true(all(diff(hiet$precip_acum_mm) >= 0))
})

test_that("validar_tabla_bloque_alterno: detecta tablas malformadas", {
  # Válida
  tabla_ok <- data.frame(duracion_min = c(30, 60), precip_acum_mm = c(20, 40))
  expect_true(validar_tabla_bloque_alterno(tabla_ok)$valido)

  # Precipitación decreciente → inválida
  tabla_dec <- data.frame(duracion_min = c(30, 60), precip_acum_mm = c(40, 20))
  expect_false(validar_tabla_bloque_alterno(tabla_dec)$valido)

  # Duración no incremental → inválida
  tabla_dur <- data.frame(duracion_min = c(60, 30), precip_acum_mm = c(20, 40))
  expect_false(validar_tabla_bloque_alterno(tabla_dur)$valido)

  # Columnas faltantes → inválida
  tabla_col <- data.frame(duracion_min = c(30, 60))
  expect_false(validar_tabla_bloque_alterno(tabla_col)$valido)
})
