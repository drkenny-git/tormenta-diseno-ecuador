# =============================================================================
# run_tests.R - Ejecutar todos los tests del sistema
# =============================================================================

library(testthat)

# Directorio raíz del proyecto (donde están los scripts y datos)
# Se resuelve a partir del argumento --file de Rscript (no de sys.frame,
# que no existe cuando el script corre como programa top-level).
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
test_root <- if (length(script_arg) > 0) {
  normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), ".."))
} else {
  normalizePath(".")
}

cat("\n")
cat("================================================================\n")
cat("  TESTSUITE - SISTEMA DE HIETOGRAMAS INAMHI\n")
cat("================================================================\n")
cat("Proyecto:", test_root, "\n\n")

# Ejecutar todos los archivos de test
resultado <- test_dir(
  path    = file.path(test_root, "tests/testthat"),
  env     = new.env(parent = globalenv()),
  reporter = "progress"
)

cat("\n")
cat("================================================================\n")
cat("  RESUMEN FINAL\n")
cat("================================================================\n")

# Resumen numérico
df <- as.data.frame(resultado)
n_passed  <- sum(df$passed,  na.rm = TRUE)
n_failed  <- sum(df$failed,  na.rm = TRUE)
n_warned  <- sum(df$warning, na.rm = TRUE)
n_skipped <- sum(df$skipped, na.rm = TRUE)
n_total   <- sum(df$nb, na.rm = TRUE)

cat(sprintf("  Tests ejecutados  : %d\n", n_total))
cat(sprintf("  ✓ Pasaron         : %d\n", n_passed))
cat(sprintf("  ✗ Fallaron        : %d\n", n_failed))
cat(sprintf("  ⚠ Advertencias    : %d\n", n_warned))
cat(sprintf("  - Omitidos        : %d\n", n_skipped))
cat("\n")

if (n_failed == 0) {
  cat("  ✅ TODOS LOS TESTS PASARON\n")
} else {
  cat("  ❌ ALGUNOS TESTS FALLARON - ver detalles arriba\n")
}
cat("================================================================\n\n")

invisible(resultado)
