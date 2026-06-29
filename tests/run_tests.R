# =============================================================================
# run_tests.R - Ejecutar todos los tests del sistema
# =============================================================================

library(testthat)

# Directorio raíz del proyecto (donde están los scripts y datos)
test_root <- normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."),
                           mustWork = FALSE)
# Si se ejecuta con Rscript directamente:
if (!exists("test_root") || is.na(test_root) || test_root == "") {
  test_root <- "/Users/drkenny/Projects/hidrologia/IDF_Ecuador_app"
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
