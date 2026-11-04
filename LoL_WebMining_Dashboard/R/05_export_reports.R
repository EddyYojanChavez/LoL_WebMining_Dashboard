# R/05_export_reports.R - Módulo de Generación y Exportación de Reportes

dir.create("exports", showWarnings = FALSE, recursive = TRUE)
dir.create("logs", showWarnings = FALSE, recursive = TRUE)

registrar_log <- function(mensaje) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste(timestamp, "-", mensaje)
  cat(log_entry, "\n", file = "logs/system_activity.log", append = TRUE)
}

generar_reporte_csv <- function() {
  registrar_log("Generando archivo CSV consolidado para exportación.")
  tryCatch({
    if (file.exists("data/processed/ranking_dominancia_global.rds")) {
      df <- readRDS("data/processed/ranking_dominancia_global.rds")
      write.csv(df, "exports/reporte_meta_lol_completo.csv", row.names = FALSE)
      registrar_log("ÉXITO: Reporte CSV exportado en exports/reporte_meta_lol_completo.csv")
      return(TRUE)
    } else {
      registrar_log("ADVERTENCIA: No se encontró ranking_dominancia_global.rds para exportar.")
      return(FALSE)
    }
  }, error = function(e) {
    registrar_log(paste("ERROR al exportar CSV:", e$message))
    return(FALSE)
  })
}

cat("Ejecutando script de exportación R/05_export_reports.R...\n")
generar_reporte_csv()
cat("¡Proceso finalizado! Revisa la carpeta 'exports/'.\n")