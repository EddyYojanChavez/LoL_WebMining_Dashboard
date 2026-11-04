# R/04_analytics.R - Módulo de Analítica Avanzada, Correlaciones y Ranking Multicriterio

library(dplyr)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("logs", showWarnings = FALSE, recursive = TRUE)

registrar_log <- function(mensaje) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste(timestamp, "-", mensaje)
  cat(log_entry, "\n", file = "logs/system_activity.log", append = TRUE)
}

ejecutar_analitica_avanzada <- function() {
  registrar_log("Iniciando cálculo de analítica avanzada y modelo multicriterio en R/04_analytics.R.")
  
  tryCatch({
    # Verificar que existan los datasets previos de meta y sentimientos
    if (file.exists("data/processed/campeones_meta_completo.rds") && file.exists("data/processed/sentimientos_campeones.rds")) {
      meta_df <- readRDS("data/processed/campeones_meta_completo.rds")
      sent_df <- readRDS("data/processed/sentimientos_campeones.rds")
      
      # Unir datasets por nombre de campeón
      df_unificado <- meta_df %>%
        left_join(sent_df[, c("Campeon", "Score_Sentimiento")], by = "Campeon")
      
      df_unificado$Score_Sentimiento[is.na(df_unificado$Score_Sentimiento)] <- 0.10
      
      # 1. Cálculo del Índice de Dominancia Multicriterio (Ponderación académica de tesis)
      # 40% WinRate, 30% PickRate, 30% Sentimiento de la comunidad
      wr_norm <- (df_unificado$WinRate - min(df_unificado$WinRate)) / (max(df_unificado$WinRate) - min(df_unificado$WinRate))
      pr_norm <- (df_unificado$PickRate - min(df_unificado$PickRate)) / (max(df_unificado$PickRate) - min(df_unificado$PickRate))
      st_norm <- (df_unificado$Score_Sentimiento - min(df_unificado$Score_Sentimiento)) / (max(df_unificado$Score_Sentimiento) - min(df_unificado$Score_Sentimiento))
      
      df_unificado$Indice_Dominancia <- round((wr_norm * 0.4 + pr_norm * 0.3 + st_norm * 0.3) * 100, 2)
      
      # Ordenar por el Ranking Global
      ranking_global <- df_unificado[order(df_unificado$Indice_Dominancia, decreasing = TRUE), ]
      
      # Guardar archivo procesado para el Dashboard
      saveRDS(ranking_global, "data/processed/ranking_dominancia_global.rds")
      registrar_log("ÉXITO: Ranking de dominancia multicriterio generado y guardado correctamente.")
      return(TRUE)
    } else {
      registrar_log("ADVERTENCIA: Faltan archivos previos para la analítica avanzada.")
      return(FALSE)
    }
  }, error = function(e) {
    registrar_log(paste("ERROR en analítica avanzada:", e$message))
    return(FALSE)
  })
}

cat("Ejecutando script R/04_analytics.R...\n")
ejecutar_analitica_avanzada()
cat("¡Proceso finalizado! Revisa 'data/processed/ranking_dominancia_global.rds'.\n")