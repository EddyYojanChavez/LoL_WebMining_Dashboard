# global.R - Configuración global y librerías del Dashboard

# Instalar bslib y DT si no los tienes
install.packages(c("shiny", "bslib", "DT", "igraph", "tidygraph", "ggraph", "ggplot2"))

library(shiny)
library(bslib)
library(DT)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggplot2)

# Crear archivo de log inicial para la bitácora si no existe
log_path <- "logs/system_activity.log"
if (!file.exists(log_path)) {
  dir.create("logs", showWarnings = FALSE, recursive = TRUE)
  writeLines(paste(Sys.time(), "- [INIT] Sistema de Minería Web y Dashboard inicializado correctamente."), log_path)
}

# Función para registrar eventos en la Bitácora
registrar_bitacora <- function(mensaje) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste(timestamp, "-", mensaje)
  cat(log_entry, "\n", file = log_path, append = TRUE)
}