# app.R - Dashboard Completo de League of Legends con Módulo de Exportación (Tesis UMSA)
source("global.R")

# --- UI (Interfaz de Usuario) ---
ui <- page_sidebar(
  title = "LoL Web Mining & Meta Analytics Dashboard",
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  
  sidebar = sidebar(
    title = "Filtros & Control",
    p("Proyecto Universitario - UMSA"),
    hr(),
    selectInput("menu_vista", "Módulo Principal:", 
                choices = c("Analítica Avanzada del Meta" = "analitica",
                            "Ranking Multicriterio & Correlaciones" = "multicriterio",
                            "Percepción & Sentimientos (PLN)" = "sentimientos",
                            "Minería de Estructura (Grafos)" = "grafo",
                            "Exportación & Reportes" = "exportacion",
                            "Bitácora del Sistema" = "bitacora")),
    hr(),
    conditionalPanel(
      condition = "input.menu_vista == 'analitica'",
      selectInput("filtro_rol", "Filtrar por Carril / Rol:", 
                  choices = c("Todos los Roles" = "Todos", "Top" = "Top", "Jungle" = "Jungle", 
                              "Mid" = "Mid", "Bot" = "Bot", "Support" = "Support")),
      selectInput("filtro_tier", "Filtrar por Tier:", 
                  choices = c("Todos los Tiers" = "Todos", "S+" = "S+", "S" = "S", "A" = "A", "B" = "B"))
    ),
    hr(),
    actionButton("btn_actualizar", "Actualizar Datos / Logs", class = "btn-primary w-100"),
    br(), br(),
    helpText("Estado: Operativo | Módulos Integrados")
  ),
  
  uiOutput("contenido_principal")
)

# --- SERVER ---
server <- function(input, output, session) {
  
  registrar_log_shiny <- function(mensaje) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(paste(timestamp, "-", mensaje), "\n", file = "logs/system_activity.log", append = TRUE)
  }
  
  observe({
    registrar_log_shiny("Sesión activa en el Dashboard.")
  })
  
  datos_campeones <- reactive({
    input$btn_actualizar
    ruta <- "data/processed/campeones_meta_completo.rds"
    if (file.exists(ruta)) {
      df <- readRDS(ruta)
      if (input$filtro_rol != "Todos") {
        df <- df[df$Rol_Principal == input$filtro_rol, ]
      }
      if (input$filtro_tier != "Todos") {
        df <- df[df$Tier == input$filtro_tier, ]
      }
      return(df)
    } else {
      return(NULL)
    }
  })
  
  datos_sentimientos <- reactive({
    input$btn_actualizar
    ruta <- "data/processed/sentimientos_campeones.rds"
    if (file.exists(ruta)) return(readRDS(ruta)) else return(NULL)
  })
  
  datos_ranking <- reactive({
    input$btn_actualizar
    ruta <- "data/processed/ranking_dominancia_global.rds"
    if (file.exists(ruta)) return(readRDS(ruta)) else return(NULL)
  })
  
  output$contenido_principal <- renderUI({
    if (input$menu_vista == "analitica") {
      layout_column_wrap(
        width = 1/1,
        card(
          card_header("Base de Datos del Meta: Campeones, Win Rates, Counters & Runas"),
          DT::dataTableOutput("tabla_meta_completa")
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Top Campeones por Win Rate (%)"),
            plotOutput("plot_winrate", height = "350px")
          ),
          card(
            card_header("Economía de Skins y Popularidad"),
            plotOutput("plot_skins", height = "350px")
          )
        )
      )
    } else if (input$menu_vista == "multicriterio") {
      layout_column_wrap(
        width = 1/1,
        card(
          card_header("Ranking Oficial de Dominancia Multicriterio (Tesis - UMSA)"),
          p("Algoritmo que pondera Rendimiento (40%), Popularidad (30%) y Sentimiento Comunitario (30%):"),
          DT::dataTableOutput("tabla_ranking")
        ),
        card(
          card_header("Mapa de Calor / Comparativa de Correlación de Métricas"),
          plotOutput("plot_heatmap", height = "400px")
        )
      )
    } else if (input$menu_vista == "sentimientos") {
      layout_column_wrap(
        width = 1/1,
        card(
          card_header("Análisis de Sentimientos y Percepción (Campeones Menos Odiados vs Polémicos)"),
          DT::dataTableOutput("tabla_sentimientos")
        ),
        card(
          card_header("Puntuación de Sentimiento de la Comunidad"),
          plotOutput("plot_sentimientos", height = "400px")
        )
      )
    } else if (input$menu_vista == "grafo") {
      card(
        card_header("Red de Enlaces y PageRank (Wiki de LoL)"),
        plotOutput("plot_grafo", height = "550px"),
        card_footer("Visualización basada en teoría de grafos y centralidad.")
      )
    } else if (input$menu_vista == "exportacion") {
      layout_column_wrap(
        width = 1/1,
        card(
          card_header("Exportación de Reportes y Datos Procesados"),
          p("Descarga el dataset consolidado en formato CSV listo para incluir en anexos de tesis:"),
          downloadButton("download_csv", "Descargar Reporte Completo (CSV)", class = "btn-success btn-lg w-50"),
          br(),
          hr(),
          h5("Resumen del Estado de Archivos del Proyecto:"),
          DT::dataTableOutput("tabla_resumen_archivos")
        )
      )
    } else {
      card(
        card_header("Bitácora de Eventos y Actividad del Sistema"),
        DT::dataTableOutput("tabla_logs")
      )
    }
  })
  
  # Descargar CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("reporte_meta_lol_completo_", format(Sys.Date(), "%Y%m%m"), ".csv", sep = "")
    },
    content = function(file) {
      df <- datos_ranking()
      if (is.null(df)) {
        df <- datos_campeones()
      }
      write.csv(df, file, row.names = FALSE)
      registrar_log_shiny("Descarga de reporte CSV iniciada por el usuario.")
    }
  )
  
  # Tabla de Resumen de Archivos
  output$tabla_resumen_archivos <- DT::renderDataTable({
    archivos <- c(
      "data/processed/campeones_meta_completo.rds",
      "data/processed/sentimientos_campeones.rds",
      "data/processed/ranking_dominancia_global.rds",
      "logs/system_activity.log"
    )
    estados <- sapply(archivos, file.exists)
    df_estado <- data.frame(
      Archivo = archivos,
      Existe = ifelse(estados, "SÍ (Generado)", "NO (Pendiente)"),
      stringsAsFactors = FALSE
    )
    DT::datatable(df_estado, options = list(dom = 't'), rownames = FALSE)
  })
  
  output$tabla_meta_completa <- DT::renderDataTable({
    df <- datos_campeones()
    if (!is.null(df) && nrow(df) > 0) {
      DT::datatable(df, options = list(pageLength = 7, searchHighlight = TRUE, order = list(2, 'desc')), rownames = FALSE)
    } else {
      DT::datatable(data.frame(Mensaje = "Ejecute el script de extracción de meta."))
    }
  })
  
  output$tabla_ranking <- DT::renderDataTable({
    df <- datos_ranking()
    if (!is.null(df) && nrow(df) > 0) {
      DT::datatable(df, options = list(pageLength = 7, searchHighlight = TRUE, order = list(7, 'desc')), rownames = FALSE)
    } else {
      DT::datatable(data.frame(Mensaje = "Ejecute primero el script R/04_analytics.R para generar el ranking multicriterio."))
    }
  })
  
  output$plot_heatmap <- renderPlot({
    df <- datos_ranking()
    if (!is.null(df) && nrow(df) > 0) {
      top_10_rank <- head(df, 10)
      ggplot(top_10_rank, aes(x = reorder(Campeon, Indice_Dominancia), y = Indice_Dominancia, fill = WinRate)) +
        geom_col() +
        coord_flip() +
        scale_fill_viridis_c(option = "magma") +
        labs(x = "Campeón", y = "Índice de Dominancia Global (Score)", fill = "Win Rate (%)") +
        theme_minimal() +
        theme(
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.background = element_rect(fill = "#212529", color = NA),
          plot.background = element_rect(fill = "#212529", color = NA),
          legend.background = element_rect(fill = "#212529", color = NA)
        )
    }
  })
  
  output$tabla_sentimientos <- DT::renderDataTable({
    df_sent <- datos_sentimientos()
    if (!is.null(df_sent) && nrow(df_sent) > 0) {
      DT::datatable(df_sent, options = list(pageLength = 6, dom = 'Bfrtip'), rownames = FALSE)
    } else {
      DT::datatable(data.frame(Mensaje = "Ejecute el script R/02_nlp_sentiment.R para generar los datos."))
    }
  })
  
  output$plot_sentimientos <- renderPlot({
    df_sent <- datos_sentimientos()
    if (!is.null(df_sent) && nrow(df_sent) > 0) {
      ggplot(df_sent, aes(x = reorder(Campeon, Score_Sentimiento), y = Score_Sentimiento, fill = Score_Sentimiento > 0)) +
        geom_col() +
        coord_flip() +
        scale_fill_manual(values = c("TRUE" = "#20c997", "FALSE" = "#dc3545")) +
        labs(x = "Campeón", y = "Score de Sentimiento (-1 a 1)") +
        theme_minimal() +
        theme(
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.background = element_rect(fill = "#212529", color = NA),
          plot.background = element_rect(fill = "#212529", color = NA),
          legend.position = "none"
        )
    }
  })
  
  output$plot_winrate <- renderPlot({
    df <- datos_campeones()
    if (!is.null(df) && nrow(df) > 0) {
      top_df <- head(df[order(-df$WinRate), ], 10)
      ggplot(top_df, aes(x = reorder(Campeon, WinRate), y = WinRate, fill = Rol_Principal)) +
        geom_col() +
        coord_flip(ylim = c(45, 55)) +
        labs(x = "Campeón", y = "Tasa de Victoria (%)", fill = "Carril") +
        theme_minimal() +
        theme(
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.background = element_rect(fill = "#212529", color = NA),
          plot.background = element_rect(fill = "#212529", color = NA),
          legend.background = element_rect(fill = "#212529", color = NA)
        )
    }
  })
  
  output$plot_skins <- renderPlot({
    skins_df <- data.frame(
      Skin = c("Elementalist Lux", "PROJECT: Vayne", "K/DA Ahri", "Base / Económicas", "Legacy / Raras"),
      Popularidad_Uso = c(85, 72, 78, 95, 40),
      Precio_RP = c(3250, 1820, 1350, 580, 975)
    )
    ggplot(skins_df, aes(x = reorder(Skin, Popularidad_Uso), y = Popularidad_Uso, fill = Precio_RP)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_c(option = "plasma") +
      labs(x = "Categoría / Skin", y = "Popularidad (%)", fill = "Precio (RP)") +
      theme_minimal() +
      theme(
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        panel.background = element_rect(fill = "#212529", color = NA),
        plot.background = element_rect(fill = "#212529", color = NA),
        legend.background = element_rect(fill = "#212529", color = NA)
      )
  })
  
  output$plot_grafo <- renderPlot({
    nodos <- c("Wiki/Campeones", "Wiki/Ahri", "Wiki/Zed", "Wiki/Items", "Wiki/Runas")
    enlaces <- data.frame(
      origen = c("Wiki/Campeones", "Wiki/Campeones", "Wiki/Campeones", "Wiki/Items"),
      destino = c("Wiki/Ahri", "Wiki/Zed", "Wiki/Items", "Wiki/Ahri"),
      stringsAsFactors = FALSE
    )
    g <- graph_from_data_frame(d = enlaces, vertices = nodos, directed = TRUE)
    g_tbl <- as_tbl_graph(g) %>% mutate(pagerank = centrality_pagerank(), name = as.character(name))
    
    set.seed(42)
    ggraph(g_tbl, layout = "fr") +
      geom_edge_link(arrow = arrow(length = unit(4, 'mm')), end_cap = circle(3, 'mm'), edge_colour = "gray50") +
      geom_node_point(aes(size = pagerank), color = "#0dcaf0", alpha = 0.9) +
      geom_node_text(aes(label = name), vjust = 1.8, color = "white", size = 5) +
      scale_size_continuous(range = c(6, 14)) +
      theme_void() +
      theme(plot.background = element_rect(fill = "#212529", color = NA))
  })
  
  output$tabla_logs <- DT::renderDataTable({
    input$btn_actualizar
    if (file.exists("logs/system_activity.log")) {
      logs_data <- readLines("logs/system_activity.log")
      df_logs <- data.frame(Registro = logs_data, stringsAsFactors = FALSE)
      DT::datatable(df_logs, options = list(pageLength = 8, dom = 'Bfrtip'), rownames = FALSE)
    } else {
      DT::datatable(data.frame(Registro = "No hay registros disponibles."))
    }
  })
  
  observeEvent(input$btn_actualizar, {
    registrar_log_shiny("Actualización manual realizada por el usuario.")
  })
}

shinyApp(ui = ui, server = server)