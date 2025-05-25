# app.R – Dashboard One‑Page de Tiendas OXXO con análisis descriptivo
# -------------------------------------------------------------------
# Este script extiende la versión anterior agregando todos los gráficos
# exploratorios que compartiste. Se añadieron sub‑pestañas dentro del
# tab "Gráficos" para que puedas navegar por cada visualización.
# -------------------------------------------------------------------

# Cargar librerías ---------------------------------------------------
library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(DT)

# EDA helpers
library(tidyr)
library(skimr)
library(GGally)
library(corrplot)
library(janitor)
library(naniar)

# Cargar datos -------------------------------------------------------
# Ajusta las rutas de acuerdo con tu estructura de proyecto

tiendas_path      <- "data/DIM_TIENDA_V2.csv"   # catálogo tiendas
base_modelo_path  <- "data/base_modelo_V2.csv"      # dataset con ratio y exito

tiendas      <- read.csv(tiendas_path, encoding = "UTF-8")
base_modelo  <- read.csv(base_modelo_path, encoding = "UTF-8")

# UI ----------------------------------------------------------------
ui <- fluidPage(
  titlePanel("One‑Page de Tiendas OXXO"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "entorno", "Entorno:",
        choices   = sort(unique(tiendas$ENTORNO_DES)),
        multiple  = TRUE
      ),
      selectInput(
        "nivel", "Nivel Socioeconómico:",
        choices   = sort(unique(tiendas$NIVELSOCIOECONOMICO_DES)),
        multiple  = TRUE
      ),
      tags$hr(),
      helpText("Filtra las tiendas y navega por las pestañas para ver el mapa, el resumen y los distintos análisis gráficos.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Mapa",          leafletOutput("mapa", height = 550)),
        tabPanel("Resumen",       dataTableOutput("tabla")),
        
        # ---------- NUEVO: colección de gráficas exploratorias ----------
        tabPanel(
          "Gráficos",
          tabsetPanel(
            tabPanel("Tiendas por Segmento",   plotOutput("grafico_segmento")),
            tabPanel("Distribución Ratio",    plotOutput("dist_ratio")),
            tabPanel("Proporción Éxito",     plotOutput("prop_exito")),
            tabPanel("Distribuciones Numéricas", plotOutput("dist_num")),
            tabPanel("Boxplots vs. Éxito",      plotOutput("box_num_exito")),
            tabPanel("Correlación Numéricas",   plotOutput("corr_num", height = 600)),
            tabPanel("Meta vs. Prom",            plotOutput("meta_prom")),
            tabPanel("Mapa Éxito",             leafletOutput("mapa_exito", height = 550))
          )
        )
      )
    )
  )
)

# Server -------------------------------------------------------------
server <- function(input, output, session) {
  
  # ---------------------- Tiendas filtradas ------------------------
  tiendas_filtradas <- reactive({
    req(tiendas)
    tiendas %>%
      filter(
        if (length(input$entorno) > 0) ENTORNO_DES %in% input$entorno else TRUE,
        if (length(input$nivel)   > 0) NIVELSOCIOECONOMICO_DES %in% input$nivel else TRUE
      )
  })
  
  # ----------------------------- Mapa ------------------------------
  output$mapa <- renderLeaflet({
    leaflet(tiendas_filtradas()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng         = ~LONGITUD_NUM,
        lat         = ~LATITUD_NUM,
        popup       = ~paste0("Tienda: ", TIENDA_ID, "<br/>", direccion),
        radius      = 4,
        color       = "#007BFF",
        fillOpacity = 0.7
      )
  })
  
  # --------------------------- Resumen -----------------------------
  output$tabla <- renderDataTable({
    datatable(tiendas_filtradas(), options = list(pageLength = 10))
  })
  
  # ---------------------- GRÁFICOS EXPLORATORIOS -------------------
  # (1) Tiendas por Segmento Maestro
  output$grafico_segmento <- renderPlot({
    ggplot(tiendas_filtradas(), aes(x = SEGMENTO_MAESTRO_DESC)) +
      geom_bar(fill = "#2C3E50") +
      theme_minimal() +
      labs(title = "Tiendas por Segmento Maestro",
           x = "Segmento Maestro", y = "Conteo")
  })
  
  # (2) Distribución del ratio prom / Meta_venta
  output$dist_ratio <- renderPlot({
    ggplot(base_modelo, aes(ratio)) +
      geom_histogram(fill = "steelblue", bins = 30) +
      labs(title = "Distribución del ratio prom / Meta_venta",
           x = "ratio", y = "Frecuencia") +
      theme_minimal()
  })
  
  # (3) Proporción de tiendas exitosas
  output$prop_exito <- renderPlot({
    base_modelo %>%
      count(EXITO_POND) %>%
      mutate(pct = prop.table(n)) %>%
      ggplot(aes(x = factor(EXITO_POND), y = pct, fill = factor(EXITO_POND))) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Éxito (1 = ratio \u2265 1)", y = "% de tiendas",
           title = "Proporción de tiendas exitosas") +
      theme_minimal()
  })
  
  # (4) Distribuciones de variables numéricas
  num_vars <- c("MTS2VENTAS_NUM", "PUERTASREFRIG_NUM",
                "CAJONESESTACIONAMIENTO_NUM", "prom", "desvstd")
  
  output$dist_num <- renderPlot({
    base_modelo %>%
      pivot_longer(all_of(num_vars)) %>%
      ggplot(aes(value)) +
      facet_wrap(~name, scales = "free") +
      geom_histogram(bins = 30, fill = "darkorange") +
      labs(title = "Distribuciones de variables numéricas") +
      theme_minimal()
  })
  
  # (5) Boxplots numéricas vs. éxito
  output$box_num_exito <- renderPlot({
    base_modelo %>%
      select(all_of(num_vars), EXITO_POND) %>%
      pivot_longer(-EXITO_POND) %>%
      ggplot(aes(x = factor(EXITO_POND), y = value, fill = factor(EXITO_POND))) +
      geom_boxplot(outlier.alpha = .2) +
      facet_wrap(~name, scales = "free") +
      labs(x = "Éxito", y = "", title = "Boxplots: numéricas vs. éxito") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # (6) Heatmap de correlación
  output$corr_num <- renderPlot({
    num_mat <- base_modelo %>%
      select(all_of(num_vars)) %>%
      mutate(across(everything(), as.numeric)) %>%
      cor(use = "pairwise.complete.obs")
    
    corrplot(num_mat, method = "color", type = "upper",
             tl.col = "black", addCoef.col = "black")
  })
  
  # (7) Meta de venta vs. promedio histórico
  output$meta_prom <- renderPlot({
    ggplot(base_modelo, aes(Meta_venta, prom, colour = factor(EXITO_POND))) +
      geom_point(alpha = .6) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      scale_color_manual(values = c("firebrick", "forestgreen")) +
      labs(colour = "Éxito", 
           title  = "Promedio vs. Meta de venta",
           x      = "Meta de venta", y = "Promedio histórico") +
      theme_minimal()
  })
  
  # (8) Mapa de éxito
  output$mapa_exito <- renderLeaflet({
    leaflet(base_modelo) %>%
      addTiles() %>%
      addCircleMarkers(
        lng    = ~LONGITUD_NUM, lat = ~LATITUD_NUM,
        color  = ~ifelse(EXITO_POND == 1, "green", "red"),
        radius = 4,
        popup  = ~paste0("Tienda ", TIENDA_ID, "<br>Ratio: ", round(ratio, 2))
      )
  })
}

# Lanzar la app ------------------------------------------------------
shinyApp(ui = ui, server = server)
