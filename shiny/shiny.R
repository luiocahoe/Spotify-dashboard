rm(list=ls())
library(shiny)
library(flexdashboard)
library(ggplot2)
library(plotly)
library(highcharter)
library(tidyverse)
library(DT)
library(arrow)
library(bslib)

datos_sep=read_feather("datos_sep.feather")

datos_sep=datos_sep %>% 
  rename_with(~ str_replace(., "_%", ""), contains("_%"))

Top10Artistas=datos_sep %>% 
  group_by(artist_names) %>% 
  summarise(Total=n()) %>% 
  arrange(desc(Total)) %>% 
  slice(1:10) %>% 
  pull(artist_names)
DatosTop10Artistas=datos_sep%>% 
  group_by(artist_names,released_month) %>% 
  summarise(Canciones=n()) %>% 
  filter(artist_names %in% Top10Artistas) %>% 
  ungroup() %>% 
  complete(artist_names, released_month,fill = list(Canciones = 0))
nombres_meses=c(
  "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
  "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
)
todos_los_meses =data.frame(
  Mes_Numero = 1:12,
  Mes_Nombre = factor(nombres_meses, levels = nombres_meses, ordered = TRUE)
)
DatosTop10Artistas=inner_join(DatosTop10Artistas, todos_los_meses,
                              by = c("released_month"="Mes_Numero")) %>% 
  mutate(Canciones = replace_na(Canciones, 0))
max_canciones=max(DatosTop10Artistas$Canciones)

shinyApp(
  ui = fluidPage(
    titlePanel("Resumen por artista (Top 10)"),
    page_sidebar(
      sidebar = sidebar(
        selectInput("artista", "Selecciona un Artista:",
                    choices = Top10Artistas)
      ),
      textOutput("texto_acumulado"),
      fluidRow(
        style = "height: 200px;",
        plotlyOutput("grafico_meses",height = 200),
      ),
      fluidRow(
        style = "height: 300px;",
        column(6,
               plotlyOutput("grafico_variables",height = 300),
        ),
        column(6,
               plotlyOutput("grafico_modo",height = 300),
        )
      ),
      fluidRow(
        style = "height: 310px;",
        column(8,
               highchartOutput("grafico_tree",height=300),
        ),
        column(4,
               highchartOutput("grafico_tarta",height=300)   
        )
      ),
      fluidRow(
        column(12,
               DTOutput("tabla_musica",height=400)
        )
      )
    )
  ),
  server = function(input, output) {
    datosFiltrados = reactive({
      datos_sep %>% 
        filter(artist_names == input$artista)
    })
    
    output$texto_acumulado = renderText({
      dato_streams_total = datosFiltrados() %>% 
        group_by(artist_names) %>% 
        summarise(total_rep = sum(streams)) %>% 
        pull(total_rep)
      paste("Acumula", round(dato_streams_total / 1000000, 2), "millones de streams")
    })
    
    output$grafico_meses = renderPlotly({
      DatosTop10Artistas %>%
        filter(artist_names == input$artista) %>%
        ggplot(aes(x = Mes_Nombre, y = Canciones, fill = Mes_Nombre)) +
        geom_bar(stat = "identity") +
        labs(x = "", y = "", title = paste("Número de canciones de", input$artista, "por mes")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ylim(0, max_canciones) +
        guides(fill = "none") +
        theme_minimal()
    })
    
    output$grafico_variables = renderPlotly({
      datosFiltrados() %>% 
        select(-(1:(ncol(datos_sep) - 7))) %>% 
        pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>% 
        ggplot(aes(x = Value, fill = Variable, y = after_stat(scaled))) +
        geom_density(alpha = 0.5) +  
        theme_minimal() +  
        labs(title = paste("Distribución de las variables de", input$artista),
             x = "Valor (%)", y = "") +
        theme_minimal() 
    })
    
    output$grafico_modo = renderPlotly({
      datosFiltrados() %>% 
        select(mode) %>% 
        ggplot(aes(x = mode, fill = mode)) +
        geom_bar(color = "black") +
        labs(title = paste("Modo de canciones de", input$artista),
             x = "",
             y = "") +
        theme_minimal() +
        theme(legend.position = "none")
    })
    output$tabla_musica=renderDT({
      datos_sep %>%
        filter(artist_names == input$artista) %>% 
        select(track_name,artist_names,released_year,streams) %>% 
        arrange(desc(streams)) %>%
        datatable(options=list(pageLength = 10))
    })
    output$grafico_tree=renderHighchart({
      datos_sep %>% 
        filter(artist_names==input$artista) %>% 
        hchart(
          type = "treemap", 
          hcaes(x = track_name, value = streams, color = streams)
        ) %>%
        hc_colorAxis(stops = color_stops(colors = c("white", "#1DB954"))) %>%
        hc_title(text = "Top canciones por streams") 
    })
    output$grafico_tarta=renderHighchart({
      datos_sep %>% 
        filter(artist_names=="Taylor Swift") %>% 
        group_by(key) %>% 
        summarise(Total=n()) %>% 
        arrange(key) %>% 
        hchart(
          "pie", hcaes(x = key, y = Total),
          name = "Canciones"
        ) %>% 
        hc_title(text="Keys más usadas")
    })
  }
)




