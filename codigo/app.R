# app.R — punto de entrada: carga librerías y módulos y lanza la app.

library(shiny)
library(bslib)
library(shinychat)
library(visNetwork)
library(ellmer)
library(fcaR)
library(jsonlite)
library(dplyr)
library(tidyr)
library(coro)
library(mirai)
library(igraph)
mirai::daemons(1)   # worker en segundo plano para el LLM

modulos <- c("motor_simbolico.R", "enrutador_semantico.R", "recuperador_grafo.R",
             "orquestador_hibrido.R", "agente_llm.R", "graph_viz.R")
for (m in modulos) {
  ruta <- if (file.exists(file.path("R", m))) file.path("R", m) else m
  if (!file.exists(ruta)) {
    stop(paste0("No encuentro el módulo '", m, "'. Colócalo en R/ o junto a app.R, ",
                "y ejecuta la app desde esa carpeta (Session > Set Working Directory > To Source File Location)."))
  }
  source(ruta, local = TRUE)
}

# --- UI y server, cargados en el MISMO entorno
ui     <- source("ui.R", local = TRUE)$value
server <- source("server.R", local = TRUE)$value

shinyApp(ui = ui, server = server)
