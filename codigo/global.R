# Carga las librerías y los módulos del proyecto.
library(shiny)
library(bslib)
library(shinychat)
library(visNetwork)
library(ellmer)
library(fcaR)
library(igraph)      # grafo del Graph-RAG
library(jsonlite)
library(dplyr)
library(tidyr)
library(coro)
library(pdftools)    # leer documentos PDF

# Paquetes de la interfaz: se instalan solos si faltan.
.pkgs_ui <- c("bsicons", "reactable", "waiter", "shinyjs", "rintrojs",
              "shinyvalidate", "shinyFeedback", "plotly", "ggplot2")
.faltan_ui <- .pkgs_ui[!vapply(.pkgs_ui, requireNamespace, logical(1), quietly = TRUE)]
if (length(.faltan_ui) > 0) {
  message("[UI] Instalando paquetes de interfaz que faltan: ", paste(.faltan_ui, collapse = ", "))
  install.packages(.faltan_ui)
}
library(bsicons)       # iconos de los KPIs
library(reactable)     # tablas interactivas
library(waiter)        # indicadores de carga
library(shinyjs)       # utilidades de interfaz
library(rintrojs)      # tour guiado
library(shinyvalidate) # validación de entradas
library(shinyFeedback) # avisos en los inputs
library(plotly)        # gráficos interactivos
library(ggplot2)       # gráficos

# Carga cada módulo (desde R/ o junto a global.R).
modulos <- c(
  "extractor_tripletas.R",
  "motor_simbolico.R",
  "enrutador_semantico.R",
  "recuperador_grafo.R",     # antes que el orquestador
  "orquestador_hibrido.R",
  "agente_llm.R",
  "graph_viz.R",
  "presentacion.R",          # helpers de la interfaz
  "evaluacion.R"             # evaluación
)
for (m in modulos) {
  ruta <- if (file.exists(file.path("R", m))) file.path("R", m) else m
  if (file.exists(ruta)) {
    source(ruta)
  } else {
    stop(paste0("No encuentro el módulo '", m, "'. Colócalo en la carpeta R/ ",
                "o junto a global.R, y ejecuta la app desde esa carpeta."))
  }
}
