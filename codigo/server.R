# server.R — lógica de la app: eventos, motor y capa visual (LLM en 2º plano con mirai).
function(input, output, session) {
  
  # --- Estado ---
  sistema_prompt      <- reactiveVal(NULL)   # el chat se crea en el worker; aquí solo el prompt
  cabecera_pendiente  <- reactiveVal(NULL)   # trazabilidad a mostrar cuando llegue la resolución
  matriz_global       <- reactiveVal(NULL)
  objetos_global      <- reactiveVal(NULL)
  atributos_global    <- reactiveVal(NULL)
  fc_global           <- reactiveVal(NULL)
  reglas_global       <- reactiveVal("Sube un CSV y extrae las reglas para verlas aquí.")
  nodos_base_global   <- reactiveVal(NULL)
  tripletas_extraidas <- reactiveVal(NULL)
  ruta_csv_activa     <- reactiveVal(NULL)
  aristas_global      <- reactiveVal(NULL)
  igraph_global    <- reactiveVal(NULL)   # ← grafo igraph para el paso Graph-RAG
  posiciones_global <- reactiveVal(NULL)  # posiciones base de los nodos (x,y) para el radial
  resultado_eval    <- reactiveVal(NULL)  # detalle de la evaluación (fila por pregunta×brazo)
  resumen_eval      <- reactiveVal(NULL)  # medias por brazo
  resumen_eval_manual <- reactiveVal(NULL)  # medias por brazo, solo gold manual (sin circularidad)

  # Capa visual (solo presentación, no toca el motor).
  # "Extraer reglas" queda deshabilitado hasta que haya datos.
  shinyjs::disable("btn_analizar")
  observeEvent(ruta_csv_activa(), shinyjs::enable("btn_analizar"), ignoreNULL = TRUE)

  # Modo avanzado: si está desactivado (Básico) se ocultan las pestañas técnicas y la traza.
  observeEvent(input$modo_avanzado, {
    tecnicas <- c("reticulo", "reglas", "datos", "evaluacion")
    if (isTRUE(input$modo_avanzado)) {
      for (t in tecnicas) bslib::nav_show("viz_tabs", t)
    } else {
      for (t in tecnicas) bslib::nav_hide("viz_tabs", t)
      bslib::nav_select("viz_tabs", "grafo")   # si estaba en una pestaña ahora oculta, vuelve al grafo
    }
  }, ignoreInit = FALSE)

  # Validación no intrusiva: el sub-retículo necesita >= 2 atributos.
  iv <- InputValidator$new()
  iv$add_rule("reticulo_attrs", function(x) {
    if (length(intersect(x, atributos_global())) < 2) "Elige al menos 2 atributos"
  })
  iv$enable()

  # KPIs del cuadro de mando (solo lectura de lo ya calculado).
  output$kpis <- renderUI({
    fc  <- fc_global()
    fmt <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "—" else format(x, big.mark = ".", decimal.mark = ",")
    n_conceptos <- if (!is.null(fc)) tryCatch(fc$concepts$size(), error = function(e) NA_integer_) else NA_integer_
    n_reglas    <- length(reglas_lineas())
    n_objetos   <- length(objetos_global())
    n_attrs     <- length(atributos_global())
    layout_columns(
      col_widths = c(6, 6, 6, 6), gap = "12px",
      kpi_box("Conceptos",      fmt(n_conceptos),                          "diagram-3",    "info"),
      kpi_box("Reglas lógicas", fmt(if (n_reglas  == 0) NA else n_reglas), "shield-check", "success"),
      kpi_box("Entidades",      fmt(if (n_objetos == 0) NA else n_objetos), "capsule",     "primary"),
      kpi_box("Atributos",      fmt(if (n_attrs   == 0) NA else n_attrs),  "tags",         "secondary")
    )
  })

  # Tour guiado (onboarding / defensa del TFG).
  observeEvent(input$tour, {
    rintrojs::introjs(session, options = list(
      nextLabel = "Siguiente", prevLabel = "Atrás", doneLabel = "Hecho", showBullets = TRUE,
      steps = list(
        list(element = "#btn_extraer",
             intro = "1) Sube un documento clínico (PDF/TXT) y pulsa aquí: la IA extrae las tripletas de conocimiento."),
        list(element = "#bloque_csv",
             intro = "Alternativa: si ya tienes las tripletas, puedes subir directamente un CSV aquí, sin pasar por la IA."),
        list(element = "#btn_analizar",
             intro = "2) El FCA construye el retículo de conceptos y las reglas lógicas deterministas."),
        list(element = "#pane_chat",
             intro = "3) Pregunta en lenguaje natural: el FCA razona (cero alucinación) y el LLM solo redacta."),
        list(element = "#pane_viz",
             intro = "Explora el grafo de conocimiento, el retículo, las reglas, las métricas y los datos.")
      )))
  })

  # Cuando el grafo termina de dibujarse, pide sus posiciones y las guarda.
  observeEvent(input$grafo_lista, {
    visNetworkProxy("grafo_fca") %>% visGetPositions()
  })
  observeEvent(input$grafo_fca_positions, {
    posiciones_global(input$grafo_fca_positions)
  })

  # Recoloca los VECINOS de 'item' en círculo a su alrededor (sin solaparse).
  reposicionar_radial <- function(item, vecinos) {
    pos <- posiciones_global()
    if (is.null(pos) || is.null(pos[[item]]) || length(vecinos) == 0) return(NULL)
    cx <- pos[[item]]$x; cy <- pos[[item]]$y
    n  <- length(vecinos)
    ang <- seq(0, 2 * pi, length.out = n + 1)[seq_len(n)]
    rr  <- max(160, 15 * n)   # el radio crece con el nº de vecinos
    data.frame(id = vecinos,
               x = round(cx + rr * cos(ang)),
               y = round(cy + rr * sin(ang)),
               stringsAsFactors = FALSE)
  }
  # Devuelve todas las posiciones base (para restaurar la vista completa).
  df_restaurar_pos <- function() {
    pos <- posiciones_global()
    if (is.null(pos) || length(pos) == 0) return(NULL)
    ids <- names(pos)
    data.frame(id = ids,
               x = vapply(ids, function(i) pos[[i]]$x, numeric(1)),
               y = vapply(ids, function(i) pos[[i]]$y, numeric(1)),
               stringsAsFactors = FALSE)
  }
  
  observeEvent(input$archivo_datos, {
    ruta_csv_activa(input$archivo_datos$datapath)
    output$estado_extraccion <- renderUI(NULL)
    output$ui_descarga_csv   <- renderUI(NULL)
  })

  # Carga el CSV de ejemplo incluido con la app.
  observeEvent(input$btn_ejemplo, {
    ruta <- "datos_medicos.csv"
    if (!file.exists(ruta)) {
      showNotification("No se encuentra datos_medicos.csv en la carpeta de la app.", type = "error")
      return(NULL)
    }
    ruta_csv_activa(ruta)
    output$estado_extraccion <- renderUI(NULL)
    output$ui_descarga_csv   <- renderUI(NULL)
    showNotification("Datos de ejemplo cargados. Pulsa 'Extraer Reglas y Contexto'.",
                     type = "message", duration = 5)
  })
  
  # EVENTO 0: EXTRACCIÓN NER+RE
  observeEvent(input$btn_extraer, {
    if (is.null(input$archivo_documento)) {
      shinyFeedback::showFeedbackDanger("archivo_documento", text = "Sube un PDF o TXT primero")
      showNotification("Por favor, sube un documento PDF o TXT primero.", type = "error")
      return(NULL)
    }
    shinyFeedback::hideFeedback("archivo_documento")
    # Barra de progreso real, un paso por fragmento.
    tripletas <- withProgress(message = "Extrayendo conocimiento con IA…",
                              detail = "Preparando…", value = 0, {
      tryCatch({
        extraer_tripletas_doc(path = input$archivo_documento$datapath, model = "llama3.1",
                              max_chars = 1800,
                              progreso_fn = function(i, n)
                                setProgress(value = i / n,
                                            detail = sprintf("Fragmento %d de %d", i, n)))
      }, error = function(e) {
        showNotification(paste("Error en extracción:", conditionMessage(e)), type = "error", duration = 8)
        NULL
      })
    })
    if (is.null(tripletas)) {
      shinyFeedback::showFeedbackDanger("archivo_documento", text = "No se pudieron extraer tripletas")
      return(NULL)
    }
    tripletas_extraidas(tripletas)
    ruta_tmp <- tempfile(fileext = ".csv")
    cols_csv <- intersect(c("Sujeto", "Relacion", "Objeto"), names(tripletas))
    write.csv(tripletas[, cols_csv], ruta_tmp, row.names = FALSE, quote = TRUE)
    ruta_csv_activa(ruta_tmp)
    output$estado_extraccion <- renderUI({
      p(class = "estado-ok", icon("circle-check"),
        sprintf(" %d tripletas extraídas. Ahora pulsa 'Extraer Reglas'.", nrow(tripletas)))
    })
    output$ui_descarga_csv <- renderUI({
      downloadButton("btn_descargar_csv", "Descargar CSV generado",
                     icon = icon("download"), class = "btn-sm btn-outline-secondary mt-1")
    })
    shinyFeedback::showFeedbackSuccess("archivo_documento",
      text = sprintf("%d tripletas extraídas", nrow(tripletas)))
    showNotification(sprintf("Extracción completada: %d trípletas médicas identificadas.", nrow(tripletas)),
                     type = "message", duration = 5)
  })
  
  # EVENTO 0b: DESCARGA CSV
  output$btn_descargar_csv <- downloadHandler(
    filename = function() paste0("tripletas_medicas_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      req(tripletas_extraidas())
      write.csv(tripletas_extraidas(), file, row.names = FALSE, quote = TRUE)
    }
  )
  
  # EVENTO 1: EXTRAER REGLAS + FCA
  observeEvent(input$btn_analizar, {
    ruta <- ruta_csv_activa()
    if (is.null(ruta)) {
      showNotification("Primero extrae conocimiento de un documento (Paso 1A) o sube un CSV (Paso 1B).", type = "error")
      return(NULL)
    }
    print("Construyendo contexto formal y extrayendo implicaciones...")
    ctx <- withProgress(message = "Construyendo el retículo de conceptos…",
                        detail = "Extrayendo implicaciones FCA…", value = 0.6, {
      tryCatch(construir_contexto(ruta),
               error = function(e) { showNotification(paste("Error:", conditionMessage(e)), type = "error"); NULL })
    })
    if (is.null(ctx)) return(NULL)
    
    matriz_global(ctx$matriz); objetos_global(ctx$objetos); atributos_global(ctx$atributos)
    fc_global(ctx$fc);         reglas_global(ctx$reglas_texto)
    igraph_global(ctx$igraph_obj)
    print("Preparando agente LLM (system prompt) y precalentando modelo...")
    sistema_prompt(crear_system_prompt())
    precalentar_keepalive()
    
    print("Renderizando Grafo Base...")
    output$grafo_fca <- renderVisNetwork({
      tryCatch({
        nodos   <- construir_nodos_base(ctx$objetos, ctx$atributos)
        aristas <- preparar_aristas(ctx$grafo_limpio)
        nodos_base_global(nodos)
        aristas_global(aristas)
        construir_grafo(nodos, aristas)
      }, error = function(e) {
        showNotification(paste("Error al dibujar el grafo:", conditionMessage(e)),
                         type = "error", duration = NULL)
        visNetwork(data.frame(id = 1, label = "Error al dibujar el grafo"))
      })
    })
    
    showNotification("Análisis completado. Grafo y Reglas extraídos.", type = "message")
    chat_append("chat_llm", "**Arquitectura FCA_Graph-RAG Inicializada.** \n Esperando consultas...")
  })
  
  # Muestra un tipo de nodo y sus vecinos, oculta el resto ("TODOS" restaura todo).
  vista_grupo <- function(tipo) {
    nodos   <- nodos_base_global()
    aristas <- aristas_global()
    if (is.null(tipo) || tipo == "TODOS") {
      nodos_update <- df_reset_nodos(nodos)
    } else {
      ids_grupo    <- nodos$id[nodos$group == tipo]
      vecinos_raw  <- unique(c(aristas$to[aristas$from %in% ids_grupo],
                               aristas$from[aristas$to %in% ids_grupo]))
      ids_visibles <- c(ids_grupo, setdiff(vecinos_raw, ids_grupo))
      nodos_update <- data.frame(
        id = nodos$id,
        hidden = !(nodos$id %in% ids_visibles),
        color  = nodos$color,
        size   = ifelse(nodos$id %in% ids_grupo, nodos$size + 25, nodos$size),
        borderWidth = ifelse(nodos$id %in% ids_grupo, 4, 1),
        shadow = nodos$id %in% ids_grupo,
        stringsAsFactors = FALSE
      )
    }
    prx <- visNetworkProxy("grafo_fca") %>%
      visUpdateNodes(nodes = nodos_update) %>%
      visUpdateEdges(df_reset_aristas(aristas))
    rp <- df_restaurar_pos()
    if (!is.null(rp)) prx <- prx %>% visUpdateNodes(rp)
    prx %>% visFit()
  }

  # EVENTO 1b: FILTRO DE GRUPO + rellenar 2º desplegable
  observeEvent(input$filtro_grafo_grupo, {
    req(nodos_base_global(), aristas_global())
    nodos <- nodos_base_global()
    tipo  <- input$filtro_grafo_grupo
    vista_grupo(tipo)

    if (is.null(tipo) || tipo == "TODOS") {
      updateSelectInput(session, "filtro_grafo_item", choices = c("Todos" = "TODOS"), selected = "TODOS")
    } else {
      items <- sort(nodos$id[nodos$group == tipo])
      updateSelectInput(session, "filtro_grafo_item",
                        choices  = c(setNames("TODOS", "Todos"), setNames(items, items)),
                        selected = "TODOS")
    }
  }, ignoreInit = TRUE)   # no ejecutar en la carga: interferiría con la estabilización del grafo

  # EVENTO 1c: FILTRO POR ELEMENTO CONCRETO -> mostrar SOLO ese nodo y sus vecinos
  observeEvent(input$filtro_grafo_item, {
    req(nodos_base_global(), aristas_global())
    item <- input$filtro_grafo_item
    if (is.null(item) || item == "TODOS") {
      vista_grupo(input$filtro_grafo_grupo)   # volver a la vista del grupo actual
      return(NULL)
    }
    vecinos <- vecinos_de(item, aristas_global())
    foco    <- unique(c(item, vecinos))
    visNetworkProxy("grafo_fca") %>%
      visUpdateNodes(df_solo_foco(nodos_base_global(), foco, item)) %>%
      visUpdateEdges(df_aristas_foco(aristas_global(), foco, ambos = TRUE))
    rad <- reposicionar_radial(item, vecinos)
    if (!is.null(rad)) visNetworkProxy("grafo_fca") %>% visUpdateNodes(rad)
    visNetworkProxy("grafo_fca") %>%
      visFit(nodes = foco, animation = list(duration = 900, easingFunction = "easeInOutQuad"))
  }, ignoreInit = TRUE)   # no ejecutar en la carga (evita el visFit que rompe la estabilización)

  # EVENTO 1d: CLIC SOBRE UN NODO -> mostrar SOLO ese nodo y sus vecinos (ocultar el resto)
  observeEvent(input$grafo_nodo_click, {
    req(nodos_base_global(), aristas_global())
    item    <- input$grafo_nodo_click
    vecinos <- vecinos_de(item, aristas_global())
    foco    <- unique(c(item, vecinos))
    visNetworkProxy("grafo_fca") %>%
      visUpdateNodes(df_solo_foco(nodos_base_global(), foco, item)) %>%
      visUpdateEdges(df_aristas_foco(aristas_global(), foco, ambos = TRUE))
    rad <- reposicionar_radial(item, vecinos)
    if (!is.null(rad)) visNetworkProxy("grafo_fca") %>% visUpdateNodes(rad)
    visNetworkProxy("grafo_fca") %>%
      visFit(nodes = foco, animation = list(duration = 700, easingFunction = "easeInOutQuad"))
  })

  # EVENTO 1e: CLIC EN EL FONDO -> restaurar el grafo completo (y posiciones base)
  observeEvent(input$grafo_click_vacio, {
    req(nodos_base_global(), aristas_global())
    prx <- visNetworkProxy("grafo_fca") %>%
      visUpdateNodes(df_reset_nodos(nodos_base_global())) %>%
      visUpdateEdges(df_reset_aristas(aristas_global()))
    rp <- df_restaurar_pos()
    if (!is.null(rp)) prx <- prx %>% visUpdateNodes(rp)
    prx %>% visFit()
  })
  
  observeEvent(input$chat_llm_user_input, {
    req(sistema_prompt(), matriz_global(), fc_global())
    pregunta <- input$chat_llm_user_input
    print(paste("QUERY RECIBIDA:", pregunta))
    
    res <- resolver_consulta(pregunta, matriz_global(), fc_global(),
                             atributos_global(), objetos_global(), ig = igraph_global(),
                             mapear_fn = function(q) mapear_sujeto_llm(q, objetos_global()))    # EVENTO 2
    # --- Enfoque del grafo: AÍSLA como el filtro (muestra solo el foco, oculta el resto) ---
    if (!is.null(nodos_base_global())) {
      suj  <- intersect(res$sujetos,  c(objetos_global(), atributos_global()))
      dest <- intersect(res$destacar, c(objetos_global(), atributos_global()))
      foco <- unique(c(intersect(res$nodos, c(objetos_global(), atributos_global())), dest))

      if (res$tipo %in% c("real", "inverso", "hipotetico") && length(foco) > 0) {
        # Solo el foco: sujeto agrandado, atributo consultado en rojo y aristas etiquetadas.
        visNetworkProxy("grafo_fca") %>%
          visUpdateNodes(df_solo_foco(nodos_base_global(), foco, suj, dest)) %>%
          visUpdateEdges(df_aristas_foco(aristas_global(), foco, ambos = TRUE))
        if (length(suj) == 1) {   # un solo sujeto -> vecinos en radial
          rad <- reposicionar_radial(suj, setdiff(foco, suj))
          if (!is.null(rad)) visNetworkProxy("grafo_fca") %>% visUpdateNodes(rad)
        }
        visNetworkProxy("grafo_fca") %>%
          visFit(nodes = foco, animation = list(duration = 900, easingFunction = "easeInOutQuad"))
      } else {
        prx <- visNetworkProxy("grafo_fca") %>%
          visUpdateNodes(df_reset_nodos(nodos_base_global())) %>%
          visUpdateEdges(df_reset_aristas(aristas_global()))
        rp <- df_restaurar_pos()
        if (!is.null(rp)) prx %>% visUpdateNodes(rp)
      }
      # Resalta la(s) arista(s) recuperada(s) por Graph-RAG (implica) sobre el enfoque
      rag_upd <- df_aristas_rag(aristas_global(), res$rag_aristas)
      if (!is.null(rag_upd)) visNetworkProxy("grafo_fca") %>% visUpdateEdges(rag_upd)
    }
    
    # --- Trazabilidad + "### 2. Resolución" YA; el LLM llega después (2º plano) ---
    # Básico: solo la respuesta en lenguaje natural. Avanzado: traza lógica + resolución.
    cabecera <- if (isTRUE(input$modo_avanzado))
      paste0("### 1. Trazabilidad Lógica\n\n", res$traza, "\n\n### 2. Resolución\n\n")
    else ""
    m <- generar_resolucion_bg(sistema_prompt(), construir_prompt_resolucion(res$veredicto, pregunta))
    chat_append("chat_llm", respuesta_diferida(cabecera, m, res))
  })
  # EVENTO 3: RENDERIZAR REGLAS
  # Lista de reglas individuales (una por elemento)
  reglas_lineas <- reactive({
    txt <- reglas_global()
    if (identical(txt, "Sube un CSV y extrae las reglas para verlas aquí.")) return(character(0))
    lineas <- unlist(strsplit(txt, "\n"))
    lineas[grepl("^Rule \\d+:", trimws(lineas))]
  })

  # Soporte de cada regla (fracción de objetos que la respaldan). Índice = nº de regla.
  reglas_soporte <- reactive({
    fc <- fc_global()
    if (is.null(fc)) return(numeric(0))
    s <- tryCatch(as.numeric(fc$implications$support()), error = function(e) NULL)
    if (is.null(s)) numeric(0) else s
  })

  # Aplica los filtros (atributos por lado + tamaño + soporte mínimo) a las reglas.
  reglas_filtradas <- reactive({
    lineas <- reglas_lineas()
    if (length(lineas) == 0) return(character(0))
    terms <- input$reglas_buscar; if (is.null(terms)) terms <- character(0)
    terms <- terms[nzchar(terms)]                         # atributos elegidos (selección exacta)
    mmode <- input$reglas_match; if (is.null(mmode)) mmode <- "todos"  # todos = Y ; alguno = O
    lado <- input$reglas_lado;   if (is.null(lado)) lado <- "cualquiera"
    modo <- input$reglas_modo_tam; if (is.null(modo)) modo <- "max"
    maxp <- input$reglas_maxprem   # NA / vacío = sin límite en antecedente
    maxc <- input$reglas_maxconc   # NA / vacío = sin límite en consecuente
    minsop <- input$reglas_minsop  # % mínimo de soporte (NA = sin límite)
    sop    <- reglas_soporte()
    attrs_de <- function(s) {                             # atributos (exactos) de un lado
      s <- sub(".*\\[", "", sub("\\].*", "", s))          # contenido entre corchetes
      Filter(nzchar, trimws(unlist(strsplit(s, ","))))
    }
    cumple_tam <- function(n, lim) {
      if (is.null(lim) || is.na(lim)) return(TRUE)        # sin límite
      if (identical(modo, "min")) n >= lim else n <= lim
    }
    keep <- vapply(lineas, function(l) {
      partes <- strsplit(l, "⇒", fixed = TRUE)[[1]]
      prem <- if (length(partes) >= 1) partes[1] else ""
      conc <- if (length(partes) >= 2) partes[2] else ""
      ap <- attrs_de(prem); ac <- attrs_de(conc)
      if (!cumple_tam(length(ap), maxp) || !cumple_tam(length(ac), maxc)) return(FALSE)
      if (length(terms) > 0) {
        side_attrs <- switch(lado, premisa = ap, conclusion = ac, unique(c(ap, ac)))
        hits <- terms %in% side_attrs
        if (identical(mmode, "alguno")) { if (!any(hits)) return(FALSE) }
        else                            { if (!all(hits)) return(FALSE) }
      }
      if (!is.null(minsop) && !is.na(minsop) && length(sop) > 0) {
        nr <- num_regla(l)
        if (!is.na(nr) && nr >= 1 && nr <= length(sop) && sop[nr] * 100 < minsop) return(FALSE)
      }
      TRUE
    }, logical(1))
    lineas[keep]
  })

  # Exporta a CSV las reglas actualmente filtradas (con soporte).
  output$dl_reglas_csv <- downloadHandler(
    filename = function() sprintf("reglas_fca_%s.csv", format(Sys.time(), "%Y%m%d_%H%M")),
    content  = function(file) {
      lin <- reglas_filtradas()
      df  <- reglas_a_dataframe(lin)
      sop <- reglas_soporte()
      if (nrow(df) > 0 && length(sop) > 0) {
        nums <- vapply(lin, num_regla, integer(1))
        df$Soporte_pct <- ifelse(!is.na(nums) & nums >= 1 & nums <= length(sop),
                                 round(sop[nums] * 100, 1), NA_real_)
      }
      utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # Controles de rango (solo aparecen cuando hay reglas)
  output$controles_reglas <- renderUI({
    n <- length(reglas_lineas())
    if (n == 0) return(NULL)
    tagList(
      div(style = "font-size:12px; color:#555; margin-bottom:6px;",
          sprintf("Total: %d reglas. Filtra y elige el rango:", n)),
      selectizeInput("reglas_buscar", "Buscar atributo(s)",
                     choices = sort(atributos_global()), multiple = TRUE,
                     options = list(placeholder = "Elige atributos…"), width = "100%"),
      radioButtons("reglas_lado", "Buscar en",
                   choices  = c("Cualquiera" = "cualquiera", "Antecedente" = "premisa",
                                "Consecuente" = "conclusion"),
                   selected = "cualquiera", inline = TRUE),
      radioButtons("reglas_match", "Coincidencia",
                   choices  = c("Todos" = "todos", "Alguno" = "alguno"),
                   selected = "todos", inline = TRUE),
      div(style = "font-size:11px; color:#888; margin-bottom:4px;", "Nº de atributos por lado:"),
      div(style = "display:flex; gap:8px; align-items:flex-end; flex-wrap:wrap; margin-bottom:8px;",
          selectInput("reglas_modo_tam", NULL, choices = c("Máx." = "max", "Mín." = "min"),
                      selected = "max", width = "80px"),
          numericInput("reglas_maxprem", "Antecedente", value = NA, min = 1, max = 99, width = "130px"),
          numericInput("reglas_maxconc", "Consecuente", value = NA, min = 1, max = 99, width = "130px")
      ),
      div(style = "font-size:11px; color:#888; margin-bottom:4px;", "Soporte mínimo (%):"),
      div(style = "display:flex; gap:8px; align-items:flex-end; flex-wrap:wrap; margin-bottom:8px;",
          numericInput("reglas_minsop", NULL, value = NA, min = 0, max = 100, width = "110px"),
          downloadButton("dl_reglas_csv", "Exportar CSV",
                         class = "btn-sm btn-outline-secondary", style = "height:38px;")
      ),
      div(style = "font-size:11px; color:#888; margin-bottom:4px;", "Rango a mostrar:"),
      div(style = "display:flex; gap:8px; align-items:flex-end; flex-wrap:wrap; margin-bottom:8px;",
          selectInput("reglas_modo_rango", NULL,
                      choices = c("Personalizado" = "rango", "Todas" = "todas"),
                      selected = "rango", width = "175px"),
          numericInput("reglas_desde", "Desde", value = 1,          min = 1, width = "90px"),
          numericInput("reglas_hasta", "Hasta", value = min(10, n), min = 1, width = "90px"),
          numericInput("reglas_concreta", "Regla concreta", value = NA, min = 1, width = "120px")
      )
    )
  })
  
  # Render de solo el rango elegido
  output$mostrar_reglas <- renderUI({
    total <- length(reglas_lineas())
    if (total == 0) return(p("Sube un CSV y extrae las reglas para verlas aquí."))

    # Búsqueda directa de una regla concreta por su número (ignora filtros y rango).
    concreta <- input$reglas_concreta
    if (!is.null(concreta) && !is.na(concreta)) {
      todas <- reglas_lineas()
      sel   <- todas[grepl(sprintf("^Rule %d:", as.integer(concreta)), trimws(todas))]
      if (length(sel) == 0)
        return(p(sprintf("No existe la Regla %d (hay %d en total).", as.integer(concreta), total)))
      bloque    <- sel[1]
      partes    <- strsplit(bloque, ":", fixed = TRUE)[[1]]
      titulo    <- trimws(partes[1])
      contenido <- trimws(gsub("\\s+", " ", paste(partes[-1], collapse = ":")))
      sop <- reglas_soporte(); nr <- num_regla(bloque)
      badge <- if (length(sop) > 0 && !is.na(nr) && nr >= 1 && nr <= length(sop))
        span(style = "color:#8a929b; font-size:11px; margin-left:8px;",
             sprintf("· soporte %s%%", format(round(sop[nr] * 100, 1), trim = TRUE))) else NULL
      return(tagList(
        div(style = "font-size:12px; color:#777; margin-bottom:4px;",
            sprintf("Mostrando la Regla %d", as.integer(concreta))),
        div(class = "reglas-container",
            div(class = "regla-bloque",
                span(class = "rule-header", paste0(titulo, ":")),
                span(class = "regla-contenido", contenido), badge))
      ))
    }

    lineas <- reglas_filtradas()
    n <- length(lineas)
    if (n == 0) return(p("Ninguna regla coincide con el filtro."))

    modo_rango <- input$reglas_modo_rango; if (is.null(modo_rango)) modo_rango <- "rango"
    if (identical(modo_rango, "todas")) {
      desde <- 1; hasta <- n
    } else {
      desde <- input$reglas_desde; hasta <- input$reglas_hasta
      if (is.null(desde) || is.na(desde)) desde <- 1
      if (is.null(hasta) || is.na(hasta)) hasta <- min(10, n)
      desde <- max(1, min(desde, n))
      hasta <- max(desde, min(hasta, n))
    }

    sop <- reglas_soporte()
    seleccion <- lineas[desde:hasta]
    elementos <- lapply(seleccion, function(bloque) {
      partes    <- strsplit(bloque, ":", fixed = TRUE)[[1]]
      titulo    <- trimws(partes[1])
      contenido <- trimws(gsub("\\s+", " ", paste(partes[-1], collapse = ":")))
      nr    <- num_regla(bloque)
      badge <- if (length(sop) > 0 && !is.na(nr) && nr >= 1 && nr <= length(sop))
        span(style = "color:#8a929b; font-size:11px; margin-left:8px;",
             sprintf("· soporte %s%%", format(round(sop[nr] * 100, 1), trim = TRUE))) else NULL
      div(class = "regla-bloque",
          span(class = "rule-header", paste0(titulo, ":")),
          span(class = "regla-contenido", contenido), badge)
    })

    tagList(
      div(style = "font-size:12px; color:#777; margin-bottom:4px;",
          sprintf("Mostrando %d–%d de %d%s", desde, hasta, n,
                  if (n < total) sprintf(" (filtradas de %d)", total) else "")),
      div(class = "reglas-container", do.call(tagList, elementos))
    )
  })
  
  # RETÍCULO DE CONCEPTOS (FCA) — sub-retículo de los ATRIBUTOS elegidos (legible)
  output$reticulo_info <- renderUI({
    fc <- fc_global()
    if (is.null(fc)) return(p(class = "text-muted",
                              "Extrae las reglas primero (Paso 2) para ver el retículo."))
    n <- tryCatch(fc$concepts$size(), error = function(e) NA_integer_)
    p(style = "font-size:13px; color:#555; margin-bottom:6px;",
      sprintf("El retículo completo tiene %s conceptos.", ifelse(is.na(n), "?", n)))
  })
  output$reticulo_control <- renderUI({
    req(atributos_global())
    tagList(
      radioButtons("reticulo_modo", NULL,
                   choices  = c("Personalizado" = "custom", "Todos" = "todos"),
                   selected = "custom", inline = TRUE),
      conditionalPanel(
        condition = "input.reticulo_modo == 'custom'",
        selectizeInput("reticulo_attrs", "Atributos del sub-retículo",
                       choices = sort(atributos_global()), multiple = TRUE,
                       options = list(placeholder = "Elige atributos…"), width = "100%")
      )
    )
  })
  # Personalizado: retículo INTERACTIVO del sub-contexto (tooltip = objetos/atributos)
  output$reticulo_vis <- renderVisNetwork({
    req(matriz_global())
    attrs <- intersect(input$reticulo_attrs, colnames(matriz_global()))
    req(length(attrs) >= 2)
    sub <- matriz_global()[, attrs, drop = FALSE]
    tryCatch({
      fcs <- FormalContext$new(sub)
      fcs$find_concepts()
      reticulo_visnetwork(fcs)
    }, error = function(e) {
      message("Retículo interactivo: ", conditionMessage(e))
      visNetwork(data.frame(id = 1, label = "No se pudo construir el retículo", shape = "text"),
                 data.frame())
    })
  })
  # Todos: retículo COMPLETO estático (solo estructura, sin etiquetas)
  output$reticulo_fca <- renderPlot({
    req(fc_global())
    g <- tryCatch(fc_global()$concepts$plot(mode = "empty"),
                  error = function(e) tryCatch(fc_global()$concepts$plot(), error = function(e2) NULL))
    if (inherits(g, "ggplot")) print(g)
  })

  # Tablas (reactable) y métricas (plotly): solo lectura de lo ya calculado.

  # Tabla de reglas — sustituye los bloques HTML por una tabla profesional.
  output$tabla_reglas <- renderReactable({
    df <- reglas_a_dataframe(reglas_lineas())
    if (nrow(df) == 0)
      return(reactable(data.frame(Info = "Sube un CSV y extrae las reglas para verlas aquí."),
                       sortable = FALSE, pagination = FALSE))
    reactable(
      df,
      searchable = TRUE, filterable = TRUE, striped = TRUE, highlight = TRUE, compact = TRUE,
      defaultPageSize = 12, showPageSizeOptions = TRUE, pageSizeOptions = c(12, 25, 50),
      defaultSorted = list(NCons = "desc"),
      columns = list(
        Regla       = colDef(name = "Regla",   maxWidth = 90),
        Antecedente = colDef(name = "Si se cumple…",           minWidth = 200),
        Consecuente = colDef(name = "…entonces (obligatorio)", minWidth = 200),
        NAnt        = colDef(name = "Nº ant.",  align = "center", maxWidth = 90),
        NCons       = colDef(name = "Nº cons.", align = "center", maxWidth = 90)
      )
    )
  })

  # Explorador de las tripletas (extraídas por IA o subidas por CSV).
  output$tabla_tripletas <- renderReactable({
    df <- tripletas_extraidas()
    if (is.null(df) && !is.null(ruta_csv_activa()))
      df <- tryCatch(read.csv(ruta_csv_activa(), stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0)
      return(reactable(data.frame(Info = "Aún no hay tripletas. Extrae de un documento o sube un CSV."),
                       sortable = FALSE, pagination = FALSE))
    reactable(
      df, searchable = TRUE, filterable = TRUE, striped = TRUE, highlight = TRUE, compact = TRUE,
      defaultPageSize = 15, showPageSizeOptions = TRUE, pageSizeOptions = c(15, 30, 60)
    )
  })

  # --- Métricas (plotly sobre ggplot; analítica agregada, no re-dibuja el grafo) ---
  output$plot_attrs <- renderPlotly({
    m <- matriz_global(); req(m)
    fr <- head(sort(colSums(m), decreasing = TRUE), 15)
    d  <- data.frame(attr = gsub("_", " ", names(fr)), n = as.integer(fr), stringsAsFactors = FALSE)
    d$attr <- factor(d$attr, levels = rev(d$attr))
    g <- ggplot(d, aes(x = n, y = attr)) +
      geom_col(fill = "#0d6e6e") +
      labs(x = "Frecuencia", y = NULL) + theme_minimal(base_size = 11)
    ggplotly(g, tooltip = c("x", "y")) %>% config(displayModeBar = FALSE)
  })

  output$plot_nodos <- renderPlotly({
    nb <- nodos_base_global(); req(nb)
    d  <- as.data.frame(table(Tipo = nb$group))
    g <- ggplot(d, aes(x = Tipo, y = Freq, fill = Tipo)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "Nº de nodos") + theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
    ggplotly(g, tooltip = c("y")) %>% config(displayModeBar = FALSE)
  })

  output$plot_reglas <- renderPlotly({
    df <- reglas_a_dataframe(reglas_lineas()); req(nrow(df) > 0)
    d  <- as.data.frame(table(Tamano = df$NAnt + df$NCons))
    g <- ggplot(d, aes(x = Tamano, y = Freq)) +
      geom_col(fill = "#18BC9C") +
      labs(x = "Nº atributos por regla", y = "Nº de reglas") + theme_minimal(base_size = 11)
    ggplotly(g, tooltip = c("y")) %>% config(displayModeBar = FALSE)
  })

  # Evaluación: comparativa de los 5 enfoques.
  observeEvent(input$btn_evaluar, {
    req(fc_global(), igraph_global(), matriz_global(), objetos_global(), atributos_global())
    ig <- igraph_global()
    e  <- igraph::as_data_frame(ig, what = "edges")
    grafo_limpio <- data.frame(Sujeto = e$from, Relacion = e$Relacion, Objeto = e$to,
                               stringsAsFactors = FALSE)
    ctx <- list(fc = fc_global(), igraph_obj = ig, matriz = matriz_global(),
                objetos = objetos_global(), atributos = atributos_global(),
                grafo_limpio = grafo_limpio)
    reps <- suppressWarnings(as.integer(input$eval_reps))
    if (length(reps) == 0 || is.na(reps) || reps < 1) reps <- 2L
    res <- withProgress(message = "Ejecutando evaluación…", detail = "Preparando…", value = 0, {
      tryCatch(
        ejecutar_evaluacion(ctx, model = "llama3.1", usar_embeddings = isTRUE(input$eval_emb),
                            repeticiones = reps,
                            progreso = function(p, msg) setProgress(value = p, detail = msg)),
        error = function(err) {
          showNotification(paste("Error en evaluación:", conditionMessage(err)),
                           type = "error", duration = 10); NULL })
    })
    if (is.null(res)) return(NULL)
    resultado_eval(res)
    resumen_eval(resumen_evaluacion(res))
    resumen_eval_manual(resumen_evaluacion(res, fuente = "manual"))   # gold independiente
    showNotification("Evaluación completada.", type = "message", duration = 5)
  })

  output$eval_estado <- renderUI({
    if (is.null(resultado_eval()))
      p(class = "text-muted",
        "Pulsa 'Ejecutar evaluación' (requiere el contexto FCA construido en el Paso 2 y Ollama activo).")
    else
      p(class = "estado-ok", icon("circle-check"),
        sprintf(" %d preguntas × %d enfoques evaluados. Motor RAG: %s.",
                length(unique(resultado_eval()$id)),
                length(unique(resultado_eval()$enfoque)),
                attr(resultado_eval(), "motor_rag")))
  })

  # Botones de descarga del informe (solo aparecen cuando hay resultados)
  output$eval_descargas <- renderUI({
    req(resultado_eval())
    div(class = "d-flex gap-2 flex-wrap mb-2",
        downloadButton("dl_eval_html", "Descargar informe (HTML)",
                       icon = icon("file-arrow-down"), class = "btn-sm btn-outline-primary"),
        downloadButton("dl_eval_csv", "Descargar datos (CSV)",
                       icon = icon("table"), class = "btn-sm btn-outline-secondary"))
  })
  output$dl_eval_html <- downloadHandler(
    filename = function() paste0("informe_evaluacion_", format(Sys.time(), "%Y%m%d_%H%M"), ".html"),
    content  = function(file) {
      req(resultado_eval())
      writeLines(informe_html_eval(resultado_eval()), file, useBytes = TRUE)
    }
  )
  output$dl_eval_csv <- downloadHandler(
    filename = function() paste0("resultados_evaluacion_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
    content  = function(file) {
      req(resultado_eval())
      write.csv(resultado_eval(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$tabla_resumen_eval <- renderReactable({
    req(resumen_eval())
    reactable(resumen_eval(), striped = TRUE, highlight = TRUE, compact = TRUE, pagination = FALSE,
              defaultColDef = colDef(align = "center"),
              columns = list(Enfoque = colDef(align = "left", minWidth = 150)))
  })

  output$tabla_resumen_eval_manual <- renderReactable({
    req(resumen_eval_manual())
    reactable(resumen_eval_manual(), striped = TRUE, highlight = TRUE, compact = TRUE, pagination = FALSE,
              defaultColDef = colDef(align = "center"),
              columns = list(Enfoque = colDef(align = "left", minWidth = 150)))
  })

  output$tabla_detalle_eval <- renderReactable({
    req(resultado_eval())
    reactable(resultado_eval(), searchable = TRUE, filterable = TRUE, striped = TRUE,
              compact = TRUE, defaultPageSize = 12, showPageSizeOptions = TRUE,
              pageSizeOptions = c(12, 25, 60))
  })

  output$plot_eval_halluc <- renderPlotly({
    req(resultado_eval())
    d <- resumen_num(resultado_eval()); d$Enfoque <- factor(d$Enfoque, levels = BRAZOS_EVAL)
    g <- ggplot(d, aes(x = Enfoque, y = Alucinacion, fill = Enfoque)) +
      geom_col(show.legend = FALSE) +
      geom_errorbar(aes(ymin = pmax(0, Alucinacion - Alucinacion_sd), ymax = Alucinacion + Alucinacion_sd),
                    width = 0.25, color = "#444444") +
      labs(x = NULL, y = "Alucinación") +
      theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 20, hjust = 1))
    ggplotly(g) %>% config(displayModeBar = FALSE)
  })

  output$plot_eval_exact <- renderPlotly({
    req(resultado_eval())
    d <- resumen_num(resultado_eval()); d$Enfoque <- factor(d$Enfoque, levels = BRAZOS_EVAL)
    g <- ggplot(d, aes(x = Enfoque, y = Exactitud, fill = Enfoque)) +
      geom_col(show.legend = FALSE) +
      geom_errorbar(aes(ymin = pmax(0, Exactitud - Exactitud_sd), ymax = pmin(1, Exactitud + Exactitud_sd)),
                    width = 0.25, color = "#444444") +
      labs(x = NULL, y = "Exactitud") +
      theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 20, hjust = 1))
    ggplotly(g) %>% config(displayModeBar = FALSE)
  })

  # EVENTO 4: LIMPIEZA
  session$onSessionEnded(function() {
    print("Aplicación cerrada. Liberando memoria RAM de Ollama...")
  })
  # Controles del grafo: solo aparecen cuando el grafo está construido
  output$controles_grafo <- renderUI({
    req(nodos_base_global())
    div(style = "display:flex; gap:18px; flex-wrap:wrap; align-items:flex-end; margin-bottom:8px;",
      selectInput(
        "filtro_grafo_grupo",
        label    = "Tipo de nodo",
        choices  = c("\u2014 Todos los tipos \u2014" = "TODOS",
                     "Paciente"  = "Paciente",
                     "Protocolo" = "Protocolo",
                     "Entidad"   = "Entidad",
                     "Atributo"  = "Atributo"),
        selected = "TODOS",
        width    = "210px"
      ),
      selectInput(
        "filtro_grafo_item",
        label    = "Elemento",
        choices  = c("Todos" = "TODOS"),
        selected = "TODOS",
        width    = "260px"
      )
    )
  })

  # Caja de la leyenda: solo aparece tras "Extraer Reglas" (igual que los controles).
  output$leyenda_box <- renderUI({
    req(nodos_base_global())
    accordion(open = FALSE,
              accordion_panel("Leyenda", uiOutput("leyenda_grafo")))
  })

  # Leyenda del grafo: marcadores SVG con la forma real del nodo y aristas con flecha.
  output$leyenda_grafo <- renderUI({
    nodo_svg <- function(color, forma) {
      shape <- if (forma == "ellipse")
        sprintf('<ellipse cx="16" cy="10" rx="15" ry="8.5" fill="%s"/>', color)
      else
        sprintf('<rect x="1" y="1.5" width="30" height="17" rx="4" fill="%s"/>', color)
      HTML(sprintf('<svg width="32" height="20" viewBox="0 0 32 20">%s</svg>', shape))
    }
    arista_svg <- function(color) {
      HTML(sprintf(
        '<svg width="36" height="12" viewBox="0 0 36 12"><line x1="1" y1="6" x2="27" y2="6" stroke="%s" stroke-width="3" stroke-linecap="round"/><path d="M27 1.5 L35 6 L27 10.5 Z" fill="%s"/></svg>',
        color, color))
    }
    item <- function(marca, texto) {
      div(style = "display:flex; align-items:center; gap:10px; padding:7px 11px; background:#f8f9fa;
                   border:1px solid #edeff1; border-radius:9px;",
          span(style = "display:inline-flex; width:36px; justify-content:center; flex:0 0 auto;", marca),
          span(style = "font-size:13px; color:#2c3e50; line-height:1.2;", texto))
    }
    seccion <- function(titulo) div(
      style = "grid-column:1 / -1; font-size:11px; font-weight:700; letter-spacing:.07em;
               text-transform:uppercase; color:#8a929a; margin:8px 2px 0;", titulo)
    rejilla <- function(...) div(
      style = "display:grid; grid-template-columns:repeat(auto-fill, minmax(195px, 1fr)); gap:8px;", ...)

    rejilla(
      seccion("Nodos"),
      item(nodo_svg(PALETA$Paciente,  "box"),     "Paciente"),
      item(nodo_svg(PALETA$Protocolo, "box"),     "Protocolo"),
      item(nodo_svg(PALETA$Entidad,   "box"),     "Entidad"),
      item(nodo_svg(PALETA$Atributo,  "ellipse"), "Atributo"),
      seccion("Relaciones (aristas)"),
      item(arista_svg("#7F8C8D"), "s\u00edntoma / condici\u00f3n"),
      item(arista_svg("#34495E"), "diagn\u00f3stico"),
      item(arista_svg("#27AE60"), "tratamiento"),
      item(arista_svg("#16A085"), "trata (indicaci\u00f3n)"),
      item(arista_svg("#2980B9"), "grupo (pertenece)"),
      item(arista_svg("#E74C3C"), "contraindicaci\u00f3n"),
      item(arista_svg("#8E44AD"), "implica (riesgo)"),
      item(arista_svg("#E67E22"), "requiere (protocolo)")
    )
  })
}