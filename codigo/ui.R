# ui.R — Interfaz de usuario.
page_sidebar(
  title = "Motor Inferencia Neuro-Simbólico (FCA · Graph-RAG)",
  theme = bs_theme(
    bootswatch   = "flatly",
    base_font    = font_google("Inter"),
    heading_font = font_google("Poppins"),
    code_font    = font_google("JetBrains Mono")
  ),

  tags$head(
    tags$style(HTML("
      /* Paleta unificada: barra superior con el mismo azul marino que las cabeceras */
      .navbar, .navbar-default, nav.navbar { background-color: #2C3E50 !important; border-color: #2C3E50 !important; }
      .navbar .navbar-brand, .navbar-brand, .navbar .navbar-title, .navbar a { color: #ffffff !important; }
      pre { white-space: pre-wrap !important; word-wrap: break-word !important; overflow-x: hidden !important; }
      .reglas-container { font-family: 'Consolas', monospace; font-size: 13px; line-height: 1.4;
                          padding: 5px; box-sizing: border-box; width: 100%; }
      .regla-bloque { background-color: #f8f9fa; border: 1px solid #e0e0e0; padding: 8px 12px;
                      margin-bottom: 6px; border-radius: 5px; white-space: normal; word-wrap: break-word;
                      box-sizing: border-box; width: 100%; }
      .rule-header { font-weight: bold; color: #2c3e50; display: inline-block; margin-right: 6px; }
      .regla-contenido { color: #333333; }
      #archivo_datos_progress, #archivo_documento_progress { display: none; }
      #reglas_buscar::placeholder { color: #c7ccd1; opacity: 1; font-style: italic; }
      .reglas-compacta .form-group,
      .reglas-compacta .shiny-input-container { margin-bottom: .35rem !important; }
      .reglas-compacta label { margin-bottom: .1rem !important; }
      .reglas-compacta .shiny-options-group { margin-top: .1rem !important; }
      .reglas-compacta hr { margin: .4rem 0 !important; }
      .estado-ok  { color: #18BC9C; font-weight: bold; font-size: 13px; }
      .estado-err { color: #E74C3C; font-weight: bold; font-size: 13px; }
      .seccion-paso { font-size: 12px; color: #555; text-transform: uppercase;
                      letter-spacing: 0.05em; margin-bottom: 4px; margin-top: 6px; }
      /* Interruptor de modo avanzado: color coherente y algo de espacio inferior */
      .form-switch { margin-bottom: 8px; }
      .form-switch .form-check-input:checked { background-color: #2C3E50; border-color: #2C3E50; }
      .form-switch .form-check-label { font-size: 14px; }
      /* (Se retira la ocultación con :empty; hacía que la confirmación y el botón de
         descarga no aparecieran tras la extracción. Un uiOutput vacío ya ocupa 0 px.) */

      /* ===== Ajustes para el MODO OSCURO (colores fijados para claro que no se adaptan) ===== */
      [data-bs-theme=\"dark\"] .seccion-paso { color: #a7b0b8 !important; }
      [data-bs-theme=\"dark\"] .rule-header { color: #e6ebf0; }
      [data-bs-theme=\"dark\"] .regla-contenido,
      [data-bs-theme=\"dark\"] .reglas-container { color: #c7cfd6; }
      [data-bs-theme=\"dark\"] .regla-bloque { background-color: #2b323a; border-color: #3a434d; }
      /* Colores fijados en línea (renderUI, leyenda): se aclaran u oscurecen según el caso */
      [data-bs-theme=\"dark\"] [style*=\"#555\"], [data-bs-theme=\"dark\"] [style*=\"#777\"],
      [data-bs-theme=\"dark\"] [style*=\"#888\"], [data-bs-theme=\"dark\"] [style*=\"#8a929a\"],
      [data-bs-theme=\"dark\"] [style*=\"#2c3e50\"], [data-bs-theme=\"dark\"] [style*=\"#667\"] { color: #a7b0b8 !important; }
      [data-bs-theme=\"dark\"] [style*=\"#f8f9fa\"], [data-bs-theme=\"dark\"] [style*=\"#f5f7f8\"] { background-color: #2b323a !important; }
      [data-bs-theme=\"dark\"] [style*=\"#edeff1\"], [data-bs-theme=\"dark\"] [style*=\"#e0e0e0\"] { border-color: #3a434d !important; }
      /* KPIs (pestaña 'Datos') — rejilla 2x2, tamaño medio */
      .kpi-row .bslib-grid { gap: 12px !important; }
      .kpi-row .bslib-value-box,
      .kpi-row .value-box { min-height: 96px !important; }
      .kpi-row .value-box-area { padding: .6rem .9rem !important; }
      .kpi-row .value-box-title { font-size: 14px !important; margin: 0 0 3px 0 !important; opacity: .9; }
      .kpi-row .value-box-value { font-size: 28px !important; margin: 0 !important; line-height: 1.1; }
      .kpi-row .value-box-showcase { padding: 0 .6rem !important; }
      .kpi-row .value-box-showcase svg { width: 2.1rem !important; height: 2.1rem !important; }
      /* Panel dividido redimensionable (chat | visualización), con scroll interno. */
      #split_row { display: flex; flex-direction: row; width: 100%; align-items: stretch;
                   height: calc(100vh - 120px); }
      #split_row .card { height: 100%; }
      #pane_chat { flex: 0 0 42%; min-width: 280px; overflow: hidden; }
      #pane_viz  { flex: 1 1 auto; min-width: 0; overflow: hidden; }
      #pane_viz.viz-hidden, #pane_chat.viz-hidden { display: none; }
      #split_gutter { flex: 0 0 10px; align-self: stretch; cursor: col-resize;
                      background: #dfe3e6; border-radius: 4px; margin: 0 4px;
                      transition: background .15s; }
      #split_gutter:hover, #split_gutter.dragging { background: #9aa7b0; }
      .btn-toggle-viz { line-height: 1; }

      /* CHAT: scroll dentro del cuerpo, no del panel */
      #pane_chat > .card { display: flex; flex-direction: column; min-height: 0; }
      #pane_chat > .card > .card-body { flex: 1 1 auto; min-height: 0; overflow: hidden; }

      /* Visualización: alto constante y scroll interno en las pestañas. */
      #pane_viz > .card { display: flex; flex-direction: column; min-height: 0; }
      #pane_viz > .card > .card-header { flex: 0 0 auto; }
      #pane_viz > .card > .tabbable,
      #pane_viz > .card > .bslib-navs-card-title,
      #pane_viz > .card > div:not(.card-header) {
        display: flex; flex-direction: column; flex: 1 1 auto; min-height: 0;
      }
      #pane_viz .nav-tabs { flex: 0 0 auto; flex-wrap: nowrap; white-space: nowrap; }
      #pane_viz .nav-tabs .nav-item { flex: 1 1 0; text-align: center; }
      #pane_viz .tab-content { flex: 1 1 auto; min-height: 0; overflow-y: auto; padding: 4px 2px; }

      /* Pestaña seleccionada: recuadro verde con letra blanca */
      #pane_viz .nav-tabs .nav-link { color: #18BC9C; border-radius: 6px 6px 0 0;
                                      padding: .6rem 1rem; font-size: 15.5px; }
      #pane_viz .nav-tabs .nav-link:hover { color: #12967c; }
      #pane_viz .nav-tabs .nav-link.active,
      #pane_viz .nav-tabs .nav-link.active:hover {
        background-color: #18BC9C !important;
        border-color: #18BC9C !important;
        color: #ffffff !important;
        font-weight: 600;
      }

      /* Barra lateral: contenido pegado arriba y más compacto */
      .bslib-sidebar-layout > .sidebar > .sidebar-content,
      .sidebar-content { padding-top: 6px !important; }
      .sidebar-content > h5:first-of-type,
      .sidebar-content > h4:first-of-type { margin-top: 2px !important; }
      .sidebar-content .shiny-input-container,
      .sidebar-content .form-group { margin-bottom: .45rem !important; }
      .sidebar-content .seccion-paso { margin-top: 4px !important; margin-bottom: 3px !important; }
      .sidebar-content hr { margin: .5rem 0 !important; }

      /* Aviso de progreso arriba a la derecha, solo texto (sin barra). */
      #shiny-notification-panel { top: 12px; bottom: auto; right: 16px; left: auto; width: 320px; }
      .shiny-notification { font-size: 14px; }
      .shiny-notification .progress { display: none !important; }
    "))
  ),

  # Divisor arrastrable propio (sin dependencias externas) + toggle de la visualización
  tags$script(HTML("
    (function(){
      function init(){
        var row  = document.getElementById('split_row'),
            left = document.getElementById('pane_chat'),
            g    = document.getElementById('split_gutter'),
            right= document.getElementById('pane_viz');
        if(!row || !left || !g || !right){ return setTimeout(init, 150); }
        if(g.dataset.ready) return; g.dataset.ready = '1';
        var dragging = false;
        function onMove(e){
          if(!dragging) return;
          var rect = row.getBoundingClientRect();
          var cx = (e.touches ? e.touches[0].clientX : e.clientX) - rect.left;
          var min = 200, max = rect.width - 220;
          cx = Math.max(min, Math.min(max, cx));
          left.style.flex = '0 0 ' + (cx / rect.width * 100) + '%';
          right.style.flex = '1 1 auto';
          if(e.cancelable) e.preventDefault();
        }
        function stop(){
          if(!dragging) return;
          dragging = false; g.classList.remove('dragging');
          document.body.style.userSelect = '';
          window.dispatchEvent(new Event('resize'));
        }
        function start(e){ dragging = true; g.classList.add('dragging');
                           document.body.style.userSelect = 'none'; if(e.cancelable) e.preventDefault(); }
        g.addEventListener('mousedown', start);
        g.addEventListener('touchstart', start, {passive:false});
        document.addEventListener('mousemove', onMove);
        document.addEventListener('touchmove', onMove, {passive:false});
        document.addEventListener('mouseup', stop);
        document.addEventListener('touchend', stop);
      }
      // Oculta/muestra la visualización (chat a pantalla completa) o la restaura.
      window.toggleViz = function(){
        var left = document.getElementById('pane_chat'),
            g    = document.getElementById('split_gutter'),
            right= document.getElementById('pane_viz');
        if(!left || !right) return;
        if(right.classList.contains('viz-hidden')){
          right.classList.remove('viz-hidden');
          if(g) g.style.display = '';
          left.style.flex = left.dataset.prevflex || '0 0 42%';
        } else {
          left.dataset.prevflex = left.style.flex || '0 0 42%';
          right.classList.add('viz-hidden');
          if(g) g.style.display = 'none';
          left.style.flex = '1 1 auto';
        }
        setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 60);
      };
      // Oculta/muestra la conversación (visualización a pantalla completa) o la restaura.
      window.toggleChat = function(){
        var left = document.getElementById('pane_chat'),
            g    = document.getElementById('split_gutter'),
            right= document.getElementById('pane_viz');
        if(!left || !right) return;
        if(left.classList.contains('viz-hidden')){
          left.classList.remove('viz-hidden');
          if(g) g.style.display = '';
          left.style.flex = left.dataset.prevflex || '0 0 42%';
          right.style.flex = '1 1 auto';
        } else {
          left.dataset.prevflex = left.style.flex || '0 0 42%';
          left.classList.add('viz-hidden');
          if(g) g.style.display = 'none';
          right.style.flex = '1 1 auto';
        }
        setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 60);
      };
      if(document.readyState !== 'loading') init();
      else document.addEventListener('DOMContentLoaded', init);
    })();
  ")),

  # Evita que la rueda del ratón desplace el panel para que haga zoom en el grafo.
  tags$script(HTML("
    (function(){
      function att(){
        var g = document.getElementById('grafo_fca');
        if(!g){ return setTimeout(att, 300); }
        if(g.dataset.wheelfix) return; g.dataset.wheelfix = '1';
        g.addEventListener('wheel', function(e){ e.preventDefault(); }, {passive:false});
      }
      if(document.readyState !== 'loading') att();
      else document.addEventListener('DOMContentLoaded', att);
    })();
  ")),

  # --- Inicializadores de la capa visual (no afectan a la lógica de razonamiento) ---
  useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  waiter::useWaiter(),
  waiter::useWaitress(),
  rintrojs::introjsUI(),

  sidebar = sidebar(
    width = 420,

    # Controles alineados a la altura de las cabeceras de la derecha.
    div(class = "d-flex justify-content-start align-items-center gap-2",
        style = "margin-top: 14px; margin-bottom: 10px; padding-right: 44px;",
        actionButton("tour", "¿Cómo funciona?", icon = icon("circle-question"),
                     class = "btn-sm btn-outline-secondary"),
        if (exists("input_dark_mode", where = asNamespace("bslib")))
          bslib::input_dark_mode(id = "modo_oscuro", mode = "light") else NULL),

    # Modo avanzado: desactivado = Básico (solo preguntar/responder); activado = todo visible.
    if (exists("input_switch", where = asNamespace("bslib")))
      bslib::input_switch("modo_avanzado", "Modo avanzado", value = FALSE)
    else
      checkboxInput("modo_avanzado", "Modo avanzado", value = FALSE),

    h5("Entrada de Conocimiento"),

    p(class = "seccion-paso", "1 — Cargar conocimiento"),

    # Opción A: extraer de un documento con IA
    fileInput("archivo_documento", "Documento médico (PDF / TXT)",
              accept = c(".pdf", ".txt", "text/plain", "application/pdf"),
              buttonLabel = "Examinar…", placeholder = "Ningún documento"),
    input_task_button("btn_extraer", "Extraer con IA",
                      icon = icon("wand-magic-sparkles"), type = "success"),
    uiOutput("estado_extraccion"),
    uiOutput("ui_descarga_csv"),

    div(class = "text-center text-muted small my-1", "— o —"),

    # Opción B: subir el CSV de tripletas directamente
    div(id = "bloque_csv",
        fileInput("archivo_datos", "Subir tripletas (.csv)",
                  accept = c("text/csv", ".csv"),
                  buttonLabel = "Examinar…", placeholder = "Ningún archivo")),

    # Carga rápida del CSV de ejemplo incluido con la app.
    actionButton("btn_ejemplo", "Usar datos de ejemplo",
                 icon = icon("flask-vial"), class = "btn-sm btn-outline-secondary"),

    hr(),

    p(class = "seccion-paso", "2 — Análisis FCA"),
    input_task_button("btn_analizar", "Extraer Reglas y Contexto",
                      icon = icon("gears"), type = "primary")
  ),

  div(
    id = "split_row",
    div(
      class = "split-pane", id = "pane_chat",
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              span("Interacción Asistida por IA"),
              tags$button(id = "btn_toggle_viz", type = "button",
                          class = "btn btn-sm btn-outline-light btn-toggle-viz py-0 px-2",
                          onclick = "toggleViz()",
                          title = "Mostrar / ocultar visualización",
                          icon("angles-right"))),
          class = "bg-primary text-white"),
        card_body(chat_ui("chat_llm"))
      )
    ),
    div(id = "split_gutter", title = "Arrastra para redimensionar"),
    div(
      class = "split-pane", id = "pane_viz",
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              span("Visualización"),
              tags$button(id = "btn_toggle_chat", type = "button",
                          class = "btn btn-sm btn-outline-light btn-toggle-viz py-0 px-2",
                          onclick = "toggleChat()",
                          title = "Mostrar / ocultar la conversación",
                          icon("angles-left"))),
          class = "bg-primary text-white"),
        navset_tab(
          id = "viz_tabs",
      # --- Pestaña: grafo de conocimiento (por defecto al iniciar) ---
      nav_panel(
        "Grafo", value = "grafo",
        uiOutput("controles_grafo"),
        # Altura fija en px: más estable para el zoom que una altura relativa (calc).
        visNetworkOutput("grafo_fca", height = "520px"),
        # Leyenda debajo; solo aparece tras "Extraer Reglas" (igual que los controles).
        uiOutput("leyenda_box")
      ),
      # --- Pestaña: retículo de conceptos (diagrama de Hasse) ---
      nav_panel(
        "Retículo", value = "reticulo",
        uiOutput("reticulo_info"),
        uiOutput("reticulo_control"),
        conditionalPanel(
          "input.reticulo_modo != 'todos'",
          visNetworkOutput("reticulo_vis", height = "430px")
        ),
        conditionalPanel(
          "input.reticulo_modo == 'todos'",
          div(style = "overflow:auto; max-height:430px;",
              plotOutput("reticulo_fca", height = "400px"))
        )
      ),
      # --- Pestaña: reglas FCA (tabla interactiva) ---
      nav_panel(
        "Reglas FCA", value = "reglas",
        div(class = "reglas-compacta",
            uiOutput("controles_reglas"),
            uiOutput("mostrar_reglas"))
      ),
      # --- Pestaña: Datos (KPIs + Métricas + Tripletas) — última opción ---
      nav_panel(
        "Datos", value = "datos",
        div(class = "kpi-row", uiOutput("kpis")),
        navset_pill(
          # Sub-pestaña: métricas / cuadro analítico
          nav_panel(
            "Métricas",
            layout_columns(
              col_widths = c(12, 6, 6),
              card(card_header("Atributos más frecuentes"),
                   plotlyOutput("plot_attrs", height = "260px")),
              card(card_header("Nodos por tipo"),
                   plotlyOutput("plot_nodos", height = "240px")),
              card(card_header("Tamaño de las reglas"),
                   plotlyOutput("plot_reglas", height = "240px"))
            )
          ),
          # Sub-pestaña: explorador de tripletas extraídas
          nav_panel(
            "Tripletas",
            reactableOutput("tabla_tripletas")
          )
        )
      ),
      # --- Pestaña: evaluación / validación (comparativa de brazos) ---
      nav_panel(
        "Evaluación", value = "evaluacion",
        div(class = "d-flex align-items-center gap-3 flex-wrap mb-2",
            input_task_button("btn_evaluar", "Ejecutar evaluación",
                              icon = icon("flask"), type = "primary"),
            checkboxInput("eval_emb", "Usar embeddings (más lento)", value = FALSE),
            div(style = "width:150px;",
                numericInput("eval_reps", "Repeticiones LLM", value = 2, min = 1, max = 5, step = 1)),
            div(class = "text-muted small",
                "Compara LLM solo · RAG+LLM · Graph-RAG+LLM · FCA solo · Graph-RAG+FCA")),
        uiOutput("eval_estado"),
        uiOutput("eval_descargas"),
        card(card_header("Resumen por enfoque"),
             reactableOutput("tabla_resumen_eval")),
        card(card_header("Resumen solo con gold manual (sin circularidad)"),
             reactableOutput("tabla_resumen_eval_manual")),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Tasa de alucinación (menor es mejor)"),
               plotlyOutput("plot_eval_halluc", height = "300px")),
          card(card_header("Exactitud global (mayor es mejor)"),
               plotlyOutput("plot_eval_exact", height = "300px"))
        ),
        card(card_header("Detalle por pregunta"),
             reactableOutput("tabla_detalle_eval"))
      )
        )
      )
    )
  )
)
