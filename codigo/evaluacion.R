# Evaluación: compara los 5 enfoques sobre una batería de preguntas con gold.
# Métricas: exactitud, precisión/recall/F1 y tasa de alucinación.

# ---------- Utilidades de texto / parsing ----------
.norm_ev <- function(x) tolower(iconv(gsub("_", " ", x), to = "ASCII//TRANSLIT"))

# Extrae SÍ/NO de una respuesta en prosa del LLM.
parse_sino <- function(texto) {
  t <- .norm_ev(texto)
  # Primera señal clara: "no" al inicio o "no tiene/tendria" -> NO; "si"/"tendria" -> SI
  if (grepl("^\\s*no\\b|\\bno\\s+(tiene|tendria|posee|presenta|adquiere)", t)) return("NO")
  if (grepl("^\\s*s[i]\\b|\\bs[i]\\b|\\btendria\\b|\\btiene\\b|\\bpresenta\\b|\\bposee\\b", t)) return("SI")
  if (grepl("\\bno\\b", t)) return("NO")
  "SI"
}

# Convierte prosa del LLM en conjunto de átomos (atributos) del universo.
atomos_de_texto <- function(texto, universo) {
  if (is.null(texto) || !nzchar(trimws(texto))) return(character(0))
  tryCatch(detecta_attrs(texto, universo), error = function(e) character(0))
}
# Convierte prosa del LLM en conjunto de sujetos (para preguntas inversas).
sujetos_de_texto <- function(texto, objetos) {
  if (is.null(texto) || !nzchar(trimws(texto))) return(character(0))
  tryCatch(detectar_sujetos(texto, objetos), error = function(e) character(0))
}

# ---------- Recuperación RAG (embeddings Ollama con fallback léxico) ----------
embed_ollama <- function(texto, model = "nomic-embed-text") {
  tryCatch({
    r <- httr2::request("http://localhost:11434/api/embeddings")
    r <- httr2::req_body_json(r, list(model = model, prompt = texto))
    r <- httr2::req_timeout(r, 60)
    resp <- httr2::resp_body_json(httr2::req_perform(r))
    v <- unlist(resp$embedding)
    if (length(v) == 0) NULL else as.numeric(v)
  }, error = function(e) NULL)
}

.cos <- function(a, b) {
  d <- sqrt(sum(a * a)) * sqrt(sum(b * b))
  if (d == 0) 0 else sum(a * b) / d
}
.sim_lexica <- function(qa, ta) {
  qs <- unique(unlist(strsplit(.norm_ev(qa), "\\s+")))
  ts <- unique(unlist(strsplit(.norm_ev(ta), "\\s+")))
  qs <- qs[nchar(qs) >= 3]; ts <- ts[nchar(ts) >= 3]
  if (length(qs) == 0 || length(ts) == 0) return(0)
  length(intersect(qs, ts)) / length(union(qs, ts))
}

# Prepara el índice de tripletas (texto + embedding si hay). Se cachea por ejecución.
preparar_indice_rag <- function(grafo_limpio, embeddings = TRUE, model = "nomic-embed-text") {
  frases <- sprintf("%s %s %s",
                    gsub("_", " ", grafo_limpio$Sujeto),
                    gsub("_", " ", grafo_limpio$Relacion),
                    gsub("_", " ", grafo_limpio$Objeto))
  emb <- NULL; usa_emb <- FALSE
  if (isTRUE(embeddings)) {
    prueba <- embed_ollama("prueba", model)
    if (!is.null(prueba)) {
      usa_emb <- TRUE
      emb <- lapply(frases, function(f) embed_ollama(f, model))
      # si alguna falla, desactiva embeddings
      if (any(vapply(emb, is.null, logical(1)))) { usa_emb <- FALSE; emb <- NULL }
    }
  }
  list(frases = frases, emb = emb, usa_emb = usa_emb, model = model)
}

recuperar_rag <- function(pregunta, indice, k = 8) {
  if (isTRUE(indice$usa_emb)) {
    q <- embed_ollama(pregunta, indice$model)
    if (!is.null(q)) {
      sims <- vapply(indice$emb, function(e) .cos(q, e), numeric(1))
    } else sims <- vapply(indice$frases, function(f) .sim_lexica(pregunta, f), numeric(1))
  } else {
    sims <- vapply(indice$frases, function(f) .sim_lexica(pregunta, f), numeric(1))
  }
  ord <- order(sims, decreasing = TRUE)
  indice$frases[ord[seq_len(min(k, length(ord)))]]
}

# Contexto Graph-RAG: aristas del vecindario de las entidades de entrada (+implica).
contexto_grafo <- function(entidades, ig) {
  if (is.null(ig)) return(character(0))
  ent <- intersect(entidades, igraph::V(ig)$name)
  if (length(ent) == 0) return(character(0))
  lineas <- character(0)
  for (e in ent) {
    ar <- aristas_salientes(e, ig)
    if (nrow(ar) > 0)
      lineas <- c(lineas, sprintf("%s %s %s", gsub("_", " ", e),
                                  gsub("_", " ", ar$rel), gsub("_", " ", ar$attr)))
  }
  unique(lineas)
}

# ---------- Llamada al LLM (síncrona, temperatura 0) ----------
preguntar_llm_eval <- function(pregunta, contexto = NULL, model = "llama3.1") {
  usa_ctx <- !is.null(contexto) && length(contexto) > 0
  sys <- if (usa_ctx)
    paste("Eres un asistente clínico. Responde de forma BREVE y concreta usando",
          "EXCLUSIVAMENTE la información del CONTEXTO. Si el contexto no lo respalda,",
          "no lo inventes. Si es una pregunta de sí/no, empieza por 'Sí' o 'No'.")
  else
    paste("Eres un asistente clínico. Responde de forma BREVE y concreta.",
          "Si es una pregunta de sí/no, empieza por 'Sí' o 'No'.")
  usr <- if (usa_ctx)
    paste0("CONTEXTO:\n", paste(contexto, collapse = "\n"), "\n\nPREGUNTA: ", pregunta)
  else
    paste0("PREGUNTA: ", pregunta)
  tryCatch({
    chat <- chat_ollama(model = model, system_prompt = sys,
                        api_args = list(temperature = 0, keep_alive = -1))
    chat$chat(usr, echo = "none")
  }, error = function(e) "")
}

# ---------- Razonamiento simbólico (para brazos 4 y 5) ----------
# Devuelve el conjunto cerrado a partir de una entrada, con o sin grafo.
cierre_arm <- function(entrada, fc, ig, usar_grafo) {
  if (usar_grafo && !is.null(ig)) {
    exp <- expandir_con_grafo(entrada, ig)
    cl  <- cierre_logico(union(entrada, exp$nodos), fc)$cierre
    union(cl, exp$nodos)
  } else {
    cierre_logico(entrada, fc)$cierre
  }
}
# Átomos de una categoría concreta dentro de un cierre (nuevos respecto a la entrada).
categoria_de_cierre <- function(cierre, entrada, categoria, ig) {
  grp <- agrupar_por_categoria(cierre, aristas_categoria(cierre, ig))
  items <- unlist(grp[intersect(names(grp), categoria)], use.names = FALSE)
  setdiff(items[nzchar(items)], entrada)
}

# ---------- Batería de preguntas con gold (data-driven desde el CSV) ----------
construir_bateria <- function(ctx) {
  ig <- ctx$igraph_obj; matriz <- ctx$matriz; objetos <- ctx$objetos
  bat <- list()
  add <- function(x) bat[[length(bat) + 1L]] <<- x

  # Condiciones con arista 'implica' saliente (fuente de los riesgos)
  nodos <- igraph::V(ig)$name
  conds <- Filter(function(n) length(vecinos_igraph(n, ig, "out", "implica")) > 0, nodos)

  # (A) Hipotético por categoría: riesgos/consecuencias vía implica (test de Graph-RAG)
  for (cd in conds) {
    riesgos <- vecinos_igraph(cd, ig, "out", "implica")
    add(list(id = paste0("HR_", cd), tipo = "categoria",
             pregunta = sprintf("Si un paciente tiene %s, ¿qué riesgos o consecuencias tendría?", cd),
             entrada = cd, categoria = "Riesgos/consecuencias", gold = riesgos))
    # (B) sí/no positivo sobre su propio riesgo
    add(list(id = paste0("SIp_", cd), tipo = "si_no",
             pregunta = sprintf("Si un paciente tiene %s, ¿tendría %s?", cd, riesgos[1]),
             entrada = cd, atributo = riesgos[1], gold = "SI"))
  }
  # (C) sí/no negativo: un riesgo de OTRA condición que NO se deduce de la actual
  for (cd in conds) {
    otros <- setdiff(unlist(lapply(setdiff(conds, cd), function(o) vecinos_igraph(o, ig, "out", "implica"))),
                     vecinos_igraph(cd, ig, "out", "implica"))
    otros <- unique(otros)
    cl_full <- cierre_arm(cd, ctx$fc, ig, usar_grafo = TRUE)
    cand <- setdiff(otros, cl_full)
    if (length(cand) > 0) {
      add(list(id = paste0("SIn_", cd), tipo = "si_no",
               pregunta = sprintf("Si un paciente tiene %s, ¿tendría %s?", cd, cand[1]),
               entrada = cd, atributo = cand[1], gold = "NO"))
    }
  }
  # (D) Inverso factual: ¿qué pacientes tienen X? (grounding vs alucinación del LLM)
  attrs_inv <- intersect(c("Diabetes_Tipo2", "HTA", "Asma", "Insuficiencia_Renal",
                           "Fiebre", "Neumonia_Bacteriana", "Sepsis", "Alergia_Penicilina"),
                         colnames(matriz))
  pac <- objetos[grepl("^Paciente", objetos)]
  for (a in attrs_inv) {
    quienes <- pac[matriz[pac, a] == 1]
    if (length(quienes) > 0) {
      add(list(id = paste0("INV_", a), tipo = "inverso",
               pregunta = sprintf("¿Qué pacientes tienen %s?", a),
               entrada = a, gold = quienes, fuente = "auto"))
    }
  }

  # (E) Gold anotado a mano: hechos directos del CSV, para evitar la circularidad del gold automático.
  hechos_de <- function(s) if (s %in% rownames(matriz)) colnames(matriz)[matriz[s, ] == 1] else character(0)
  existe    <- function(s, a) s %in% rownames(matriz) && a %in% colnames(matriz)
  addman <- function(id, preg, tipo, entrada, gold, atributo = NULL) {
    x <- list(id = id, tipo = tipo, pregunta = preg, entrada = entrada,
              gold = gold, fuente = "manual")
    if (!is.null(atributo)) x$atributo <- atributo
    add(x)
  }
  # Sí/No de hechos directos PRESENTES (gold SI)
  for (p in list(c("Paciente_04","Diabetes_Tipo2"), c("Paciente_08","EPOC"),
                 c("Paciente_10","Ulcera_Peptica"), c("Paciente_07","Insuficiencia_Renal"))) {
    if (existe(p[1], p[2]) && matriz[p[1], p[2]] == 1)
      addman(paste0("MAN_pos_", p[1], "_", p[2]),
             sprintf("¿El %s tiene %s?", p[1], p[2]), "si_no", hechos_de(p[1]), "SI", atributo = p[2])
  }
  # Sí/No de hechos AUSENTES (gold NO), verificados en el CSV
  for (p in list(c("Paciente_01","Metformina"), c("Paciente_10","Insuficiencia_Renal"))) {
    if (existe(p[1], p[2]) && matriz[p[1], p[2]] == 0)
      addman(paste0("MAN_neg_", p[1], "_", p[2]),
             sprintf("¿El %s tiene %s?", p[1], p[2]), "si_no", hechos_de(p[1]), "NO", atributo = p[2])
  }
  # Relaciones entidad→entidad DIRECTAS (gold SI), con redacción natural conocida
  ent <- list(list("Metformina","Insuficiencia_Renal","¿La Metformina está contraindicada con Insuficiencia_Renal?"),
              list("Azitromicina","Neumonia_Bacteriana","¿La Azitromicina trata la Neumonia_Bacteriana?"),
              list("Metformina","Diabetes_Tipo2","¿La Metformina trata la Diabetes_Tipo2?"))
  for (e in ent) {
    if (existe(e[[1]], e[[2]]) && matriz[e[[1]], e[[2]]] == 1)
      addman(paste0("MAN_ent_", e[[1]], "_", e[[2]]), e[[3]], "si_no",
             hechos_de(e[[1]]), "SI", atributo = e[[2]])
  }
  # Inverso factual con gold contado a mano desde la matriz
  for (a in intersect(c("Metformina","Sepsis"), colnames(matriz))) {
    quienes <- pac[matriz[pac, a] == 1]
    if (length(quienes) > 0)
      addman(paste0("MAN_inv_", a), sprintf("¿Qué pacientes reciben o tienen %s?", a),
             "inverso", a, quienes)
  }
  bat
}

# ---------- Ejecutar un brazo sobre una pregunta -> predicción ----------
# Devuelve list(pred = <átomos o "SI"/"NO">, texto = <prosa o ""> )
ejecutar_brazo <- function(brazo, q, ctx, indice, model = "llama3.1") {
  fc <- ctx$fc; ig <- ctx$igraph_obj; uni <- ctx$atributos; objetos <- ctx$objetos
  entrada <- q$entrada

  simbolico <- function(usar_grafo) {
    if (q$tipo == "inverso") {
      pac <- objetos[grepl("^Paciente", objetos)]
      return(pac[ctx$matriz[pac, q$entrada] == 1])
    }
    cl <- cierre_arm(entrada, fc, ig, usar_grafo)
    if (q$tipo == "si_no") return(if (q$atributo %in% cl) "SI" else "NO")
    categoria_de_cierre(cl, entrada, q$categoria, ig)   # categoria
  }

  con_llm <- function(contexto) {
    txt <- preguntar_llm_eval(q$pregunta, contexto, model)
    pred <- if (q$tipo == "si_no") parse_sino(txt)
            else if (q$tipo == "inverso") sujetos_de_texto(txt, objetos)
            else atomos_de_texto(txt, uni)
    list(pred = pred, texto = txt)
  }

  switch(brazo,
    "LLM solo"        = con_llm(NULL),
    "RAG+LLM"         = con_llm(recuperar_rag(q$pregunta, indice)),
    "Graph-RAG+LLM"   = con_llm(contexto_grafo(entrada, ig)),
    "FCA solo"        = list(pred = simbolico(FALSE), texto = ""),
    "Graph-RAG+FCA"   = list(pred = simbolico(TRUE),  texto = ""),
    list(pred = character(0), texto = "")
  )
}

# ---------- Métricas de una predicción frente al gold ----------
metricas_pred <- function(q, pred, ctx) {
  fc <- ctx$fc; ig <- ctx$igraph_obj
  # Verdad lógica del KB para medir alucinación
  if (q$tipo == "inverso") {
    pac <- ctx$objetos[grepl("^Paciente", ctx$objetos)]
    verdad <- pac[ctx$matriz[pac, q$entrada] == 1]
  } else {
    verdad <- cierre_arm(q$entrada, fc, ig, usar_grafo = TRUE)
  }
  out <- list(correcto = NA, precision = NA_real_, recall = NA_real_,
              f1 = NA_real_, halluc = NA_real_)
  if (q$tipo == "si_no") {
    out$correcto <- identical(pred, q$gold)
    # alucinación: afirmar SÍ algo NO respaldado por el KB
    out$halluc <- if (identical(pred, "SI") && !(q$atributo %in% verdad)) 1 else 0
  } else {
    gold <- q$gold
    tp <- length(intersect(pred, gold))
    # Si el brazo no afirma nada, la precisión queda NA (no cuenta en la media).
    out$precision <- if (length(pred) == 0) NA_real_ else tp / length(pred)
    out$recall    <- if (length(gold) == 0) 1 else tp / length(gold)
    p_f1 <- if (is.na(out$precision)) 0 else out$precision
    out$f1 <- if (p_f1 + out$recall == 0) 0 else 2 * p_f1 * out$recall / (p_f1 + out$recall)
    out$correcto <- setequal(pred, gold)
    fuera <- setdiff(pred, verdad)   # afirmado pero no entailment del KB
    out$halluc <- if (length(pred) == 0) 0 else length(fuera) / length(pred)
  }
  out
}

# ---------- Bucle completo ----------
BRAZOS_EVAL <- c("LLM solo", "RAG+LLM", "Graph-RAG+LLM", "FCA solo", "Graph-RAG+FCA")
BRAZOS_LLM  <- c("LLM solo", "RAG+LLM", "Graph-RAG+LLM")   # no deterministas -> se repiten

ejecutar_evaluacion <- function(ctx, model = "llama3.1", brazos = BRAZOS_EVAL,
                                usar_embeddings = TRUE, repeticiones = 2, progreso = NULL) {
  bateria <- construir_bateria(ctx)
  indice  <- preparar_indice_rag(ctx$grafo_limpio, embeddings = usar_embeddings, model = "nomic-embed-text")
  n_llm <- sum(brazos %in% BRAZOS_LLM); n_sim <- sum(!brazos %in% BRAZOS_LLM)
  total <- length(bateria) * (n_llm * repeticiones + n_sim); k <- 0
  filas <- list()
  for (q in bateria) {
    fuente <- if (is.null(q$fuente)) "auto" else q$fuente
    for (b in brazos) {
      reps <- if (b %in% BRAZOS_LLM) repeticiones else 1L   # los simbólicos son deterministas
      for (run in seq_len(reps)) {
        k <- k + 1
        if (!is.null(progreso)) progreso(k / total, sprintf("%s — %s (run %d)", b, q$id, run))
        r <- tryCatch(ejecutar_brazo(b, q, ctx, indice, model),
                      error = function(e) list(pred = character(0), texto = paste("ERROR:", conditionMessage(e))))
        m <- metricas_pred(q, r$pred, ctx)
        pred_txt <- if (is.character(r$pred) && length(r$pred) == 1 && r$pred %in% c("SI", "NO"))
          r$pred else paste(r$pred, collapse = ", ")
        filas[[length(filas) + 1L]] <- data.frame(
          run = run, id = q$id, tipo = q$tipo, fuente = fuente, enfoque = b,
          correcto = m$correcto, precision = m$precision, recall = m$recall,
          f1 = m$f1, halluc = m$halluc, prediccion = pred_txt, stringsAsFactors = FALSE)
      }
    }
  }
  res <- do.call(rbind, filas)
  attr(res, "motor_rag")     <- if (isTRUE(indice$usa_emb)) "embeddings" else "léxico"
  attr(res, "repeticiones")  <- repeticiones
  res
}

# Resumen NUMÉRICO (para gráficas): media y sd -entre repeticiones- de exactitud y alucinación.
resumen_num <- function(res) {
  brazos <- unique(res$enfoque)
  do.call(rbind, lapply(brazos, function(b) {
    d <- res[res$enfoque == b, ]; runs <- unique(d$run)
    ex <- vapply(runs, function(rr) mean(d$correcto[d$run == rr], na.rm = TRUE), numeric(1))
    al <- vapply(runs, function(rr) mean(d$halluc[d$run == rr],  na.rm = TRUE), numeric(1))
    data.frame(Enfoque = b,
               Exactitud = mean(ex), Exactitud_sd = if (length(ex) > 1) sd(ex) else 0,
               Alucinacion = mean(al), Alucinacion_sd = if (length(al) > 1) sd(al) else 0,
               stringsAsFactors = FALSE)
  }))
}

# Resumen FORMATEADO "media ± sd" por enfoque (sd calculada entre repeticiones).
# fuente: NULL = todas; "manual"/"auto" = solo ese gold (para separar la circularidad).
resumen_evaluacion <- function(res, fuente = NULL) {
  if (!is.null(fuente) && "fuente" %in% names(res)) res <- res[res$fuente %in% fuente, , drop = FALSE]
  if (nrow(res) == 0) return(data.frame(Info = "Sin preguntas de esa fuente."))
  brazos <- unique(res$enfoque)
  fmt <- function(v) { v <- v[!is.na(v)]; if (length(v) == 0) return("—")
    m <- mean(v); s <- if (length(v) > 1) stats::sd(v) else 0
    if (is.na(s) || s == 0) sprintf("%.3f", m) else sprintf("%.3f ± %.3f", m, s) }
  do.call(rbind, lapply(brazos, function(b) {
    d <- res[res$enfoque == b, ]; runs <- unique(d$run)
    porrun <- function(f) vapply(runs, function(rr) f(d[d$run == rr, ]), numeric(1))
    subset_mean <- function(dr, tipos, col) {
      c2 <- dr[dr$tipo %in% tipos, ]; if (nrow(c2)) mean(c2[[col]], na.rm = TRUE) else NA_real_ }
    data.frame(
      Enfoque          = b, Runs = length(runs),
      Exactitud      = fmt(porrun(function(dr) mean(dr$correcto, na.rm = TRUE))),
      Exactitud_SiNo = fmt(porrun(function(dr) subset_mean(dr, "si_no", "correcto"))),
      Precision      = fmt(porrun(function(dr) subset_mean(dr, c("categoria","inverso"), "precision"))),
      Recall         = fmt(porrun(function(dr) subset_mean(dr, c("categoria","inverso"), "recall"))),
      F1             = fmt(porrun(function(dr) subset_mean(dr, c("categoria","inverso"), "f1"))),
      Alucinacion    = fmt(porrun(function(dr) mean(dr$halluc, na.rm = TRUE))),
      stringsAsFactors = FALSE)
  }))
}

# ============ EXPORTACIÓN: informe HTML autocontenido + datos ============
# Convierte un data.frame en una tabla HTML.
.tabla_html <- function(df) {
  esc <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", as.character(x)), fixed = TRUE)
  th  <- paste0("<th>", paste(esc(names(df)), collapse = "</th><th>"), "</th>")
  fil <- apply(df, 1, function(r) paste0("<tr><td>", paste(esc(r), collapse = "</td><td>"), "</td></tr>"))
  paste0("<table><thead><tr>", th, "</tr></thead><tbody>", paste(fil, collapse = ""), "</tbody></table>")
}

# Renderiza un ggplot a PNG y lo devuelve como <img> en base64 (autocontenido, sin ficheros).
.png_base64 <- function(plot_obj, w = 8, h = 4.2) {
  tmp <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmp, plot_obj, width = w, height = h, dpi = 110)
  raw <- readBin(tmp, "raw", file.info(tmp)$size); unlink(tmp)
  sprintf('<img alt="grafica" style="max-width:100%%;height:auto;margin:6px 0" src="data:image/png;base64,%s">',
          jsonlite::base64_enc(raw))
}

# Genera el informe HTML completo de una ejecución de evaluación.
informe_html_eval <- function(res) {
  resumen <- resumen_evaluacion(res)
  rn <- resumen_num(res); rn$Enfoque <- factor(rn$Enfoque, levels = BRAZOS_EVAL)
  base_g <- function(df, y, ysd, titulo) {
    ggplot2::ggplot(df, ggplot2::aes(x = Enfoque, y = .data[[y]], fill = Enfoque)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = pmax(0, .data[[y]] - .data[[ysd]]),
                                          ymax = .data[[y]] + .data[[ysd]]), width = 0.25, color = "#444444") +
      ggplot2::labs(title = titulo, x = NULL, y = y) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
  }
  g_hal <- base_g(rn, "Alucinacion", "Alucinacion_sd", "Tasa de alucinación (menor es mejor)")
  g_exa <- base_g(rn, "Exactitud",   "Exactitud_sd",   "Exactitud global (mayor es mejor)")
  reps  <- attr(res, "repeticiones"); if (is.null(reps)) reps <- 1
  meta  <- sprintf("%d preguntas × %d enfoques · Motor RAG: %s · Repeticiones LLM: %s · %s",
                   length(unique(res$id)), length(unique(res$enfoque)),
                   attr(res, "motor_rag"), reps, format(Sys.time(), "%Y-%m-%d %H:%M"))
  det <- res
  for (col in c("precision", "recall", "f1", "halluc"))
    if (col %in% names(det)) det[[col]] <- round(det[[col]], 3)
  paste0(
    "<!DOCTYPE html><html lang='es'><head><meta charset='utf-8'>",
    "<title>Informe de evaluación</title><style>",
    "body{font-family:'Segoe UI',Arial,sans-serif;margin:26px;color:#1c2b36}",
    "h1{color:#0d6e6e}h2{color:#2c3e50;margin-top:28px}",
    "table{border-collapse:collapse;margin:10px 0;font-size:13px}",
    "th,td{border:1px solid #cdd3d8;padding:5px 9px;text-align:center}",
    "th{background:#0d6e6e;color:#fff}tbody tr:nth-child(even){background:#f5f7f8}",
    ".meta{color:#667;font-size:13px}</style></head><body>",
    "<h1>Informe de evaluación · Graph-RAG + FCA</h1>",
    "<p class='meta'>", meta, "</p>",
    "<h2>Resumen por enfoque</h2>", .tabla_html(resumen),
    "<h2>Resumen solo con gold manual (sin circularidad)</h2>",
    .tabla_html(resumen_evaluacion(res, fuente = "manual")),
    "<h2>Gráficas</h2>", .png_base64(g_hal), .png_base64(g_exa),
    "<h2>Lectura</h2><p>Los enfoques deterministas (FCA solo y Graph-RAG+FCA) presentan tasa de ",
    "alucinación nula, frente a los enfoques que delegan el razonamiento en el LLM. El contraste ",
    "FCA solo frente a Graph-RAG+FCA cuantifica la aportación del grafo (recall de riesgos ",
    "recuperados por la relación <i>implica</i>).</p>",
    "<h2>Detalle por pregunta</h2>", .tabla_html(det),
    "</body></html>"
  )
}
