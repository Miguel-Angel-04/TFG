# Orquestador híbrido: interpreta la consulta, aplica FCA + Graph-RAG y arma la respuesta.
resolver_consulta <- function(pregunta, matriz, fc, universo, objetos, ig = NULL, mapear_fn = NULL) {
  
  sp <- separar_premisa_consulta(pregunta)
  atributos_premisa     <- detecta_attrs(sp$premisa,  universo)
  atributos_consultados <- detecta_attrs(sp$consulta, universo)
  
  faltantes           <- es_faltantes(pregunta)
  objetos_mencionados <- detectar_sujetos(pregunta, objetos)
  hipotetico          <- es_hipotetico(pregunta)

  # (1) Si un sujeto está contenido en otro más largo, se queda el más específico.
  if (length(objetos_mencionados) > 1) {
    pal_s <- lapply(objetos_mencionados, function(s) unlist(strsplit(norm(s), "\\s+")))
    mantener <- vapply(seq_along(objetos_mencionados), function(i)
      !any(vapply(seq_along(objetos_mencionados), function(j)
        i != j && length(pal_s[[i]]) < length(pal_s[[j]]) && all(pal_s[[i]] %in% pal_s[[j]]),
        logical(1))), logical(1))
    objetos_mencionados <- objetos_mencionados[mantener]
  }
  # (2) Quita atributos que son parte del nombre de un sujeto (p.ej. "Sepsis" en "Protocolo_Sepsis").
  if (length(objetos_mencionados) > 0 && length(atributos_consultados) > 0) {
    pal_s <- lapply(objetos_mencionados, function(s) unlist(strsplit(norm(s), "\\s+")))
    es_subparte <- function(a) {
      pa <- unlist(strsplit(norm(a), "\\s+"))
      length(pa) > 0 && any(vapply(pal_s, function(ps)
        length(ps) > length(pa) && all(pa %in% ps), logical(1)))
    }
    atributos_consultados <- atributos_consultados[!vapply(atributos_consultados, es_subparte, logical(1))]
  }

  # (2b) Quita el sujeto que en realidad es parte del atributo (p.ej. "Betalactamicos" en "Contraindicacion_Betalactamicos").
  if (length(objetos_mencionados) > 0 && length(atributos_consultados) > 0) {
    pal_a <- lapply(atributos_consultados, function(a) unlist(strsplit(norm(a), "\\s+")))
    es_subparte_de_attr <- function(s) {
      ps <- unlist(strsplit(norm(s), "\\s+"))
      length(ps) > 0 && any(vapply(pal_a, function(pa)
        length(pa) > length(ps) && all(ps %in% pa), logical(1)))
    }
    objetos_mencionados <- objetos_mencionados[!vapply(objetos_mencionados, es_subparte_de_attr, logical(1))]
  }

  # (3) Si no se detectó sujeto, el LLM lo mapea desde la lista (respaldo NLU).
  if (length(objetos_mencionados) == 0 && !hipotetico &&
      !es_filtro_inverso(pregunta) && !is.null(mapear_fn)) {
    cand <- tryCatch(mapear_fn(pregunta), error = function(e) NULL)
    if (!is.null(cand) && length(cand) == 1 && cand %in% objetos) {
      objetos_mencionados <- cand
      ps <- unlist(strsplit(norm(objetos_mencionados), "\\s+"))
      atributos_consultados <- atributos_consultados[!vapply(atributos_consultados, function(a) {
        pa <- unlist(strsplit(norm(a), "\\s+"))
        length(pa) > 0 && length(ps) > length(pa) && all(pa %in% ps)
      }, logical(1))]
    }
  }

  obj_sin_attr <- setdiff(objetos_mencionados, atributos_consultados)
  if (length(obj_sin_attr) > 0) {
    objetos_mencionados <- obj_sin_attr
  } else if (length(objetos_mencionados) > 0) {
    if (es_filtro_inverso(pregunta)) {
      objetos_mencionados <- character(0)
    } else {
      preg_n <- norm(pregunta)
      pos <- vapply(objetos_mencionados, function(s) {
        p <- regexpr(paste0("\\b", escapar_regex(norm(s)), "\\b"), preg_n)
        if (p[1] < 0) Inf else as.double(p[1])
      }, double(1))
      primero <- objetos_mencionados[which.min(pos)]
      objetos_mencionados   <- primero
      atributos_consultados <- setdiff(atributos_consultados, primero)
    }
  }

  traza_texto         <- character(0)
  veredicto_texto     <- character(0)
  nodos_highlight_ids <- character(0)
  sujetos_out  <- character(0)
  destacar_out <- character(0)
  tipo_out     <- "ninguno"
  modo_ref     <- "ninguno"
  hechos_ref   <- character(0)
  sujeto_ref   <- ""
  grupos_ref   <- NULL
  respaldo_ref <- ""   # respuesta determinista completa (filtro inverso)
  respuesta_hip  <- ""            # frase determinista limpia para el modo hipotético
  prohibidos_hip <- character(0)  # atributos de la premisa que NO deben reaparecer
  rag_aristas_out <- data.frame(from = character(0), to = character(0), rel = character(0),
                                stringsAsFactors = FALSE)
  
  if (hipotetico) {
    # La base se siembra solo desde un sujeto contenedor real (Paciente/Protocolo);
    # una entidad doble-rol ("Diabetes_Tipo2") es atributo consultado, no semilla.
    semillas        <- objetos_mencionados[!objetos_mencionados %in% universo]
    dobles_rol      <- objetos_mencionados[objetos_mencionados %in% universo]
    consultados_hip <- setdiff(union(atributos_consultados, dobles_rol), atributos_premisa)
    base_hip <- atributos_premisa
    if (length(semillas) > 0) {
      fila <- matriz[semillas[1], ]
      base_hip <- union(names(fila)[fila == 1], atributos_premisa)
    }
    # PASO RAG: recupera del grafo lo que la base IMPLICA (aristas entidad→entidad).
    exp <- if (!is.null(ig)) expandir_con_grafo(base_hip, ig, pasos = 1)
    else list(nodos = character(0), traza = character(0),
              aristas = data.frame(from = character(0), to = character(0), rel = character(0)))
    rag_aristas_out <- exp$aristas
    base_ext <- union(base_hip, exp$nodos)
    resh <- cierre_logico(base_ext, fc)
    resh$cierre <- union(resh$cierre, exp$nodos)   # garantiza los nodos del grafo
    nuevos_deducidos <- setdiff(resh$cierre, base_hip)
    reglas_txt <- if (length(resh$traza) > 0) paste(paste0("- ", resh$traza), collapse = "\n")
    else "- Ninguna regla añade atributos nuevos."
    rag_txt    <- if (length(exp$traza) > 0) paste(paste0("- ", exp$traza), collapse = "\n")
    else "- (Sin aristas entidad→entidad relevantes en el grafo.)"
    # Resultado deducido AGRUPADO por categoría (clasificando por relación entrante).
    grp_hip <- agrupar_por_categoria(resh$cierre, aristas_categoria(resh$cierre, ig))
    bloques_hip <- if (length(grp_hip) > 0)
      paste(vapply(names(grp_hip), function(g)
        paste0("**", g, ":** ", gsub("_", " ", paste(grp_hip[[g]], collapse = ", "))),
        character(1)), collapse = "\n\n")
    else gsub("_", " ", paste(resh$cierre, collapse = ", "))
    traza_texto <- paste0(
      "**Conjunto de partida:** [ ", gsub("_", " ", paste(base_hip, collapse = ", ")), " ]\n\n",
      "**Recuperación Graph-RAG (aristas del grafo):**\n\n", rag_txt, "\n\n",
      "**Reglas FCA aplicadas:**\n\n", reglas_txt, "\n\n",
      "**Resultado deducido (por categoría):**\n\n", bloques_hip
    )
    consultados_reales <- consultados_hip
    # Si la pregunta pide una categoría (riesgos, síntomas...), responde solo esa; si no, todo.
    rels_pedidas <- relacion_consultada(pregunta)
    cats_pedidas <- unique(unname(CATEGORIA_RELACION[rels_pedidas]))
    cats_pedidas <- cats_pedidas[!is.na(cats_pedidas)]
    bonito <- function(x) gsub("_", " ", paste(x, collapse = ", "))
    prohibidos_hip <- base_hip           # lo de la premisa NO debe reaparecer en la resolución
    if (length(consultados_reales) > 0) {
      veredicto_texto <- vapply(consultados_reales, function(a) {
        paste0("- ", a, ": ", if (a %in% resh$cierre) "SÍ" else "NO")
      }, character(1))
      respuesta_hip <- paste(vapply(consultados_reales, function(a)
        paste0("El paciente ", if (a %in% resh$cierre) "tendría " else "no tendría ",
               gsub("_", " ", a), "."), character(1)), collapse = " ")
      prohibidos_hip <- character(0)     # aquí sí puede nombrarse lo consultado
    } else if (length(cats_pedidas) > 0) {
      etiqueta <- paste(cats_pedidas, collapse = " / ")
      items    <- unlist(grp_hip[intersect(names(grp_hip), cats_pedidas)], use.names = FALSE)
      # Solo los NUEVOS de esa categoría: se excluye lo que ya estaba en la premisa.
      items    <- intersect(items[nzchar(items)], nuevos_deducidos)
      if (length(items) > 0) {
        el_cat <- tolower(etiqueta); it_txt <- bonito(items)
        respuesta_hip   <- if (grepl("tratamiento", el_cat))
            paste0("El paciente recibiría tratamiento con ", it_txt, ".")
          else if (grepl("prueba", el_cat))
            paste0("El paciente requeriría ", it_txt, ".")
          else if (grepl("monitor", el_cat))
            paste0("El paciente requeriría monitorización de ", it_txt, ".")
          else
            paste0("El paciente presentaría ", it_txt, " como ", el_cat, ".")
        veredicto_texto <- paste0(respuesta_hip,
          " (Enúncialo en una frase natural, mencionando la categoría, en positivo, sin negaciones, ",
          "sin añadir causas ni las condiciones de partida y sin mencionar otras categorías.)")
      } else {
        respuesta_hip   <- paste0("El paciente no presentaría ningún atributo nuevo en la categoría ",
                                  tolower(etiqueta), ".")
        veredicto_texto <- respuesta_hip
      }
    } else if (length(nuevos_deducidos) > 0) {
      # Estilo agrupado por categoría (mismo que el listado factual), solo con lo NUEVO.
      grp_n <- agrupar_por_categoria(nuevos_deducidos, aristas_categoria(nuevos_deducidos, ig))
      respuesta_hip <- prosa_agrupada("El paciente", grp_n, verbo = "presentaría")
      veredicto_texto <- paste0(respuesta_hip,
        " (Enúncialo en una frase natural agrupando por categoría —síntomas/signos, condiciones, ",
        "diagnósticos, tratamiento, riesgos/consecuencias, etc.—, en positivo y sin negaciones.)")
    } else {
      respuesta_hip   <- "El paciente no presentaría ningún atributo nuevo por deducción."
      veredicto_texto <- respuesta_hip
    }
    nodos_highlight_ids <- unique(c(objetos_mencionados, base_ext))
    sujetos_out  <- objetos_mencionados
    destacar_out <- consultados_reales
    tipo_out     <- "hipotetico"
    modo_ref     <- "hipotetico"
    
  } else if (length(objetos_mencionados) > 0) {
    for (obj in objetos_mencionados) {
      fila       <- matriz[obj, ]
      atr_matriz <- names(fila)[fila == 1]
      res <- cierre_logico(atr_matriz, fc)     # cierre FCA directo (sin grafo)
      exp <- list(nodos = character(0), traza = character(0),
                  aristas = data.frame(from = character(0), to = character(0), rel = character(0)))
      # Graph-RAG solo si se pregunta por un atributo que el FCA no da y el grafo alcanza.
      if (length(atributos_consultados) > 0 && !is.null(ig)) {
        faltan_consult <- setdiff(atributos_consultados, res$cierre)
        if (length(faltan_consult) > 0) {
          e         <- expandir_con_grafo(atr_matriz, ig, pasos = 1)
          rel_nodos <- intersect(e$nodos, faltan_consult)         # solo lo preguntado
          keep      <- e$aristas$to %in% faltan_consult
          if (length(rel_nodos) > 0) {
            res <- cierre_logico(union(atr_matriz, rel_nodos), fc)
            res$cierre <- union(res$cierre, rel_nodos)
            exp <- list(nodos = rel_nodos, traza = e$traza[keep],
                        aristas = e$aristas[keep, , drop = FALSE])
          }
        }
      }
      rag_aristas_out <- rbind(rag_aristas_out, exp$aristas)

      # Aristas del sujeto: para AGRUPAR por categoría en trazabilidad y listados.
      ars_obj <- if (!is.null(ig)) aristas_salientes(obj, ig)
                 else data.frame(attr = character(0), rel = character(0), stringsAsFactors = FALSE)

      # --- LISTADO (modo listar): por relación si la pregunta la nombra; si no, agrupado ---
      lista_hechos    <- res$cierre
      lista_txt_traza <- NULL
      lista_txt_vered <- NULL
      es_listar <- length(atributos_consultados) == 0 && !faltantes
      if (es_listar) {
        rels_pedidas <- relacion_consultada(pregunta)
        ars <- ars_obj
        if (length(rels_pedidas) > 0 && nrow(ars) > 0) {
          # Filtrado por tipo de relación (usa las aristas tipadas del grafo)
          sel        <- intersect(unique(ars$attr[ars$rel %in% rels_pedidas]), res$cierre)
          rels_match <- unique(ars$rel[ars$rel %in% rels_pedidas & ars$attr %in% sel])
          etiqueta   <- etiqueta_relacion(if (length(rels_match)) rels_match else rels_pedidas)
          lista_hechos    <- sel
          sel_txt <- gsub("_", " ", paste(sel, collapse = ", "))
          lista_txt_traza <- paste0("**Objeto:** ", obj, "\n\n**", etiqueta, ":** ",
            if (length(sel) > 0) sel_txt else "ninguno")
          if (length(sel) > 0) grupos_ref <- setNames(list(sel), etiqueta)
          lista_txt_vered <- if (length(sel) > 0)
            paste0(prosa_agrupada(obj, setNames(list(sel), etiqueta)),
                   " Reprodúcelo con naturalidad, sin añadir ni quitar nada.")
          else paste0(obj, " no tiene ningún atributo registrado de tipo ",
                      paste(rels_pedidas, collapse = "/"), ".")
        } else if (nrow(ars) > 0) {
          # Genérico: TODO agrupado por categoría clínica
          grp <- agrupar_por_categoria(res$cierre, ars)
          grupos_ref      <- grp   # para el respaldo en prosa del verificador
          lista_txt_traza <- traza_agrupada(obj, res$cierre, ars)
          lista_txt_vered <- paste0(
            "Describe en PROSA NATURAL y continua lo que tiene ", obj, ", agrupando por categoría ",
            "(p.ej. 'X e Y como síntomas; Z como condición; recibe tratamiento con W'), sin dejarte ninguno. Datos: ",
            paste(vapply(names(grp), function(g)
              paste0(gsub("_", " ", paste(grp[[g]], collapse = ", ")), " (", tolower(g), ")"),
              character(1)), collapse = "; "), ".")
        } else {
          # Sin grafo disponible: lista plana (comportamiento anterior)
          lista_txt_traza <- paste0("**Objeto:** ", obj, "\n\n",
            "**Atributos (cierre lógico):** [ ", paste(res$cierre, collapse = ", "), " ]")
          lista_txt_vered <- paste0(obj, " presenta en su ficha estos ", length(res$cierre),
            " atributos: ", paste(res$cierre, collapse = ", "), ".")
        }
      }

      if (length(atributos_consultados) > 0) {
        rag_line <- if (length(exp$traza) > 0)
          paste0("\n\n**Recuperación Graph-RAG:**\n\n", paste(paste0("- ", exp$traza), collapse = "\n")) else ""
        # Lo recuperado por el grafo ya se muestra en su bloque: no se repite en las categorías.
        cierre_traza <- setdiff(res$cierre, exp$nodos)
        traza_texto  <- c(traza_texto, paste0(traza_agrupada(obj, cierre_traza, ars_obj), rag_line))
      } else if (faltantes) {
        grupos_ref  <- agrupar_por_categoria(res$cierre, ars_obj)
        traza_texto <- c(traza_texto, traza_agrupada(obj, res$cierre, ars_obj))
      } else {
        traza_texto <- c(traza_texto, lista_txt_traza)
      }
      
      if (length(atributos_consultados) > 0) {
        v <- vapply(atributos_consultados, function(a) {
          paste0("- ", obj, " / ", a, ": ", if (a %in% res$cierre) "SÍ" else "NO")
        }, character(1))
        veredicto_texto <- c(veredicto_texto, v)
      } else if (faltantes) {
        posee_todo <- length(res$cierre) >= length(universo)
        veredicto_texto <- c(veredicto_texto, if (!posee_todo) {
          datos <- paste(vapply(names(grupos_ref), function(g)
            paste0(gsub("_", " ", paste(grupos_ref[[g]], collapse = ", ")), " (", tolower(g), ")"),
            character(1)), collapse = "; ")
          paste0("El usuario pregunta qué NO tiene. ", obj, " posee ÚNICAMENTE: ", datos,
                 ". Descríbelo EN POSITIVO y en prosa natural, agrupando por categoría, y añade que ",
                 "no tiene ningún otro atributo. NO nombres atributos ausentes concretos.")
        } else {
          paste0(obj, " no le falta NINGÚN atributo: posee todo el universo.")
        })
      } else {
        veredicto_texto <- c(veredicto_texto, lista_txt_vered)
      }
      nodos_highlight_ids <- unique(c(nodos_highlight_ids, obj, res$cierre))
    }
    traza_texto <- paste(traza_texto, collapse = "\n\n---\n\n")
    sujetos_out  <- objetos_mencionados
    destacar_out <- atributos_consultados
    tipo_out     <- "real"
    modo_ref <- if (length(atributos_consultados) > 0) "hecho" else if (faltantes) "faltantes" else "listar"
    if (length(objetos_mencionados) == 1) { sujeto_ref <- objetos_mencionados[1]; hechos_ref <- lista_hechos }
    
  } else if (length(union(atributos_premisa, atributos_consultados)) > 0) {
    atributos_buscar <- union(atributos_premisa, atributos_consultados)
    negativo <- es_negativo(pregunta)
    positivos <- character(0)
    for (obj in objetos) {
      fila <- matriz[obj, ]
      res <- cierre_logico(names(fila)[fila == 1], fc)
      if (all(atributos_buscar %in% res$cierre)) positivos <- c(positivos, obj)
    }
    # Filtra por el tipo preguntado (pacientes/protocolos/...); si es genérica, todos.
    tipo_ped <- tipo_inverso_pedido(pregunta)
    filtra   <- function(ids) if (is.null(tipo_ped)) ids else ids[clasifica_sujeto(ids) == tipo_ped]
    atr_txt  <- gsub("_", " ", paste(atributos_buscar, collapse = ", "))
    if (negativo) {
      cumplen <- filtra(setdiff(objetos, positivos))
      traza_texto <- paste0(
        "**Consulta NEGADA.** Atributos: [ ", atr_txt, " ]\n\n",
        "**Sí los tienen (se excluyen):** ", agrupar_sujetos(filtra(positivos)), "\n\n",
        "**No los tienen:**\n\n",
        if (length(cumplen) > 0) agrupar_sujetos(cumplen, markdown = TRUE) else "_Ninguno_"
      )
    } else {
      cumplen <- filtra(positivos)
      traza_texto <- paste0(
        "**Atributos buscados:** [ ", atr_txt, " ]\n\n",
        if (length(cumplen) > 0) agrupar_sujetos(cumplen, markdown = TRUE) else "_Ninguno_"
      )
    }
    respaldo_ref    <- prosa_inverso(cumplen, atr_txt, negativo)
    veredicto_texto <- paste0(respaldo_ref,
      " Reprodúcelo con naturalidad en frases continuas (sin viñetas ni guiones), sin dejarte ningún sujeto.")
    nodos_highlight_ids <- unique(c(cumplen, atributos_buscar))
    sujetos_out  <- cumplen
    destacar_out <- atributos_buscar
    tipo_out     <- "inverso"
    modo_ref     <- "inverso"
    
  } else {
    traza_texto     <- "_No se han identificado objetos ni atributos conocidos en la consulta._"
    veredicto_texto <- "Solo puedo responder sobre los objetos y atributos del archivo cargado. Inténtelo de nuevo."
  }
  
  list(
    traza       = traza_texto,
    veredicto   = paste(veredicto_texto, collapse = "\n"),
    nodos       = nodos_highlight_ids,
    sujetos     = sujetos_out,
    destacar    = destacar_out,
    tipo        = tipo_out,
    modo        = modo_ref,
    hechos      = hechos_ref,
    sujeto      = sujeto_ref,
    universo    = universo,
    grupos      = grupos_ref,
    respaldo    = respaldo_ref,
    respuesta_hip = respuesta_hip,
    prohibidos    = prohibidos_hip,
    rag_aristas = rag_aristas_out
  )
}