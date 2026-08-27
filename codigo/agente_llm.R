# Capa LLM: genera la respuesta en 2º plano (mirai) y la verifica.

# NLU de respaldo: si no se detecta el sujeto por texto, el LLM lo elige de la lista.
mapear_sujeto_llm <- function(pregunta, objetos, model = "llama3.1") {
  sys <- paste(
    "Eres un normalizador de consultas. Recibes una PREGUNTA (puede tener erratas,",
    "mayúsculas/minúsculas o espacios raros) y una LISTA de sujetos válidos. Devuelve",
    "EXACTAMENTE el nombre de la lista que corresponde al sujeto de la pregunta. Si",
    "ninguno encaja con claridad, devuelve NINGUNO. Responde SOLO con el nombre, sin",
    "explicaciones ni comillas."
  )
  prompt <- paste0("LISTA:\n", paste(objetos, collapse = ", "),
                   "\n\nPREGUNTA:\n", pregunta,
                   "\n\nNombre EXACTO de la lista (o NINGUNO):")
  r <- tryCatch({
    chat <- chat_ollama(model = model, system_prompt = sys,
                        api_args = list(temperature = 0, keep_alive = -1))
    trimws(chat$chat(prompt, echo = "none"))
  }, error = function(e) "")
  if (r %in% objetos) return(r)
  hit <- objetos[norm(objetos) == norm(r)]   # tolera espacios/acentos/mayúsculas
  if (length(hit)) return(hit[1])
  NULL
}

crear_system_prompt <- function() {
  prompt_list <- list(
    rol = paste(
      "Eres el REDACTOR (Sistema 1) de un sistema neuro-simbólico.",
      "El motor FCA (fcaR) ya ha calculado y MOSTRADO toda la lógica y su trazabilidad.",
      "Tú solo escribes la conclusión final en lenguaje natural."
    ),
    tarea_unica = paste(
      "Recibirás un VEREDICTO ya calculado. Escribe SOLO una o dos frases de respuesta,",
      "coherentes al 100% con ese veredicto. NO generes trazabilidad, ni reglas, ni encabezados."
    ),
    prohibiciones = list(
      "PROHIBIDO recalcular, aplicar reglas o deducir nada.",
      "PROHIBIDO contradecir el VEREDICTO.",
      "PROHIBIDO inventar atributos que no estén en el VEREDICTO.",
      "IGNORA tu conocimiento previo del mundo: solo existe lo que dice el VEREDICTO."
    )
  )
  toJSON(prompt_list, auto_unbox = TRUE, pretty = TRUE)
}

construir_prompt_resolucion <- function(veredicto_final, pregunta) {
  paste0(
    "Esto es una CONSULTA a un dataset (NO es asesoramiento médico ni una decisión clínica).\n",
    "El motor FCA ya ha resuelto la lógica. VEREDICTO calculado (verdad absoluta):\n",
    veredicto_final, "\n\n",
    "PREGUNTA DEL USUARIO:\n", pregunta, "\n\n",
    "Responde SIEMPRE con naturalidad, basándote SOLO en ese resultado. ",
    "NUNCA te niegues: es una simple consulta de datos. Reglas:\n",
    "- FIEL: no añadas ni inventes NADA que no aparezca en el veredicto.\n",
    "- LISTAR: enumera EXACTAMENTE los atributos que indica el veredicto, sin dejarte ninguno ni añadir ",
    "otros. Si el veredicto viene AGRUPADO por categorías (Síntomas, Diagnósticos, Tratamiento…), respeta ",
    "esa agrupación en tu respuesta. Si acota a un tipo (p.ej. solo diagnósticos), lista SOLO esos. ",
    "No copies literalmente instrucciones como 'enuméralos' o 'reprodúcelo'.\n",
    "- CATEGORÍAS: NUNCA cambies un atributo de categoría ni mezcles unas con otras: cada atributo debe ",
    "quedar en la MISMA categoría en la que aparece en el veredicto. Si el veredicto NO incluye una ",
    "categoría (por ejemplo Tratamiento o Monitorización), NO la menciones bajo ningún concepto.\n",
    "- NO-TIENE: si preguntan qué NO tiene, responde en positivo ('solo posee X, Y, Z') y NUNCA ",
    "enumeres ni inventes atributos ausentes.\n",
    "- SÍ/NO: en preguntas de sí/no responde con una FRASE COMPLETA que repita el sujeto y el atributo ",
    "usando el verbo de la pregunta (p.ej. 'El Ibuprofeno pertenece a los AINE'). NUNCA respondas 'Sí'/'No' ",
    "a secas. Para un resultado AFIRMATIVO NO añadas la palabra 'sí' (di simplemente 'tiene X' o 'presenta X', ",
    "nunca 'tiene X sí' ni 'presenta sí X'); usa 'no' SOLO en los negativos. Si se preguntan VARIOS atributos, ",
    "di el resultado de CADA uno (p.ej. 'tiene dificultad respiratoria pero no asma').\n",
    "- MINÚSCULAS: escribe 'no' en minúscula dentro de la frase, nunca en mayúsculas ('NO').\n",
    "- SIN VIÑETAS: responde SIEMPRE en frases continuas separando con comas y 'y'. NUNCA uses listas ",
    "con viñetas, guiones ni un elemento por línea (p.ej. 'Los pacientes que tienen X son A, B y C').\n",
    "- CONCISO: ve al grano (una frase basta), sin repetir la trazabilidad."
  )
}

# Respuesta determinista desde el veredicto (respaldo si el LLM falla, se niega o contradice).
respuesta_fallback <- function(veredicto) {
  lineas <- unlist(strsplit(veredicto, "\n")); frases <- character(0)
  for (l in lineas) {
    l <- trimws(l); if (l == "") next
    val <- if (grepl(":\\s*S[IÍ]$", l)) "SI" else if (grepl(":\\s*NO$", l)) "NO" else NA
    if (!is.na(val)) {
      cuerpo <- gsub("_", " ", trimws(sub("^-\\s*", "", sub(":\\s*(S[IÍ]|NO)$", "", l))))
      p <- strsplit(cuerpo, "\\s*/\\s*")[[1]]
      if (length(p) == 2) frases <- c(frases, paste0(p[1], if (val=="SI") " tiene " else " no tiene ", p[2], "."))
      else                frases <- c(frases, paste0(if (val=="SI") "Sí, " else "No, ", cuerpo, "."))
    } else frases <- c(frases, l)
  }
  if (length(frases) == 0) "(Consulta la trazabilidad para el detalle.)" else paste(frases, collapse = " ")
}

# ¿La respuesta del LLM contradice el veredicto SÍ/NO? Robusto a atributos multi-palabra.
contradice_veredicto <- function(texto, veredicto) {
  txt <- tolower(iconv(texto, to = "ASCII//TRANSLIT"))
  for (l in unlist(strsplit(veredicto, "\n"))) {
    m <- regmatches(l, regexec("/\\s*(.+?)\\s*:\\s*(S[IÍ]|NO)\\s*$", l))[[1]]
    if (length(m) != 3) next
    attr_n <- tolower(iconv(gsub("_", " ", m[2]), to = "ASCII//TRANSLIT"))
    esperado_si <- grepl("^S", m[3])
    palabras <- unlist(strsplit(gsub("[^a-z ]", " ", attr_n), "\\s+"))
    palabras <- palabras[nchar(palabras) >= 4]
    if (length(palabras) == 0) next
    raices <- substr(palabras, 1, pmax(4, nchar(palabras) - 2))
    clausulas <- unlist(strsplit(txt, "[,.;:]| pero | aunque | sin embargo "))
    con <- clausulas[vapply(clausulas, function(cl) {
      all(vapply(raices, function(r) grepl(r, cl, fixed = TRUE), logical(1)))
    }, logical(1))]
    if (length(con) == 0) next
    neg <- any(grepl("\\bno\\b|\\bsin\\b|\\bni\\b|nunca|tampoco|no se menciona|no consta|no figura|no aparece",
                     con))
    if (esperado_si && neg)   return(TRUE)
    if (!esperado_si && !neg) return(TRUE)
  }
  FALSE
}

# Frase continua a partir de la agrupación por categoría (respaldo determinista).
prosa_agrupada <- function(suj, grupos, verbo = "presenta") {
  # Solo los pacientes RECIBEN tratamiento; un protocolo lo contempla.
  es_paciente <- grepl("^Paciente", suj, ignore.case = TRUE)
  # Predicado propio de la categoría (NULL si es atributiva y necesita el verbo genérico).
  predicado <- function(g, it, n) {
    gl <- tolower(iconv(g, to = "ASCII//TRANSLIT"))
    if      (grepl("alternativ", gl))          paste0("cuenta con ", it, " como alternativa de tratamiento")
    else if (grepl("tratamiento", gl))
      paste0(if (es_paciente) "recibe tratamiento con " else "contempla el tratamiento con ", it)
    else if (grepl("prueba", gl))              paste0(if (n > 1) "requiere las pruebas " else "requiere la prueba ", it)
    else if (grepl("monitor", gl))             paste0("requiere monitorización de ", it)
    else if (grepl("requier|requerid", gl) && grepl("sintoma", gl))
      paste0(if (n > 1) "requiere los síntomas " else "requiere el síntoma ", it)
    else if (grepl("requier|requerid", gl) && grepl("condicion", gl))
      paste0(if (n > 1) "requiere las condiciones " else "requiere la condición ", it)
    else if (grepl("contraindicacion", gl))    paste0("se contraindica con ", it)
    else if (grepl("indicacion", gl))          paste0("trata ", it)
    else if (grepl("grupo", gl))               paste0("pertenece a ", it)
    else if (grepl("incluye", gl))             paste0("incluye ", it)
    else if (grepl("riesgo|consecuencia", gl)) paste0("implica ", it)
    else NULL
  }
  atrib <- character(0); pred <- character(0)
  for (g in names(grupos)) {
    it <- gsub("_", " ", paste(grupos[[g]], collapse = ", "))
    p  <- predicado(g, it, length(grupos[[g]]))
    if (is.null(p)) atrib <- c(atrib, paste0(it, " como ", tolower(g)))
    else            pred  <- c(pred, p)
  }
  partes <- c(if (length(atrib) > 0) paste0(verbo, " ", paste(atrib, collapse = "; ")), pred)
  paste0(suj, " ", paste(partes, collapse = "; "), ".")
}

# Verifica la redacción del LLM según el modo; NULL si es válida o un texto de reemplazo.
verificar_redaccion <- function(texto, res, forzar = FALSE) {
  modo   <- if (is.null(res$modo)) "ninguno" else res$modo
  hechos <- res$hechos
  suj    <- if (is.null(res$sujeto)) "" else res$sujeto

  # Nada reconocido: no se delega en el LLM, se avisa de forma determinista (evita que invente).
  if (identical(modo, "ninguno")) {
    return(paste0(
      "No he identificado en la consulta ningún paciente, protocolo, entidad ni atributo de los datos ",
      "cargados, por lo que no puedo responder. Revisa el nombre o consulta la pestaña de Datos para ",
      "ver los elementos disponibles."))
  }
  sinsep <- function(x) gsub("[^a-z0-9]", "", tolower(iconv(x, to = "ASCII//TRANSLIT")))
  txt    <- sinsep(texto)
  # Atributos que NO aparecen en el texto (de más largo a más corto, para no confundir anidados).
  faltan_de <- function(items) {
    it <- items[order(-nchar(sinsep(items)))]
    t <- txt; out <- character(0)
    for (h in it) {
      hn <- sinsep(h)
      if (nzchar(hn) && grepl(hn, t, fixed = TRUE)) t <- sub(hn, "", t, fixed = TRUE)
      else out <- c(out, h)
    }
    out
  }

  # Sí/no: cada atributo consultado debe aparecer; si el LLM se deja alguno -> respaldo.
  if (identical(modo, "hecho")) {
    if (forzar) return(respuesta_fallback(res$veredicto))
    attrs <- character(0)
    for (l in unlist(strsplit(res$veredicto, "\n"))) {
      m <- regmatches(l, regexec("/\\s*(.+?)\\s*:\\s*(S[IÍ]|NO)\\s*$", l))[[1]]
      if (length(m) == 3) attrs <- c(attrs, m[2])
    }
    if (length(attrs) >= 2) {
      falta <- any(vapply(attrs, function(a) {
        pal    <- unlist(strsplit(norm(a), " ")); pal <- pal[nchar(pal) >= 3]
        raices <- sinsep(substr(pal, 1, pmax(4, nchar(pal) - 2)))
        !all(vapply(raices, function(r) grepl(r, txt, fixed = TRUE), logical(1)))
      }, logical(1)))
      if (falta) return(respuesta_fallback(res$veredicto))
    }
    return(NULL)
  }

  # --- INVERSO: deben aparecer TODOS los sujetos; si se deja alguno -> respaldo completo.
  if (identical(modo, "inverso")) {
    subs <- res$sujetos
    if (is.null(subs) || length(subs) == 0) return(NULL)
    if (forzar || length(faltan_de(subs)) > 0) return(res$respaldo)
    return(NULL)
  }

  # Hipotético: se usa SIEMPRE la frase determinista (ya es correcta y natural). El LLM tendía a
  # repetir la categoría por elemento ("X como tratamiento y Y como tratamiento") o a colar la premisa.
  if (identical(modo, "hipotetico")) {
    if (!is.null(res$respuesta_hip) && nzchar(res$respuesta_hip)) return(res$respuesta_hip)
    return(respuesta_fallback(res$veredicto))
  }

  if (is.null(hechos) || length(hechos) == 0 || !nzchar(suj)) return(NULL)
  if (!modo %in% c("listar", "faltantes")) return(NULL)

  if (identical(modo, "faltantes")) {
    alienos   <- setdiff(res$universo, hechos)
    alienos_n <- sinsep(alienos)
    nombrados <- alienos[nchar(alienos_n) >= 5 &
                           vapply(alienos_n, function(a) grepl(a, txt, fixed = TRUE), logical(1))]
    patron_enum <- grepl("no\\s+(tiene|posee|pose|present|cuenta)",
                         tolower(iconv(texto, to = "ASCII//TRANSLIT"))) &&
      grepl(":\\s*\\[?\\s*[A-ZÁÉÍÓÚ]", texto)
    # COMPLETITUD: debe mencionar TODOS los que SÍ posee; si se deja alguno -> respaldo.
    incompleto <- length(faltan_de(hechos)) > 0
    if (forzar || length(nombrados) > 0 || patron_enum || incompleto) {
      if (!is.null(res$grupos) && length(res$grupos) > 0) {
        return(paste0(prosa_agrupada(suj, res$grupos, verbo = "solo posee"),
                      " No tiene ningún otro atributo fuera de esa lista."))
      }
      return(paste0(suj, " posee únicamente ", gsub("_", " ", paste(hechos, collapse = ", ")),
                    ". No tiene ningún otro atributo fuera de esa lista."))
    }
    return(NULL)
  }

  # ¿El LLM menciona una categoría que no está en el veredicto? (p.ej. "recibe tratamiento" sin Tratamiento)
  cats_inventadas <- function() {
    if (is.null(res$grupos) || length(res$grupos) == 0) return(FALSE)
    t2  <- tolower(iconv(texto, to = "ASCII//TRANSLIT"))
    hay <- function(pat) any(grepl(pat, names(res$grupos), ignore.case = TRUE))
    reglas <- list(
      c("recibe tratamiento|tratamiento con|se le trata con", "Tratamiento"),
      c("requiere monitorizacion|monitorizacion de",          "Monitor"),
      c("como diagnostico",                                   "Diagn"),
      c("como sintoma",                                       "Sintoma|signo"),
      c("como condicion",                                     "Condicion")
    )
    for (r in reglas) if (grepl(r[1], t2) && !hay(r[2])) return(TRUE)
    FALSE
  }

  # El LLM tiende a mezclar o a repetir categorías al redactar listas ("X como tratamiento y
  # Y como tratamiento"): se usa siempre la redacción determinista agrupada, correcta y natural.
  if (!is.null(res$grupos) && length(res$grupos) >= 1) return(prosa_agrupada(suj, res$grupos))

  # Listar: si el LLM se deja algún atributo, copia instrucciones o inventa categoría -> respaldo agrupado.
  faltan   <- faltan_de(hechos)
  eco      <- grepl("enumeralos|exactamente estos atributos|del veredicto",
                    tolower(iconv(texto, to = "ASCII//TRANSLIT")))
  if (forzar || eco || cats_inventadas() || length(faltan) > 0) {
    if (!is.null(res$grupos) && length(res$grupos) > 0) return(prosa_agrupada(suj, res$grupos))
    return(paste0(suj, " presenta ", gsub("_", " ", paste(hechos, collapse = ", ")), "."))
  }
  NULL
}

iniciar_chat <- function(system_prompt, model = "llama3.1") {
  chat_ollama(model = model, system_prompt = system_prompt,
              api_args = list(temperature = 0, keep_alive = -1))
}

precalentar_keepalive <- function(model = "llama3.1") {
  try({
    httr2::request("http://localhost:11434/api/chat") %>%
      httr2::req_body_json(list(
        model = model,
        messages = list(list(role = "user", content = "OK")),
        stream = FALSE, keep_alive = -1
      )) %>%
      httr2::req_timeout(600) %>%
      httr2::req_perform()
  }, silent = TRUE)
}

generar_resolucion_bg <- function(system_prompt, prompt, model = "llama3.1") {
  mirai::mirai(
    {
      library(ellmer)
      chat <- chat_ollama(
        model = model,
        system_prompt = system_prompt,
        api_args = list(temperature = 0, keep_alive = -1)
      )
      chat$chat(prompt, echo = "none")
    },
    system_prompt = system_prompt,
    prompt        = prompt,
    model         = model
  )
}

# Muestra la cabecera, espera al LLM (2º plano) y verifica/corrige la resolución.
respuesta_diferida <- function(cabecera, mirai_obj, res, pausa = 0.03) {
  veredicto <- res$veredicto
  espera <- function(seg) promises::promise(function(resolve, reject) later::later(function() resolve(TRUE), seg))
  tok <- function(x) { t <- unlist(strsplit(x, "(?<=\\s)", perl = TRUE)); t[nchar(t) > 0] }
  es_negativa <- function(txt) grepl("lo siento|no puedo|no estoy|no me es posible|consulta a un|acude a un|profesional",
                                     tolower(txt))
  coro::async_generator(function() {
    for (tk in tok(cabecera)) { yield(tk); await(espera(pausa)) }
    texto <- tryCatch(await(promises::as.promise(mirai_obj)), error = function(e) "")
    duro <- nchar(trimws(texto)) == 0 || es_negativa(texto) || contradice_veredicto(texto, veredicto)
    corr <- verificar_redaccion(texto, res, forzar = duro)
    if (!is.null(corr))      texto <- corr
    else if (duro)           texto <- respuesta_fallback(veredicto)
    texto <- gsub("\\bNO\\b", "no", texto)        # coherencia: 'no' en minúscula
    texto <- gsub("\\bS[IÍ]\\b", "sí", texto)
    # En hechos sí/no, quita el 'sí' sobrante ('tiene sepsis sí' -> 'tiene sepsis').
    if (identical(res$modo, "hecho")) texto <- trimws(gsub("\\s+sí\\b", "", texto))
    for (tk in tok(trimws(texto))) { yield(tk); await(espera(pausa)) }
  })()
}