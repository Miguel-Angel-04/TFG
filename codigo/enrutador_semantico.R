# Enrutador (Sistema 1 ligero): interpreta la consulta en lenguaje natural.

# Minúsculas + sin acentos + '_' -> ' ' (para emparejar mejor).
norm <- function(x) tolower(iconv(gsub("_", " ", x), to = "ASCII//TRANSLIT"))

# Conectores que se cuelan entre las palabras de un atributo; se quitan al emparejar.
CONECTORES_MATCH <- c("a", "al", "la", "el", "los", "las", "de", "del", "con",
                      "en", "y", "o", "un", "una", "unos", "unas", "por", "para")
norm_match <- function(x) {
  t <- norm(x)
  t <- gsub(paste0("\\b(", paste(CONECTORES_MATCH, collapse = "|"), ")\\b"), " ", t)
  gsub("\\s+", " ", trimws(t))
}
# Detecta preguntas del tipo "¿qué pacientes... tienen X?" (piden una lista).
es_filtro_inverso <- function(pregunta) {
  grepl("\\bcuales\\b|\\bquienes\\b|que\\s+(pacientes|farmacos|sujetos|objetos|entidades|protocolos|casos)\\b",
        norm(pregunta))
}
# Escapa metacaracteres de regex (para nombres arbitrarios).
escapar_regex <- function(x) gsub("([][{}().^$*+?\\\\|])", "\\\\\\1", x)

# Detecta qué atributos del universo aparecen en el texto, sin sobre-detectar.
detecta_attrs <- function(txt, universo) {
  txt_n <- norm_match(txt)   # quita conectores
  palabras_txt <- unlist(strsplit(txt_n, "\\s+"))
  # Candidatos: subcadena exacta, por palabras, o fuzzy (solo nombres largos).
  candidatos <- universo[sapply(universo, function(a) {
    a_n <- norm(a)
    # Debe empezar en frontera de palabra (evita 'asma' dentro de 'fantasma');
    # el final queda libre para admitir plurales.
    if (grepl(paste0("\\b", escapar_regex(a_n)), txt_n)) return(TRUE)
    pal_a <- unlist(strsplit(a_n, "\\s+"))
    pal_a <- pal_a[nchar(pal_a) >= 4]                                 # palabras significativas
    if (length(pal_a) >= 2 &&
        all(vapply(pal_a, function(p) any(grepl(p, palabras_txt, fixed = TRUE)), logical(1)))) {
      return(TRUE)                                                    # todas las palabras presentes
    }
    if (nchar(a_n) >= 8) {
      # Fuzzy por ventana de palabras: tolera erratas sin cruzar atributos parecidos.
      pal_a_all <- unlist(strsplit(a_n, "\\s+"))
      na <- length(pal_a_all)
      if (length(palabras_txt) >= na) {
        d <- max(1L, min(2L, floor(nchar(a_n) * 0.12)))
        for (i in seq_len(length(palabras_txt) - na + 1L)) {
          win <- paste(palabras_txt[i:(i + na - 1L)], collapse = " ")
          if (isTRUE(adist(a_n, win)[1, 1] <= d)) return(TRUE)
        }
      }
      return(FALSE)
    }
    FALSE
  })]
  if (length(candidatos) <= 1) return(candidatos)
  # Descarta atributos que son subcadena de otro (se queda con el más específico).
  cand_n <- norm(candidatos)
  mantener <- vapply(seq_along(candidatos), function(i) {
    !any(cand_n != cand_n[i] & grepl(cand_n[i], cand_n, fixed = TRUE))
  }, logical(1))
  candidatos[mantener]
}

# Detecta los sujetos mencionados (con variantes numéricas), sin fuzzy.
detectar_sujetos <- function(pregunta, objetos) {
  pregunta_norm <- norm(pregunta)
  pregunta_sep  <- gsub("([a-z])(\\d)", "\\1 \\2", pregunta_norm)  # "paciente9" -> "paciente 9"

  objetos[sapply(objetos, function(s) {
    s_n <- norm(s)
    s_sin_cero <- gsub("\\b0+(\\d)", "\\1", s_n)                   # "paciente 09" -> "paciente 9"
    for (q_var in unique(c(pregunta_norm, pregunta_sep))) {
      for (s_var in unique(c(s_n, s_sin_cero))) {
        if (grepl(paste0("\\b", escapar_regex(s_var), "\\b"), q_var)) return(TRUE)
      }
    }
    # Sin fuzzy: pacientes con nombres casi iguales se confundirían entre sí.
    FALSE
  })]
}

# Separa la premisa ("con X, Y") de la pregunta ("¿sería Z?").
separar_premisa_consulta <- function(pregunta) {
  partes <- strsplit(pregunta, "¿", fixed = TRUE)[[1]]
  if (length(partes) >= 2) {
    list(premisa = partes[1], consulta = paste(partes[-1], collapse = " "))
  } else {
    list(premisa = pregunta, consulta = pregunta)
  }
}

# --- Detectores de intención ---
es_faltantes <- function(pregunta) {
  grepl("falta|faltan|carec|le queda|no tien|no pose|no tenga|no cuenta con", norm(pregunta))
}

es_hipotetico <- function(pregunta) {
  patron_hip <- paste(
    "imagina", "supongamos", "supon", "asumiend", "hipot", "se convertir",
    "nuevo paciente", "nuevo caso", "nuevo objeto",
    "si tengo", "si tuvier", "si fuer", "si adem", "ademas tuvi", "tambien tuvier",
    "si un paciente", "un paciente que", "un paciente con", "un caso con",
    "si presenta", "si presentar", "presentara", "si le anad",
    sep = "|"
  )
  grepl(patron_hip, norm(pregunta))
}

es_negativo <- function(pregunta) {
  grepl("\\bno\\b|\\bsin\\b|ningun", norm(pregunta))
}

# Devuelve el/los tipos de relación que pide una pregunta de listar (o vacío si es genérica).
relacion_consultada <- function(pregunta) {
  p <- norm(pregunta)
  if (grepl("sintoma|signo", p))                       return(c("tiene_sintoma", "requiere_sintoma"))
  if (grepl("diagnostic|\\benfermedad", p))            return("diagnosticado_con")
  if (grepl("padec|sufre|comorbilidad|condicion", p))  return(c("tiene_condicion", "diagnosticado_con", "requiere_condicion"))
  if (grepl("prueba|analitic|\\btest\\b|radiograf|hemocultivo|ecograf|gasometr|electrocardiog|\\becg\\b|\\btac\\b|resonancia", p))
                                                       return("requiere_prueba")
  if (grepl("monitoriz|vigila", p))                    return("requiere_monitorizacion")
  if (grepl("contraindicad|contraindicacion", p))      return("contraindicado_con")
  # 'incluye' antes que 'pertenece_a': "qué incluye el grupo X" lleva ambas palabras.
  if (grepl("incluye|contiene|componen", p))           return("incluye")
  if (grepl("pertenece|\\bgrupo\\b|\\bclase\\b|familia", p)) return("pertenece_a")
  if (grepl("alternativ", p))                          return(c("es_alternativa_a", "alternativa_tratamiento"))
  if (grepl("implica|conlleva|riesgo|consecuencia", p)) return("implica")
  if (grepl("tratamiento|farmac|medicaci|medicament|toma|recibe|administr", p)) return("recibe_tratamiento")
  if (grepl("\\btrata\\b|indicad|indicacion|sirve para", p)) return("trata")
  character(0)
}

# Categoría clínica de cada relación (para agrupar el listado).
CATEGORIA_RELACION <- c(
  tiene_sintoma           = "Síntomas/signos",
  tiene_condicion         = "Condiciones",
  diagnosticado_con       = "Diagnósticos",
  recibe_tratamiento      = "Tratamiento",
  requiere_prueba         = "Pruebas",
  requiere_monitorizacion = "Monitorización",
  pertenece_a             = "Grupo farmacológico",
  incluye                 = "Incluye",
  trata                   = "Indicaciones",
  contraindicado_con      = "Contraindicaciones",
  implica                 = "Riesgos/consecuencias",
  requiere_sintoma        = "Síntomas requeridos",
  requiere_condicion      = "Condiciones requeridas",
  es_alternativa_a        = "Alternativas",
  alternativa_tratamiento = "Alternativas"
)
ORDEN_CATEGORIAS <- c("Síntomas/signos", "Condiciones", "Diagnósticos", "Tratamiento", "Pruebas",
                      "Monitorización", "Grupo farmacológico", "Indicaciones", "Contraindicaciones",
                      "Riesgos/consecuencias", "Incluye", "Síntomas requeridos", "Condiciones requeridas",
                      "Alternativas", "Otros (deducidos)")

etiqueta_relacion <- function(rels) {
  cats <- unique(unname(CATEGORIA_RELACION[rels]))
  cats <- cats[!is.na(cats)]
  if (length(cats) == 0) return("Atributos")
  paste(cats, collapse = "/")
}

# Agrupa los atributos del cierre por categoría, usando las aristas del nodo.
agrupar_por_categoria <- function(cierre, ars) {
  cat_de <- setNames(rep("Otros (deducidos)", length(cierre)), cierre)
  if (nrow(ars) > 0) {
    for (i in seq_len(nrow(ars))) {
      a <- ars$attr[i]
      if (a %in% cierre) {
        cc <- unname(CATEGORIA_RELACION[ars$rel[i]])
        if (!is.na(cc)) cat_de[a] <- cc
      }
    }
  }
  grupos <- split(names(cat_de), unname(cat_de))
  orden  <- ORDEN_CATEGORIAS[ORDEN_CATEGORIAS %in% names(grupos)]
  grupos[orden]
}

# Traza agrupada por categoría (en vez del cierre plano).
traza_agrupada <- function(obj, attrs, ars) {
  grp <- agrupar_por_categoria(attrs, ars)
  if (length(grp) == 0) {
    return(paste0("**Objeto:** ", obj, "\n\n**Atributos:** ",
                  gsub("_", " ", paste(attrs, collapse = ", "))))
  }
  bloques <- vapply(names(grp), function(g)
    paste0("**", g, ":** ", gsub("_", " ", paste(grp[[g]], collapse = ", "))), character(1))
  paste0("**Objeto:** ", obj, "\n\n", paste(bloques, collapse = "\n\n"))
}

# Clasifica sujetos por tipo (Pacientes / Protocolos / Entidades).
clasifica_sujeto <- function(ids) {
  ifelse(grepl("^Paciente",  ids, ignore.case = TRUE), "Pacientes",
  ifelse(grepl("^Protocolo", ids, ignore.case = TRUE), "Protocolos", "Entidades"))
}

# ¿La pregunta inversa pide un tipo concreto? (pacientes, protocolos, entidades).
tipo_inverso_pedido <- function(pregunta) {
  p <- norm(pregunta)
  if (grepl("\\bpaciente",  p)) return("Pacientes")
  if (grepl("\\bprotocolo", p)) return("Protocolos")
  if (grepl("farmac|medicament|medicaci|\\bentidad|sustancia", p)) return("Entidades")
  NULL
}

# Frase del filtro inverso por tipo (respaldo determinista).
prosa_inverso <- function(sujetos, atr_txt, negativo) {
  if (length(sujetos) == 0)
    return(paste0(if (negativo) "Todos tienen " else "Ninguno tiene ", atr_txt, "."))
  verbo <- if (negativo) "no tienen" else "tienen"
  etiq  <- c(Pacientes = "Los pacientes", Protocolos = "Los protocolos", Entidades = "Las entidades")
  g <- split(sujetos, factor(clasifica_sujeto(sujetos),
                             levels = c("Pacientes", "Protocolos", "Entidades")))
  g <- g[vapply(g, length, integer(1)) > 0]
  frases <- vapply(names(g), function(k)
    paste0(etiq[[k]], " que ", verbo, " ", atr_txt, " son ",
           gsub("_", " ", paste(g[[k]], collapse = ", ")), "."), character(1))
  paste(frases, collapse = " ")
}

# Agrupa una lista de sujetos por tipo, como texto.
agrupar_sujetos <- function(ids, markdown = FALSE) {
  if (length(ids) == 0) return("ninguno")
  tipo  <- clasifica_sujeto(ids)
  g <- split(ids, factor(tipo, levels = c("Pacientes", "Protocolos", "Entidades")))
  g <- g[vapply(g, length, integer(1)) > 0]
  paste(vapply(names(g), function(k) {
    items <- gsub("_", " ", paste(g[[k]], collapse = ", "))
    if (markdown) paste0("**", k, ":** ", items) else paste0(k, ": ", items)
  }, character(1)), collapse = if (markdown) "\n\n" else "; ")
}
