# Extractor de tripletas (NER+RE): del documento a tripletas Sujeto–Relación–Objeto.

# Tipos de relación médica que se reconocen.
RELACIONES_MEDICAS <- c(
  "tiene_sintoma", "tiene_condicion", "diagnosticado_con",
  "recibe_tratamiento", "pertenece_a", "trata",
  "contraindicado_con", "requiere_sintoma", "requiere_condicion",
  "requiere_prueba", "implica", "requiere_monitorizacion",
  "es_alternativa_a", "incluye", "causa_efecto",
  "alternativa_tratamiento", "alternativa_si"
)

# Palabras de cabecera que el LLM confunde con sujetos; se descartan.
SUJETOS_INVALIDOS <- tolower(c(
  "Contraindicaciones", "Diagnosticos", "Diagnostico", "Sintomas", "Sintoma",
  "Tratamiento", "Tratamientos", "Implicaciones", "Implicacion",
  "Alternativas", "Alternativa", "Pruebas", "Prueba", "Antecedentes",
  "Observaciones", "Conclusiones", "Recomendaciones", "Indicaciones",
  "Nota", "Notas", "Resumen", "Resultados", "Analisis"
))

# Normaliza un nombre: sin acentos, espacios a "_" y capitalizado.
normalizar_entidad <- function(x) {
  x <- trimws(as.character(x))
  x <- iconv(x, to = "ASCII//TRANSLIT")        # sin acentos
  x <- gsub("[^a-zA-Z0-9_\\-]", "_", x)        # solo alfanuméricos y _
  x <- gsub("_+", "_", x)                       # guiones dobles a uno
  x <- gsub("^_|_$", "", x)                     # sin _ al inicio/fin
  partes <- strsplit(x, "_")[[1]]
  partes <- paste0(toupper(substr(partes, 1, 1)), substr(partes, 2, nchar(partes)))
  paste(partes, collapse = "_")
}

# Lee un PDF o TXT y devuelve el texto plano.
leer_documento <- function(path) {
  ext <- tolower(tools::file_ext(path))
  texto <- tryCatch({
    if (ext == "pdf") {
      paginas <- pdftools::pdf_text(path)
      paste(paginas, collapse = "\n")
    } else {
      lineas <- readLines(path, encoding = "UTF-8", warn = FALSE)
      paste(lineas, collapse = "\n")
    }
  }, error = function(e) {
    stop(paste0("No se pudo leer el documento '", basename(path), "': ", conditionMessage(e)))
  })
  # Limpia espacios y saltos de más
  texto <- gsub("[ \t]+", " ", texto)
  texto <- gsub("\n{3,}", "\n\n", texto)
  trimws(texto)
}

# Parte el texto en trozos (chunks) para el LLM, respetando los párrafos.
trocear_texto <- function(texto, max_chars = 1800) {
  parrafos <- strsplit(texto, "\n{2,}")[[1]]
  parrafos <- trimws(parrafos)
  parrafos <- parrafos[nchar(parrafos) >= 30]   # descarta líneas triviales

  chunks       <- character(0)
  chunk_actual <- ""

  for (p in parrafos) {
    if (nchar(chunk_actual) + nchar(p) + 2 > max_chars) {
      if (nchar(trimws(chunk_actual)) > 0) {
        chunks <- c(chunks, trimws(chunk_actual))
      }
      # Si un párrafo es más largo que el máximo, se parte por oraciones
      if (nchar(p) > max_chars) {
        oraciones <- strsplit(p, "(?<=[.!?])\\s+", perl = TRUE)[[1]]
        bloque <- ""
        for (o in oraciones) {
          if (nchar(bloque) + nchar(o) + 1 > max_chars) {
            if (nchar(trimws(bloque)) > 0) chunks <- c(chunks, trimws(bloque))
            bloque <- o
          } else {
            bloque <- paste(bloque, o)
          }
        }
        chunk_actual <- bloque
      } else {
        chunk_actual <- p
      }
    } else {
      chunk_actual <- paste(chunk_actual, p, sep = "\n")
    }
  }
  if (nchar(trimws(chunk_actual)) > 0) chunks <- c(chunks, trimws(chunk_actual))
  chunks
}

# Instrucciones (system prompt) para el LLM extractor.
prompt_sistema_extraccion <- function() {
  paste(
    "Eres un extractor especializado en conocimiento médico-clínico.",
    "Tu única tarea es identificar relaciones médicas en el texto y devolver",
    "un array JSON con ellas. No añadas texto adicional, solo el JSON.\n",
    "TIPOS DE RELACION PERMITIDOS:",
    "  tiene_sintoma         - el paciente manifiesta este síntoma",
    "  tiene_condicion       - el paciente tiene esta comorbilidad o condición previa",
    "  diagnosticado_con     - el paciente ha sido diagnosticado con esta enfermedad",
    "  recibe_tratamiento    - el paciente recibe este fármaco o terapia",
    "  pertenece_a           - el fármaco pertenece a este grupo farmacológico",
    "  trata                 - el fármaco o protocolo está indicado para esta enfermedad",
    "  contraindicado_con    - el fármaco está contraindicado con esta condición",
    "  requiere_sintoma      - el protocolo clínico requiere este síntoma",
    "  requiere_condicion    - el protocolo requiere esta condición previa",
    "  requiere_prueba       - el protocolo requiere esta prueba diagnóstica",
    "  implica               - la condición implica este riesgo o consecuencia",
    "  requiere_monitorizacion - requiere vigilancia de este parámetro analítico",
    "  es_alternativa_a      - el fármaco es alternativa terapéutica al objeto",
    "  incluye               - el grupo farmacológico incluye este fármaco\n",
    "REGLAS CRÍTICAS:",
    "  - El SUJETO debe ser SIEMPRE una entidad médica concreta (paciente, fármaco, enfermedad,",
    "    protocolo). NUNCA uses como sujeto las palabras: Contraindicaciones, Sintomas, Diagnosticos,",
    "    Tratamiento, Implicaciones, Alternativas, Pruebas, Antecedentes u otros encabezados de sección.",
    "  - Si el texto dice 'Los AINE están contraindicados', el sujeto es 'AINE', no 'Contraindicaciones'.",
    "  - Si el texto dice 'Síntomas: Fiebre, Tos', el paciente mencionado antes es el sujeto.",
    "  - Usa guiones bajos en lugar de espacios en los nombres (ej: Insuficiencia_Renal).",
    "  - Capitaliza la primera letra de cada segmento (ej: Fiebre, Tos_Productiva).",
    "  - Devuelve SOLO el array JSON. Si no hay relaciones médicas, devuelve [].",
    "  - Formato exacto: [{\"sujeto\":\"...\",\"relacion\":\"...\",\"objeto\":\"...\"}]",
    sep = "\n"
  )
}

# Prompt de usuario para un chunk.
prompt_usuario_chunk <- function(chunk) {
  paste0(
    "Extrae todas las relaciones médicas del siguiente texto.\n",
    "Devuelve SOLO el array JSON, sin explicaciones ni texto adicional:\n\n",
    chunk
  )
}

# Convierte la respuesta JSON del LLM en un data.frame de tripletas (NULL si falla).
parsear_json_tripletas <- function(respuesta) {
  if (is.null(respuesta) || nchar(trimws(respuesta)) == 0) return(NULL)

  # Quita el markdown (```json ... ```)
  respuesta <- gsub("```json\\s*", "", respuesta, perl = TRUE)
  respuesta <- gsub("```\\s*",     "", respuesta, perl = TRUE)

  # Saca cada objeto {...} por separado (robusto aunque el JSON venga sin cerrar).
  objetos <- regmatches(respuesta, gregexpr("\\{[^{}]*\\}", respuesta, perl = TRUE))[[1]]
  if (length(objetos) == 0) return(NULL)

  filas <- lapply(objetos, function(o) {
    tryCatch({
      x <- jsonlite::fromJSON(o, simplifyVector = TRUE)
      names(x) <- tolower(trimws(names(x)))
      if (!all(c("sujeto", "relacion", "objeto") %in% names(x))) return(NULL)
      data.frame(Sujeto   = as.character(x[["sujeto"]])[1],
                 Relacion = as.character(x[["relacion"]])[1],
                 Objeto   = as.character(x[["objeto"]])[1],
                 stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  })
  df <- do.call(rbind, Filter(Negate(is.null), filas))
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Quita filas con NA o vacías
  df <- df[complete.cases(df) & nchar(trimws(df$Sujeto)) > 0 &
             nchar(trimws(df$Objeto)) > 0, ]
  if (nrow(df) == 0) return(NULL)
  # Normaliza nombres
  df$Sujeto   <- sapply(df$Sujeto,   normalizar_entidad)
  df$Objeto   <- sapply(df$Objeto,   normalizar_entidad)
  df$Relacion <- tolower(trimws(df$Relacion))
  df$Relacion <- gsub(" ", "_", df$Relacion)
  # Descarta relaciones no reconocidas
  invalidas <- !df$Relacion %in% RELACIONES_MEDICAS
  if (any(invalidas)) {
    message(sprintf("[Extractor] %d relaciones no reconocidas omitidas: %s",
                    sum(invalidas), paste(unique(df$Relacion[invalidas]), collapse = ", ")))
    df <- df[!invalidas, ]
  }
  # Descarta sujetos genéricos (cabeceras de sección)
  suj_norm <- tolower(iconv(gsub("_", " ", df$Sujeto), to = "ASCII//TRANSLIT"))
  suj_invalido <- suj_norm %in% SUJETOS_INVALIDOS
  if (any(suj_invalido)) {
    message(sprintf("[Extractor] %d filas con sujeto genérico omitidas: %s",
                    sum(suj_invalido),
                    paste(unique(df$Sujeto[suj_invalido]), collapse = ", ")))
    df <- df[!suj_invalido, ]
  }
  if (nrow(df) == 0) return(NULL)
  df
}

# Extrae las tripletas de un chunk (una llamada al LLM).
extraer_tripletas_chunk <- function(chunk, model = "llama3.1") {
  chat_tmp <- tryCatch(
    chat_ollama(
      model         = model,
      system_prompt = prompt_sistema_extraccion(),
      # num_ctx/num_predict altos para que no corte el JSON en chunks grandes.
      api_args      = list(temperature = 0, keep_alive = -1,
                           num_ctx = 8192, num_predict = 4096)
    ),
    error = function(e) {
      message("[Extractor] No se puede conectar con Ollama: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(chat_tmp)) return(NULL)

  respuesta <- tryCatch(
    chat_tmp$chat(prompt_usuario_chunk(chunk)),
    error = function(e) {
      message("[Extractor] Error en llamada LLM: ", conditionMessage(e))
      NULL
    }
  )
  parsear_json_tripletas(respuesta)
}

# Extrae las tripletas del documento entero. progreso_fn(i, n) actualiza la UI.
extraer_tripletas_doc <- function(path, model = "llama3.1",
                                  max_chars = 1800, progreso_fn = NULL) {
  texto  <- leer_documento(path)
  chunks <- trocear_texto(texto, max_chars = max_chars)
  n      <- length(chunks)

  if (n == 0) stop("El documento está vacío o no contiene texto extraíble.")
  message(sprintf("[Extractor] Documento dividido en %d chunks.", n))

  resultados <- vector("list", n)
  for (i in seq_len(n)) {
    message(sprintf("[Extractor] Procesando chunk %d/%d...", i, n))
    if (!is.null(progreso_fn)) progreso_fn(i, n)
    r <- extraer_tripletas_chunk(chunks[[i]], model = model)
    # Trazabilidad: guarda de qué fragmento sale cada tripleta.
    if (!is.null(r) && nrow(r) > 0) r$Origen <- i
    resultados[[i]] <- r
  }

  # Deduplica manteniendo el orden y juntando los fragmentos de origen.
  df_total <- do.call(rbind, Filter(Negate(is.null), resultados))
  if (is.null(df_total) || nrow(df_total) == 0) {
    stop("No se extrajeron tripletas. Verifica que el documento contiene información médica.")
  }
  clave  <- paste(df_total$Sujeto, df_total$Relacion, df_total$Objeto, sep = "")
  origen <- tapply(df_total$Origen, clave,
                   function(x) paste0("chunk ", paste(sort(unique(x)), collapse = ", ")))
  df_total <- df_total[!duplicated(clave), c("Sujeto", "Relacion", "Objeto")]
  df_total$Origen <- unname(origen[paste(df_total$Sujeto, df_total$Relacion, df_total$Objeto, sep = "")])
  message(sprintf("[Extractor] Total tripletas únicas extraídas: %d", nrow(df_total)))
  df_total
}
