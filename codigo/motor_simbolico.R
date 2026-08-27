# Motor simbólico (Sistema 2): construye el contexto FCA y hace el cierre lógico.

# Construye el contexto formal (matriz + FormalContext + reglas) desde el CSV.
construir_contexto <- function(datapath) {
  grafo_tripletas <- read.csv(datapath, stringsAsFactors = FALSE)

  if (!all(c("Sujeto", "Objeto") %in% colnames(grafo_tripletas))) {
    stop("El CSV debe contener las columnas 'Sujeto' y 'Objeto'.")
  }

  # Conserva la relación si el CSV la trae; si no, pone una genérica.
  if ("Relacion" %in% colnames(grafo_tripletas)) {
    grafo_limpio <- grafo_tripletas %>% select(Sujeto, Relacion, Objeto) %>% distinct()
  } else {
    grafo_limpio <- grafo_tripletas %>% select(Sujeto, Objeto) %>% distinct()
    grafo_limpio$Relacion <- "relacionado_con"
  }

  # La matriz FCA solo usa Sujeto×Objeto (la relación no entra).
  matriz_df <- grafo_limpio %>%
    select(Sujeto, Objeto) %>% distinct() %>%
    mutate(Valor = 1) %>%
    pivot_wider(names_from = Objeto, values_from = Valor, values_fill = list(Valor = 0))

  objetos <- matriz_df$Sujeto
  matriz_incidencia <- as.matrix(matriz_df %>% select(-Sujeto))
  rownames(matriz_incidencia) <- objetos

  fc <- FormalContext$new(matriz_incidencia)
  fc$find_concepts()
  fc$find_implications()

  # Simplifica el contenido de las reglas (sin reducir su número).
  try(
    fc$implications$apply_rules(rules = c("composition", "generalization", "simplification")),
    silent = TRUE
  )

  # Quita las reglas de soporte 0 (premisa que no cumple ningún objeto): son ruido.
  sop <- tryCatch(fc$implications$support(), error = function(e) NULL)
  if (!is.null(sop) && any(sop <= 0)) {
    idx_utiles <- which(sop > 0)
    if (length(idx_utiles) > 0) {
      filtradas <- tryCatch(fc$implications[idx_utiles], error = function(e) NULL)
      if (!is.null(filtradas)) {
        ok <- tryCatch({ fc$implications <- filtradas; TRUE }, error = function(e) FALSE)
        if (ok) message(sprintf("Reglas FCA: %d de %d", length(idx_utiles), length(sop)))
      }
    }
  }

  # Texto de las reglas para el panel lateral.
  lineas_reglas <- capture.output(fc$implications$print())
  texto_unido <- paste(lineas_reglas, collapse = "")
  texto_unido <- gsub("->", "⇒", texto_unido)
  texto_unido <- gsub("\\{", "[ ", texto_unido)
  texto_unido <- gsub("\\}", " ]", texto_unido)
  texto_unido <- gsub("(Rule \\d+:)", "\n\\1", texto_unido)

  list(
    matriz       = matriz_incidencia,
    objetos      = objetos,
    atributos    = colnames(matriz_incidencia),
    fc           = fc,
    reglas_texto = texto_unido,
    grafo_limpio = grafo_limpio,
    igraph_obj   = construir_igraph(grafo_limpio)
  )
}

# Cierre lógico: aplica las implicaciones hasta que no se añade ningún atributo nuevo.
cierre_logico <- function(atributos_iniciales, fc) {
  universo <- fc$attributes
  atributos_iniciales <- intersect(atributos_iniciales, universo)

  lhs <- as.matrix(fc$implications$get_LHS_matrix())
  rhs <- as.matrix(fc$implications$get_RHS_matrix())

  # Por si la matriz viniera transpuesta (los atributos van en filas)
  n_reglas <- fc$implications$cardinality()
  if (ncol(lhs) != n_reglas) { lhs <- t(lhs); rhs <- t(rhs) }

  actual <- atributos_iniciales
  traza  <- character(0)
  cambio <- TRUE
  n_pasada <- 0

  while (cambio) {
    cambio <- FALSE
    n_pasada <- n_pasada + 1
    for (i in seq_len(n_reglas)) {
      ante <- universo[lhs[, i] > 0]
      cons <- universo[rhs[, i] > 0]
      if (length(ante) == 0 || all(ante %in% actual)) {
        nuevos <- setdiff(cons, actual)
        if (length(nuevos) > 0) {
          actual <- union(actual, nuevos)
          traza <- c(traza, sprintf(
            "Pasada %d — Regla %d: [ %s ] ⇒ [ %s ]  ->  añade: %s",
            n_pasada, i,
            paste(ante, collapse = ", "),
            paste(cons, collapse = ", "),
            paste(nuevos, collapse = ", ")
          ))
          cambio <- TRUE
        }
      }
    }
  }
  list(cierre = actual, traza = traza)
}

# Construye el grafo dirigido tipado (igraph) para el Graph-RAG.
construir_igraph <- function(grafo_limpio) {
  df <- grafo_limpio
  if (!"Relacion" %in% names(df)) df$Relacion <- "relacionado_con"
  igraph::graph_from_data_frame(d = df[, c("Sujeto", "Objeto", "Relacion")], directed = TRUE)
}
