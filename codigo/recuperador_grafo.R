# Capa de recuperación (Graph-RAG): saca del grafo el conocimiento entidad→entidad.

# Solo 'implica' propaga estado al sujeto; otras relaciones meterían atributos falsos.
RELACIONES_PROPAGABLES <- c("implica")

# Vecinos de un nodo por dirección, filtrando por tipo de relación.
vecinos_igraph <- function(nodo, ig, modo = "out", relaciones = NULL) {
  if (is.null(ig) || !(nodo %in% igraph::V(ig)$name)) return(character(0))
  eids <- igraph::incident(ig, nodo, mode = modo)
  if (length(eids) == 0) return(character(0))
  if (!is.null(relaciones)) {
    rel  <- igraph::edge_attr(ig, "Relacion", eids)
    eids <- eids[rel %in% relaciones]
  }
  if (length(eids) == 0) return(character(0))
  ends <- igraph::ends(ig, eids, names = TRUE)
  vecinos <- ifelse(ends[, 1] == nodo, ends[, 2], ends[, 1])
  unique(setdiff(vecinos, nodo))
}

# Expande un conjunto de atributos con lo que 'implica' en el grafo.
# Devuelve los nodos nuevos, la traza ("A --rel--> B") y las aristas.
expandir_con_grafo <- function(attrs, ig, pasos = 1, relaciones = RELACIONES_PROPAGABLES) {
  vacio <- list(nodos = character(0), traza = character(0),
                aristas = data.frame(from = character(0), to = character(0),
                                     rel = character(0), stringsAsFactors = FALSE))
  if (is.null(ig) || length(attrs) == 0) return(vacio)
  alcanzado <- character(0); traza <- character(0)
  a_from <- character(0); a_to <- character(0); a_rel <- character(0)
  frontera <- intersect(attrs, igraph::V(ig)$name)
  for (k in seq_len(pasos)) {
    nueva_frontera <- character(0)
    for (nodo in frontera) {
      eids <- igraph::incident(ig, nodo, mode = "out")
      if (length(eids) == 0) next
      rel  <- igraph::edge_attr(ig, "Relacion", eids)
      keep <- rel %in% relaciones
      eids <- eids[keep]; rel <- rel[keep]
      if (length(eids) == 0) next
      ends <- igraph::ends(ig, eids, names = TRUE)
      dest <- ifelse(ends[, 1] == nodo, ends[, 2], ends[, 1])
      for (j in seq_along(dest)) {
        if (!(dest[j] %in% c(attrs, alcanzado))) {
          alcanzado      <- c(alcanzado, dest[j])
          nueva_frontera <- c(nueva_frontera, dest[j])
          traza  <- c(traza, sprintf("%s --%s--> %s", nodo, rel[j], dest[j]))
          a_from <- c(a_from, nodo); a_to <- c(a_to, dest[j]); a_rel <- c(a_rel, rel[j])
        }
      }
    }
    if (length(nueva_frontera) == 0) break
    frontera <- unique(nueva_frontera)
  }
  list(nodos = unique(alcanzado), traza = traza,
       aristas = data.frame(from = a_from, to = a_to, rel = a_rel, stringsAsFactors = FALSE))
}

# Aristas salientes de un nodo como data.frame(attr, rel): para listar y agrupar por relación.
aristas_salientes <- function(nodo, ig) {
  vacio <- data.frame(attr = character(0), rel = character(0), stringsAsFactors = FALSE)
  if (is.null(ig) || !(nodo %in% igraph::V(ig)$name)) return(vacio)
  eids <- igraph::incident(ig, nodo, mode = "out")
  if (length(eids) == 0) return(vacio)
  rel  <- igraph::edge_attr(ig, "Relacion", eids)
  ends <- igraph::ends(ig, eids, names = TRUE)
  dest <- ifelse(ends[, 1] == nodo, ends[, 2], ends[, 1])
  data.frame(attr = dest, rel = rel, stringsAsFactors = FALSE)
}

# Clasifica atributos por su relación ENTRANTE (para agrupar sin sujeto, p.ej. hipótesis).
aristas_categoria <- function(attrs, ig) {
  vacio <- data.frame(attr = character(0), rel = character(0), stringsAsFactors = FALSE)
  if (is.null(ig)) return(vacio)
  filas <- lapply(intersect(attrs, igraph::V(ig)$name), function(a) {
    eids <- igraph::incident(ig, a, mode = "in")
    if (length(eids) == 0) return(NULL)
    rel <- igraph::edge_attr(ig, "Relacion", eids)
    data.frame(attr = a, rel = rel[1], stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(vacio), Filter(Negate(is.null), filas)))
}

# Consulta directa "¿sujeto (relación) objeto?" mirando solo el grafo.
consulta_grafo_directa <- function(sujeto, objeto, ig, relaciones = NULL) {
  if (is.null(ig)) return(FALSE)
  objeto %in% vecinos_igraph(sujeto, ig, modo = "out", relaciones = relaciones)
}
