# Grafo base (visNetwork) y resaltados dinámicos.

# Retículo de conceptos como red interactiva (diagrama de Hasse).
reticulo_visnetwork <- function(fcs) {
  ext  <- as.matrix(fcs$concepts$extents()) > 0
  int  <- as.matrix(fcs$concepts$intents()) > 0
  objs <- fcs$objects
  atrs <- fcs$attributes
  if (nrow(ext) != length(objs)) ext <- t(ext)   # objetos x conceptos
  if (nrow(int) != length(atrs)) int <- t(int)   # atributos x conceptos
  nC <- ncol(int)

  # below[i,j] = concepto i es subconcepto ESTRICTO de j (i más específico)
  below <- matrix(FALSE, nC, nC)
  for (i in seq_len(nC)) for (j in seq_len(nC)) {
    if (i != j && all(int[, j] <= int[, i]) && any(int[, i] != int[, j])) below[i, j] <- TRUE
  }
  # cobertura (Hasse): i cubierto por j si NO hay k intermedio (i < k < j)
  cover <- below
  for (i in seq_len(nC)) for (j in which(below[i, ])) {
    if (any(below[i, ] & below[, j])) cover[i, j] <- FALSE
  }

  labels <- character(nC); titles <- character(nC); niveles <- integer(nC)
  for (i in seq_len(nC)) {
    intent_i <- atrs[int[, i]]
    extent_i <- objs[ext[, i]]
    niveles[i] <- length(intent_i)
    sup <- which(cover[i, ])
    intent_sup <- if (length(sup)) atrs[rowSums(int[, sup, drop = FALSE]) > 0] else character(0)
    introducidos <- setdiff(intent_i, intent_sup)
    labels[i] <- gsub("_", " ", paste(introducidos, collapse = ", "))
    titles[i] <- paste0(
      "<b>Objetos (", length(extent_i), "):</b> ",
      if (length(extent_i)) gsub("_", " ", paste(extent_i, collapse = ", ")) else "&mdash;",
      "<br><b>Atributos (", length(intent_i), "):</b> ",
      if (length(intent_i)) gsub("_", " ", paste(intent_i, collapse = ", ")) else "&mdash;")
  }
  nodos <- data.frame(id = seq_len(nC),
                      label = ifelse(nzchar(labels), labels, " "),
                      title = titles, level = niveles,
                      shape = "box", stringsAsFactors = FALSE)
  idx <- which(cover, arr.ind = TRUE)   # col1 = i (sub), col2 = j (super)
  aristas <- if (nrow(idx) > 0) data.frame(from = idx[, 2], to = idx[, 1])
             else data.frame(from = integer(0), to = integer(0))

  visNetwork(nodos, aristas, height = "100%") %>%
    visHierarchicalLayout(direction = "UD", sortMethod = "directed",
                          levelSeparation = 100, nodeSpacing = 170) %>%
    visEdges(arrows = "", color = list(color = "#B0B7BC"), smooth = FALSE) %>%
    visNodes(color = list(background = "#eaf2f8", border = "#5D8AA8"),
             font = list(size = 13)) %>%
    visPhysics(enabled = FALSE) %>%
    visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, tooltipDelay = 80) %>%
    visOptions(nodesIdSelection = FALSE)
}

# --- Paleta de colores por tipo de entidad ---
PALETA <- list(
  Paciente  = "#3498DB",
  Protocolo = "#9B59B6",
  Entidad   = "#E67E22",
  Atributo  = "#1ABC9C",
  Highlight_Sujeto = "#E74C3C",
  Highlight_Attr   = "#F39C12"
)
# --- Colores de arista por tipo de relación (Graph tipado) ---
COLORES_RELACION <- c(
  tiene_sintoma           = "#7F8C8D",
  tiene_condicion         = "#7F8C8D",
  diagnosticado_con       = "#34495E",
  recibe_tratamiento      = "#27AE60",
  trata                   = "#16A085",
  pertenece_a             = "#2980B9",
  incluye                 = "#2980B9",
  contraindicado_con      = "#E74C3C",
  implica                 = "#8E44AD",
  requiere_prueba         = "#E67E22",
  requiere_monitorizacion = "#E67E22",
  requiere_sintoma        = "#E67E22",
  requiere_condicion      = "#E67E22",
  es_alternativa_a        = "#F1C40F",
  alternativa_tratamiento = "#F1C40F"
)
COLOR_RELACION_DEFECTO <- "#B0B7BC"

color_relacion <- function(rel) {
  out <- unname(COLORES_RELACION[rel])
  out[is.na(out)] <- COLOR_RELACION_DEFECTO
  out
}

# data.frame de aristas para visNetwork: id, rel (Relacion), color por tipo,
# title (tooltip) y label vacío (el texto de la relación se muestra solo al enfocar).
preparar_aristas <- function(grafo_limpio) {
  rel <- if ("Relacion" %in% names(grafo_limpio)) grafo_limpio$Relacion
  else rep("relacionado_con", nrow(grafo_limpio))
  a <- data.frame(
    from  = grafo_limpio$Sujeto,
    to    = grafo_limpio$Objeto,
    rel   = rel,
    color = color_relacion(rel),
    title = rel,
    label = "",           # sin texto en la vista base (evita saturar el render)
    stringsAsFactors = FALSE
  )
  # 'incluye' y 'pertenece_a' son inversas (mismo par de nodos en sentidos opuestos):
  # al dibujarse encimadas sus etiquetas se solapan. Se conserva solo 'pertenece_a'.
  clave      <- paste(pmin(a$from, a$to), pmax(a$from, a$to), sep = "\r")
  tiene_pert <- clave %in% clave[a$rel == "pertenece_a"]
  a <- a[!(a$rel == "incluye" & tiene_pert), , drop = FALSE]

  a$id <- paste0("e", seq_len(nrow(a)))
  a
}

clasificar_nodo <- function(id, es_sujeto) {
  if (!es_sujeto)                                  return("Atributo")
  if (grepl("^Paciente",  id, ignore.case = TRUE)) return("Paciente")
  if (grepl("^Protocolo", id, ignore.case = TRUE)) return("Protocolo")
  return("Entidad")
}

construir_nodos_base <- function(objetos, atributos) {
  atributos_unicos <- setdiff(atributos, objetos)
  tipos_obj <- sapply(objetos,          function(x) clasificar_nodo(x, TRUE),  USE.NAMES = FALSE)
  tipos_atr <- sapply(atributos_unicos, function(x) clasificar_nodo(x, FALSE), USE.NAMES = FALSE)
  tipos_all <- c(tipos_obj, tipos_atr)
  
  colores  <- unlist(PALETA[tipos_all], use.names = FALSE)
  formas   <- ifelse(tipos_all == "Atributo", "ellipse", "box")
  tamanios <- dplyr::case_when(
    tipos_all == "Paciente"  ~ 28,
    tipos_all == "Protocolo" ~ 30,
    tipos_all == "Entidad"   ~ 26,
    TRUE                     ~ 22
  )
  
  data.frame(
    id          = c(objetos, atributos_unicos),
    label       = c(objetos, atributos_unicos),
    group       = tipos_all,
    shape       = formas,
    color       = colores,
    size        = tamanios,
    borderWidth = 1,
    shadow      = FALSE,
    stringsAsFactors = FALSE
  )
}

construir_grafo <- function(nodos, aristas) {
  visNetwork(nodos, aristas, height = "100%") %>%
    # Layout estático (igraph) con física OFF: el grafo no oscila y el zoom va fluido.
    visIgraphLayout(layout = "layout_with_fr", randomSeed = 42) %>%
    visGroups(groupname = "Paciente",
              color = list(background = PALETA$Paciente,  border = "#1A6FA8"),
              font  = list(color = "white", size = 12)) %>%
    visGroups(groupname = "Protocolo",
              color = list(background = PALETA$Protocolo, border = "#6C3483"),
              font  = list(color = "white", size = 12)) %>%
    visGroups(groupname = "Entidad",
              color = list(background = PALETA$Entidad,   border = "#A04000"),
              font  = list(color = "white", size = 12)) %>%
    visGroups(groupname = "Atributo",
              color = list(background = PALETA$Atributo,  border = "#0E8068"),
              font  = list(color = "white", size = 11)) %>%
    visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
      color  = list(color = "#B0B7BC", highlight = "#E74C3C", hover = "#E67E22"),
      smooth = FALSE,   # rectas: imprescindible para que el zoom/paneo vaya fluido con el grafo completo
      font   = list(size = 11, align = "top", strokeWidth = 4, strokeColor = "#ffffff"),
      width  = 1
    ) %>%
    visOptions(nodesIdSelection = FALSE) %>%
    visInteraction(
      dragNodes         = TRUE,
      dragView          = TRUE,
      zoomView          = TRUE,
      navigationButtons = TRUE,
      keyboard          = list(enabled = TRUE, bindToWindow = FALSE),
      tooltipDelay      = 150
    ) %>%
    visEvents(
      # Al dibujarse avisa a Shiny y repinta: el panel aún se está redimensionando y
      # algunas etiquetas de arista no llegan a pintarse en el primer dibujado.
      afterDrawing = "function(){ if(!this._posDone){ this._posDone = true;
        Shiny.setInputValue('grafo_lista', Math.random(), {priority:'event'});
        var net = this;
        setTimeout(function(){ net.setSize('100%','100%'); net.redraw(); }, 200);
        setTimeout(function(){ net.setSize('100%','100%'); net.redraw(); }, 800);
      } }",
      # Doble clic en un nodo: lo aísla con sus vecinos; en el fondo: restaura todo.
      # Se usa doble clic (no simple) para no interferir con el zoom ni los botones.
      doubleClick = "function(params){
        if (params.nodes && params.nodes.length > 0) {
          Shiny.setInputValue('grafo_nodo_click', params.nodes[0], {priority: 'event'});
        } else if (!params.edges || params.edges.length === 0) {
          Shiny.setInputValue('grafo_click_vacio', Math.random(), {priority: 'event'});
        }
      }"
    )
}

# ENFOQUE POR CLIC: muestra SOLO el nodo pulsado y sus vecinos; OCULTA el resto
# (hidden = TRUE). El nodo pulsado se agranda y resalta.
df_solo_foco <- function(nodos_base, ids_foco, item, destacar = character(0)) {
  en_foco <- nodos_base$id %in% ids_foco
  es_item <- nodos_base$id %in% item
  es_dest <- nodos_base$id %in% destacar
  data.frame(
    id          = nodos_base$id,
    hidden      = !en_foco,
    color       = ifelse(es_dest, "#E74C3C", nodos_base$color),
    size        = ifelse(es_item, pmax(nodos_base$size + 6, 26), nodos_base$size),
    borderWidth = ifelse(es_item | es_dest, 4, 1),
    shadow      = es_item | es_dest,
    stringsAsFactors = FALSE
  )
}

# RESET de nodos: los vuelve a mostrar todos con su color y tamaño base.
df_reset_nodos <- function(nodos_base) {
  data.frame(
    id          = nodos_base$id,
    hidden      = FALSE,
    color       = nodos_base$color,
    size        = nodos_base$size,
    borderWidth = 1,
    shadow      = FALSE,
    stringsAsFactors = FALSE
  )
}

# Resaltado simple (compatibilidad).
df_resaltado <- function(nodos_highlight_ids, objetos, universo) {
  ids_validos <- unique(intersect(nodos_highlight_ids, c(objetos, universo)))
  if (length(ids_validos) == 0) return(NULL)
  es_sujeto <- ids_validos %in% objetos
  data.frame(
    id          = ids_validos,
    size        = ifelse(es_sujeto, 36, 26),
    color       = ifelse(es_sujeto, PALETA$Highlight_Sujeto, PALETA$Highlight_Attr),
    borderWidth = 3,
    shadow      = TRUE,
    stringsAsFactors = FALSE
  )
}

# Vecinos directos de un conjunto de ids.
vecinos_de <- function(ids, aristas) {
  unique(c(aristas$to[aristas$from %in% ids],
           aristas$from[aristas$to %in% ids]))
}

# ENFOQUE de NODOS: foco en rojo/amarillo grande; el resto, gris y pequeño.
df_enfoque <- function(nodos_base, ids_foco, objetos) {
  foco      <- nodos_base$id %in% ids_foco
  es_sujeto <- nodos_base$id %in% objetos
  data.frame(
    id          = nodos_base$id,
    color       = ifelse(!foco, "#DDE1E3",
                         ifelse(es_sujeto, PALETA$Highlight_Sujeto, PALETA$Highlight_Attr)),
    size        = ifelse(!foco, pmax(nodos_base$size - 8, 12),
                         ifelse(es_sujeto, 40, 28)),
    borderWidth = ifelse(foco, 3, 1),
    shadow      = foco,
    stringsAsFactors = FALSE
  )
}

# ENFOQUE de ARISTAS: rojas y gruesas las que unen dos nodos del foco; el resto
# muy tenues. Requiere que 'aristas' tenga columna id.
df_enfoque_aristas <- function(aristas, ids_foco) {
  en_foco <- aristas$from %in% ids_foco & aristas$to %in% ids_foco
  data.frame(
    id    = aristas$id,
    color = ifelse(en_foco, "#E74C3C", "#ECECEC"),
    width = ifelse(en_foco, 3, 0.4),
    stringsAsFactors = FALSE
  )
}

# Devuelve las aristas a su estilo base (gris).
df_reset_aristas <- function(aristas) {
  data.frame(
    id     = aristas$id,
    color  = aristas$color,
    width  = 1,
    label  = "",
    dashes = FALSE,
    stringsAsFactors = FALSE
  )
}
# ENFOQUE por CONSULTA: sujeto(s) en SU color, objetos preguntados en ROJO,
# ambos a tamaño 25; el resto atenuado en gris.
df_enfoque_consulta <- function(nodos_base, sujetos, destacar) {
  es_dest <- nodos_base$id %in% destacar
  es_suj  <- nodos_base$id %in% sujetos & !es_dest
  foco    <- es_suj | es_dest
  data.frame(
    id          = nodos_base$id,
    color       = ifelse(es_dest, "#E74C3C",
                         ifelse(es_suj, nodos_base$color, "#DDE1E3")),
    size        = ifelse(foco, 25, pmax(nodos_base$size - 8, 12)),
    borderWidth = ifelse(foco, 3, 1),
    shadow      = foco,
    stringsAsFactors = FALSE
  )
}
# Enfoque completo: el objeto y sus atributos en color, el preguntado en rojo, resto gris.
df_enfoque_full <- function(nodos_base, foco, sujetos, destacar) {
  en_foco <- nodos_base$id %in% foco
  es_dest <- nodos_base$id %in% destacar
  es_suj  <- nodos_base$id %in% sujetos
  data.frame(
    id          = nodos_base$id,
    color       = ifelse(es_dest, "#E74C3C",
                         ifelse(en_foco, nodos_base$color, "#DDE1E3")),
    size        = ifelse(es_suj, 25,
                         ifelse(en_foco | es_dest, nodos_base$size, pmax(nodos_base$size - 8, 12))),
    borderWidth = ifelse(en_foco | es_dest, 3, 1),
    shadow      = en_foco | es_dest,
    stringsAsFactors = FALSE
  )
}

# ENFOQUE por FILTRO (desplegable): el elemento buscado a tamaño 25 con borde;
# sus VECINOS conservan su color y tamaño por defecto; el resto, gris.
df_enfoque_item <- function(nodos_base, ids_foco, item) {
  en_foco <- nodos_base$id %in% ids_foco
  es_item <- nodos_base$id %in% item
  data.frame(
    id          = nodos_base$id,
    color       = ifelse(en_foco, nodos_base$color, "#DDE1E3"),
    size        = ifelse(es_item, 25,
                         ifelse(en_foco, nodos_base$size, pmax(nodos_base$size - 8, 12))),
    borderWidth = ifelse(es_item, 3, 1),
    shadow      = es_item,
    stringsAsFactors = FALSE
  )
}

df_aristas_foco <- function(aristas, ids, ambos = TRUE) {
  en <- if (ambos) (aristas$from %in% ids & aristas$to %in% ids)
  else       (aristas$from %in% ids | aristas$to %in% ids)
  data.frame(
    id     = aristas$id,
    color  = ifelse(en, aristas$color, "#ECECEC"),
    width  = ifelse(en, 2.4, 0.4),
    label  = ifelse(en, aristas$rel, ""),
    dashes = FALSE,
    stringsAsFactors = FALSE
  )
}
# Resalta en MORADO grueso y discontinuo las aristas recuperadas por Graph-RAG
# (p.ej. Asma --implica--> Riesgo_Broncoespasmo). rag_pairs: data.frame(from, to).
df_aristas_rag <- function(aristas, rag_pairs) {
  if (is.null(rag_pairs) || nrow(rag_pairs) == 0) return(NULL)
  key_a <- paste(aristas$from, aristas$to, sep = "\r")
  key_r <- paste(rag_pairs$from, rag_pairs$to, sep = "\r")
  sel   <- key_a %in% key_r
  if (!any(sel)) return(NULL)
  data.frame(
    id     = aristas$id[sel],
    color  = "#8E44AD",
    width  = 4,
    label  = aristas$rel[sel],
    dashes = TRUE,
    stringsAsFactors = FALSE
  )
}