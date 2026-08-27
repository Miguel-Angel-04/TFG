# presentacion.R — Funciones de apoyo a la interfaz (solo dan formato).

# Pasa las líneas de reglas ("Rule N: [A, B] ⇒ [C]") a un data.frame para la tabla.
reglas_a_dataframe <- function(lineas) {
  vacio <- data.frame(Regla = character(0), Antecedente = character(0),
                      Consecuente = character(0), NAnt = integer(0),
                      NCons = integer(0), stringsAsFactors = FALSE)
  if (length(lineas) == 0) return(vacio)

  # Texto entre corchetes, legible (quita guiones bajos)
  contenido <- function(s) {
    s <- sub(".*\\[", "", sub("\\].*", "", s))
    trimws(gsub("_", " ", gsub("\\s+", " ", s)))
  }
  # Cuenta los atributos de un lado
  n_attr <- function(s) {
    s <- sub(".*\\[", "", sub("\\].*", "", s))
    length(Filter(nzchar, trimws(unlist(strsplit(s, ",")))))
  }

  regla  <- trimws(sub(":.*", "", lineas))              # "Rule 12"
  cuerpo <- sub("^Rule\\s*\\d+:\\s*", "", lineas)        # "[A, B] ⇒ [C]"
  partes <- strsplit(cuerpo, "⇒", fixed = TRUE)
  ant <- vapply(partes, function(p) if (length(p) >= 1) p[[1]] else "", character(1))
  con <- vapply(partes, function(p) if (length(p) >= 2) p[[2]] else "", character(1))

  data.frame(
    Regla       = regla,
    Antecedente = vapply(ant, contenido, character(1)),
    Consecuente = vapply(con, contenido, character(1)),
    NAnt        = vapply(ant, n_attr, integer(1)),
    NCons       = vapply(con, n_attr, integer(1)),
    stringsAsFactors = FALSE
  )
}

# Crea una tarjeta KPI (value_box) con estilo uniforme.
kpi_box <- function(titulo, valor, icono, tema = "primary") {
  bslib::value_box(
    title    = titulo,
    value    = valor,
    showcase = bsicons::bs_icon(icono),
    theme    = tema
  )
}
