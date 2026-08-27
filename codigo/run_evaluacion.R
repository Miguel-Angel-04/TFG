# ==========================================
# run_evaluacion.R — Script REPRODUCIBLE de evaluación (P0)
# Ejecuta la batería en los 5 brazos, calcula métricas y genera:
#   evaluacion_salida/resultados.csv     (fila por pregunta × brazo)
#   evaluacion_salida/resumen.csv        (medias por brazo)
#   evaluacion_salida/grafica_*.png      (comparativas)
#   evaluacion_salida/informe.md         (resumen para la memoria)
#
# Uso:  Rscript run_evaluacion.R  [ruta_csv]   (por defecto datos_medicos.csv)
# Requisitos: Ollama en marcha con 'llama3.1' (y opcional 'nomic-embed-text').
# ==========================================

# --- Carga de módulos (sin arrancar Shiny) ---
suppressWarnings(suppressMessages({
  library(fcaR); library(igraph); library(dplyr); library(tidyr)
  library(ellmer); library(ggplot2)
}))
for (m in c("motor_simbolico.R", "enrutador_semantico.R", "recuperador_grafo.R",
            "orquestador_hibrido.R", "agente_llm.R", "evaluacion.R")) {
  ruta <- if (file.exists(file.path("R", m))) file.path("R", m) else m
  source(ruta)
}

args     <- commandArgs(trailingOnly = TRUE)
ruta_csv <- if (length(args) >= 1) args[1] else "datos_medicos.csv"
dir_out  <- "evaluacion_salida"
dir.create(dir_out, showWarnings = FALSE)

cat("[Eval] Construyendo contexto formal desde", ruta_csv, "...\n")
ctx <- construir_contexto(ruta_csv)

cat("[Eval] Ejecutando batería en los 5 brazos (puede tardar; hay llamadas al LLM)...\n")
res <- ejecutar_evaluacion(ctx, model = "llama3.1", usar_embeddings = TRUE, repeticiones = 3,
                           progreso = function(p, msg) cat(sprintf("  [%3d%%] %s\n", round(p * 100), msg)))
resumen <- resumen_evaluacion(res)   # formateado "media ± sd"
rn      <- resumen_num(res)          # numérico para gráficas

cat("\n=== RESUMEN POR BRAZO (motor RAG:", attr(res, "motor_rag"),
    "| repeticiones LLM:", attr(res, "repeticiones"), ") ===\n")
print(resumen, row.names = FALSE)

write.csv(res,     file.path(dir_out, "resultados.csv"), row.names = FALSE)
write.csv(resumen, file.path(dir_out, "resumen.csv"),    row.names = FALSE)

# --- Gráficas (numéricas, con barras de error = sd entre repeticiones) ---
rn$Brazo <- factor(rn$Brazo, levels = BRAZOS_EVAL)

g1 <- ggplot(rn, aes(x = Brazo, y = Alucinacion, fill = Brazo)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = pmax(0, Alucinacion - Alucinacion_sd), ymax = Alucinacion + Alucinacion_sd),
                width = 0.25, color = "#444444") +
  geom_text(aes(label = sprintf("%.2f", Alucinacion)), vjust = -0.6, size = 3.5) +
  labs(title = "Tasa de alucinación por brazo (menor es mejor)", x = NULL, y = "Alucinación") +
  theme_minimal(base_size = 12) + theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(dir_out, "grafica_alucinacion.png"), g1, width = 8, height = 4.5, dpi = 120)

g2 <- ggplot(rn, aes(x = Brazo, y = Exactitud, fill = Brazo)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = pmax(0, Exactitud - Exactitud_sd), ymax = pmin(1, Exactitud + Exactitud_sd)),
                width = 0.25, color = "#444444") +
  geom_text(aes(label = sprintf("%.2f", Exactitud)), vjust = -0.6, size = 3.5) +
  labs(title = "Exactitud global por brazo (mayor es mejor)", x = NULL, y = "Exactitud") +
  theme_minimal(base_size = 12) + theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(dir_out, "grafica_exactitud.png"), g2, width = 8, height = 4.5, dpi = 120)

# --- Informe markdown ---
tabla_md <- function(df) {
  cab <- paste("|", paste(names(df), collapse = " | "), "|")
  sep <- paste("|", paste(rep("---", ncol(df)), collapse = " | "), "|")
  fil <- apply(df, 1, function(r) paste("|", paste(r, collapse = " | "), "|"))
  paste(c(cab, sep, fil), collapse = "\n")
}
n_preg <- length(unique(res$id))
n_man  <- length(unique(res$id[res$fuente == "manual"]))
informe <- paste0(
  "# Evaluación del sistema neuro-simbólico (FCA · Graph-RAG · LLM)\n\n",
  "Batería: **", n_preg, " preguntas** (", n_man, " con gold anotado a mano, independiente del grafo; ",
  "el resto auto: categoría-riesgos vía `implica`, sí/no y filtro inverso). ",
  "Motor de recuperación RAG: **", attr(res, "motor_rag"), "**. ",
  "Repeticiones de los brazos con LLM: **", attr(res, "repeticiones"), "** (media ± sd). ",
  "Temperatura del LLM: 0.\n\n",
  "## Resumen por brazo\n\n", tabla_md(resumen), "\n\n",
  "## Lectura\n\n",
  "- **Alucinación**: los brazos deterministas (FCA solo, Graph-RAG+FCA) deben salir en ~0; ",
  "los brazos con LLM razonando (LLM solo, RAG+LLM, Graph-RAG+LLM) muestran su tendencia a inventar.\n",
  "- **Graph-RAG+FCA vs FCA solo**: mide lo que aporta el grafo (`implica`) en recall de riesgos.\n",
  "- **Graph-RAG+FCA vs Graph-RAG+LLM**: mide lo que aporta el cierre FCA frente a dejar razonar al LLM.\n\n",
  "![Alucinación](grafica_alucinacion.png)\n\n![Exactitud](grafica_exactitud.png)\n"
)
writeLines(informe, file.path(dir_out, "informe.md"))
cat("\n[Eval] Hecho. Resultados en:", normalizePath(dir_out), "\n")
