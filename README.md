# Motor de Inferencia Neuro-Simbólico (FCA · Graph-RAG)

Aplicación de razonamiento médico que combina un Modelo de Lenguaje de Gran Tamaño (LLM), el Análisis Formal de Conceptos (FCA) y la recuperación aumentada basada en grafos (Graph-RAG) en una arquitectura **neuro-simbólica** desarrollada en R/Shiny.

> Trabajo de Fin de Grado — Grado en Ingeniería Informática, Universidad de Málaga.
> Departamento de Matemática Aplicada. Tutor: Ángel Mora Bonilla.

---

## Idea central

La mayoría de los sistemas basados en LLM delegan el razonamiento en el propio modelo, que genera texto de forma probabilística y puede **alucinar**. Este proyecto invierte esa división de tareas:

- **El LLM solo extrae y redacta.** Nunca razona. Se encarga de convertir un documento clínico en tripletas de conocimiento y de redactar en lenguaje natural la respuesta ya calculada.
- **El FCA es el motor de razonamiento determinista.** A partir del contexto formal y de la base de implicaciones de Duquenne-Guigues, el **cierre lógico** (encadenamiento hacia adelante hasta punto fijo) deduce la respuesta sin ninguna alucinación.
- **Graph-RAG recupera lo que la lógica de co-ocurrencia no alcanza.** Propaga por el grafo *únicamente* la relación `implica`, recuperando riesgos y consecuencias que el cierre por sí solo no deduce.

El resultado es un sistema en el que el razonamiento es **trazable, auditable y sin alucinación**, y en el que cada respuesta puede acompañarse de su traza lógica y de visualizaciones interactivas.

---

## Arquitectura

```
Documento clínico / CSV
        │
   [LLM extractor]  ──►  Tripletas (Sujeto, Relación, Objeto)
        │
   [Motor simbólico FCA]  ──►  Contexto formal + implicaciones + cierre lógico
        │
   [Graph-RAG]  ──►  Propagación de `implica` (riesgos/consecuencias)
        │
   [Orquestador híbrido]  ──►  Respuesta determinista + traza
        │
   [LLM redactor]  ──►  Respuesta en lenguaje natural
```

Cada capa está aislada: el razonamiento (FCA + grafo) es determinista, y el LLM queda acotado a la entrada (extracción) y la salida (redacción).

---

## Características

- Extracción automática de conocimiento desde documentos (PDF/TXT) con IA.
- Razonamiento lógico determinista mediante FCA (conceptos, retículo e implicaciones).
- Recuperación de riesgos y consecuencias mediante Graph-RAG.
- Respuestas en lenguaje natural con **traza lógica** de cómo se han obtenido.
- Preguntas factuales, categóricas, de sí/no, inversas e **hipotéticas** ("imagina un paciente con síntomas X, Y…").
- Visualizaciones interactivas: grafo de conocimiento, retículo de conceptos, tabla de reglas FCA y panel de datos.
- **Modo básico / avanzado**: divulgación progresiva sobre el mismo motor.
- Módulo de **evaluación** que compara cinco enfoques con métricas (exactitud, precisión/recall/F1 y tasa de alucinación).
- Modo oscuro y paneles maximizables.

---

## Requisitos

- **R** (versión reciente) y **RStudio**.
- **Ollama** para ejecutar el modelo de lenguaje en local. Tras instalarlo, descargar los modelos:
```bash
  ollama pull llama3.1
  ollama pull nomic-embed-text
```
- **Paquetes de R**: `shiny`, `bslib`, `fcaR`, `igraph`, `visNetwork`, `ellmer`, `mirai`, `dplyr`, `reactable`, `ggplot2` (los de interfaz se instalan automáticamente la primera vez).

## Puesta en marcha

1. Asegúrate de que **Ollama está en ejecución**.
2. Abre el proyecto en RStudio y sitúate en `app.R`.
3. Pulsa **Run App**. La aplicación se abrirá en el navegador.
4. Carga conocimiento (extraer de un documento con IA, subir un CSV de tripletas o usar los datos de ejemplo), pulsa **Extraer Reglas y Contexto** y empieza a preguntar.

---

## Estructura del repositorio

### Código de la aplicación (R)

| Archivo | Responsabilidad |
|---|---|
| `app.R` | Punto de entrada de la aplicación Shiny. |
| `global.R` | Carga de librerías, fuentes y estado global. |
| `ui.R` / `server.R` | Interfaz y lógica de servidor. |
| `motor_simbolico.R` | Motor FCA: contexto formal, implicaciones y `cierre_logico`. |
| `recuperador_grafo.R` | Graph-RAG: `expandir_con_grafo` y `RELACIONES_PROPAGABLES`. |
| `orquestador_hibrido.R` | Orquestación FCA + grafo + LLM y construcción de la respuesta. |
| `enrutador_semantico.R` | Clasificación de preguntas y mapeo semántico (NLU). |
| `agente_llm.R` | Agente redactor: convierte el resultado lógico en prosa. |
| `extractor_tripletas.R` | Extracción de tripletas desde documentos con el LLM. |
| `evaluacion.R` / `run_evaluacion.R` | Batería de evaluación y comparativa de enfoques. |
| `graph_viz.R` | Preparación de nodos y aristas para la visualización. |
| `presentacion.R` | Textos de presentación / ayuda de la aplicación. |

### Datos

| Archivo | Descripción |
|---|---|
| `datos_medicos.csv` | Dataset principal (medicina interna: respiratorio, cardiovascular, infeccioso, metabólico). |
| `datos_medicos_v2.csv` | Dataset paralelo para validar la generalización (neurología, reumatología, endocrino, digestivo, psiquiatría). Mismo esquema, vocabulario distinto. |
| `documento_prueba_medico.txt` | Documento clínico de ejemplo para la extracción con IA. |

### Documentación (memoria)

Los capítulos de la memoria se encuentran en los archivos `memoria_*.md` (marco teórico, metodología, tecnologías, desarrollo, arquitectura del motor, validación, conclusiones), junto con `manual_usuario.md`, `bibliografia.md` y material de apoyo (`preguntas.md`, `preparacion_defensa_QA.md`).

---

## Tipos de pregunta

- **Factual**: "¿Qué síntomas tiene el Paciente_01?"
- **Categórica**: "¿Qué tratamiento recibe el Paciente_03?"
- **Sí/No**: "¿El Paciente_02 tiene asma?"
- **Inversa**: "¿Qué pacientes reciben Metformina?"
- **Sobre protocolos**: "¿Qué pruebas requiere el protocolo de neumonía?"
- **Hipotética**: "Imagina un paciente con Fiebre y Tos Productiva, ¿qué se deduciría?"

---

## Evaluación

El sistema se valida por ablación comparando cinco enfoques —LLM solo, RAG+LLM, Graph-RAG+LLM, FCA solo y Graph-RAG+FCA— sobre una batería de preguntas con verdad de referencia **automática** (derivada del grafo) y **manual** (anotada a mano, para romper la circularidad). Las métricas incluyen exactitud, precisión/recall/F1 y **tasa de alucinación**; los enfoques deterministas mantienen alucinación nula.

---

## Autoría

Trabajo de Fin de Grado de Ingeniería Informática (Universidad de Málaga), tutorizado por Ángel Mora Bonilla (Departamento de Matemática Aplicada).
