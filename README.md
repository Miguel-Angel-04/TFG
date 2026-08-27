# Motor de Inferencia Neuro-Simbólico (FCA · Graph-RAG)

Aplicación de razonamiento médico que combina un Modelo de Lenguaje de Gran Tamaño (LLM), el Análisis Formal de Conceptos (FCA) y la recuperación aumentada basada en grafos (Graph-RAG) en una arquitectura **neuro-simbólica** desarrollada en R/Shiny.

> Trabajo de Fin de Grado | Grado en Ingeniería Informática, Universidad de Málaga.

> Departamento de Matemática Aplicada. Tutor: Ángel Mora Bonilla.

---

## Idea central

La mayoría de los sistemas basados en LLM delegan el razonamiento en el propio modelo, que genera texto de forma probabilística y puede **alucinar**. Este proyecto invierte esa división de tareas:

- **El LLM solo extrae y redacta.** Nunca razona. Se encarga de convertir un documento clínico en tripletas de conocimiento y de redactar en lenguaje natural la respuesta ya calculada.
- **El FCA es el motor de razonamiento determinista.** A partir del contexto formal y de la base de implicaciones de Duquenne-Guigues, el **cierre lógico** (encadenamiento hacia adelante hasta punto fijo) deduce la respuesta sin ninguna alucinación.
- **Graph-RAG recupera lo que la lógica de co-ocurrencia no alcanza.** Propaga por el grafo únicamente la relación `implica`, recuperando riesgos y consecuencias que el cierre por sí solo no deduce.

El resultado es un sistema en el que el razonamiento es **trazable, auditable y sin alucinación**, y en el que cada respuesta puede acompañarse de su traza lógica y de visualizaciones interactivas.

---

## Arquitectura

```
Documento clínico / CSV
        |
   [LLM extractor]  -->  Tripletas (Sujeto, Relación, Objeto)
        |
   [Motor simbólico FCA]  -->  Contexto formal + implicaciones + cierre lógico
        |
   [Graph-RAG]  -->  Propagación de `implica` (riesgos/consecuencias)
        |
   [Orquestador híbrido]  -->  Respuesta determinista + traza
        |
   [LLM redactor]  -->  Respuesta en lenguaje natural
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

La aplicación responde en lenguaje natural a preguntas ancladas siempre a los datos cargados. Para la validación, la batería de preguntas **se genera de forma programática a partir de los propios datos**, recorriendo tanto la matriz objeto-atributo del contexto formal como el grafo de conocimiento. Así cada pregunta lleva asociada su respuesta de referencia sin intervención manual y la batería crece automáticamente al ampliar el conjunto de datos. Se producen cuatro familias de forma automática, más un conjunto anotado a mano:

- **Hipotéticas por categoría (riesgos)** - "Si un paciente tiene [condición], ¿qué riesgos o consecuencias tendría?". Su respuesta de referencia son exactamente los nodos alcanzables a través de la relación `implica` desde esa condición, de modo que evalúan directamente la recuperación por grafo.
- **Sí/No positivas** - "Si un paciente tiene [condición], ¿tendría [uno de sus riesgos]?", con respuesta correcta SÍ. Comprueban que el sistema deduce lo que sí se sigue lógicamente.
- **Sí/No negativas** - de forma simétrica, "Si un paciente tiene [condición], ¿tendría [un riesgo de otra condición]?", eligiendo un riesgo que no se deduce; la respuesta correcta es NO. Penalizan afirmar algo que no procede.
- **Inversas factuales** - "¿Qué pacientes tienen [atributo]?", cuya referencia es la lista de pacientes que lo poseen según la matriz de datos. Contrastan el anclaje de las respuestas frente a la invención de sujetos.
- **Anotadas a mano** - preguntas cuya respuesta correcta se fija manualmente a partir de hechos directos y verificables (por ejemplo, si un paciente concreto tiene una condición, o si un fármaco está contraindicado con una alergia). Al ser independientes del razonamiento del sistema, **rompen la posible circularidad** de la evaluación.

Cada pregunta generada se identifica internamente mediante un **prefijo que codifica su tipo**, seguido de la entidad sobre la que versa, lo que facilita su trazabilidad en las tablas de resultados.

---

## Evaluación

La evaluación se plantea para verificar que trasladar el razonamiento a un motor simbólico determinista, en lugar de confiarlo al modelo de lenguaje, elimina las alucinaciones sin renunciar a la calidad de las respuestas. Se ponen a prueba tres afirmaciones: (1) el razonamiento sobre FCA no produce afirmaciones injustificadas; (2) la capa Graph-RAG aporta riesgos y consecuencias que el cierre lógico por sí solo no recupera; y (3) el sistema completo supera a los enfoques que delegan el razonamiento en el modelo de lenguaje.

Se comparan **cinco enfoques** que se diferencian solo en cómo recuperan y razonan, de modo que las diferencias puedan atribuirse a esa pieza concreta:

- **LLM solo** : línea base, sin recuperación de conocimiento.
- **RAG + LLM** : recuperación por similitud de las tripletas más parecidas a la pregunta.
- **Graph-RAG + LLM** : recuperación del vecindario de las entidades en el grafo.
- **FCA solo** : razonamiento por cierre lógico, sin propagación por el grafo.
- **Graph-RAG + FCA** : (sistema completo) propagación de `implica` + razonamiento simbólico.

La **verdad de referencia** (*gold*) procede de dos fuentes: una parte se deriva automáticamente de la estructura del conocimiento y otra se **anota a mano** a partir de hechos directos y verificables. Esta doble procedencia es deliberada: el conjunto anotado a mano ancla la evaluación en hechos independientes del sistema y **rompe la circularidad** de evaluar el motor contra su propia salida. Para medir la alucinación se toma como referencia la **verdad lógica** del conocimiento (el cierre completo de lo que se deduce): se considera alucinación toda afirmación que quede fuera de ese cierre.

Las **métricas** son la exactitud (global y en las preguntas de sí/no), la precisión, el recall y su media armónica F1 en las preguntas de conjunto (con un tratamiento explícito de la abstención, que se excluye del promedio en lugar de contarse como precisión perfecta), y, como métrica central, la **tasa de alucinación**. Como los enfoques con modelo de lenguaje no son deterministas, se ejecutan varias veces y sus métricas se reportan como **media ± desviación típica**; los enfoques simbólicos, deterministas, se ejecutan una sola vez.

El resultado principal confirma que los enfoques deterministas (FCA solo y Graph-RAG+FCA) presentan **alucinación nula**, frente a los que delegan el razonamiento en el modelo; y el sistema completo, gracias a la propagación de `implica`, mejora el *recall* de riesgos que el cierre lógico por sí solo no alcanza. Los experimentos se ejecutan con un modelo local mediante **Ollama** (temperatura cero), y cada evaluación puede exportarse como informe **HTML** autocontenido o como datos **CSV** para su reproducción.
