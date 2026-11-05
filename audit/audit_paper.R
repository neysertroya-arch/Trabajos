# ============================================================
# AUDITOR CIENTÍFICO DEL ARTÍCULO
# ============================================================
#
# Objetivo:
# Comprobar que el proyecto contiene los elementos científicos
# mínimos necesarios para avanzar de manera reproducible.
#
# Estados:
# PASS   = requisito satisfecho
# REVIEW = existe, pero requiere revisión humana
# BLOCK  = ausencia crítica que impide considerar el artículo
#          listo para submission
#
# ============================================================


auditar_articulo <- function(project_dir = ".") {

  # ----------------------------------------------------------
  # 1. DEPENDENCIAS
  # ----------------------------------------------------------

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop(
      "Falta el paquete 'yaml'. Ejecute renv::install('yaml')."
    )
  }

  project_dir <- normalizePath(
    project_dir,
    winslash = "/",
    mustWork = TRUE
  )

  registro_file <- file.path(
    project_dir,
    "01_protocol",
    "registro_cientifico.yml"
  )

  if (!file.exists(registro_file)) {
    stop(
      "No existe 01_protocol/registro_cientifico.yml"
    )
  }

  x <- yaml::read_yaml(registro_file)


  # ----------------------------------------------------------
  # 2. FUNCIONES AUXILIARES
  # ----------------------------------------------------------

  tiene_texto <- function(valor) {

    !is.null(valor) &&
      length(valor) == 1 &&
      !is.na(valor) &&
      nzchar(trimws(as.character(valor)))
  }


  agregar_control <- function(
    area,
    control,
    estado,
    detalle
  ) {

    data.frame(
      area = area,
      control = control,
      estado = estado,
      detalle = detalle,
      stringsAsFactors = FALSE
    )
  }


  controles <- list()


  # ----------------------------------------------------------
  # 3. PREGUNTA DE INVESTIGACIÓN
  # ----------------------------------------------------------

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Integridad científica",
      "Pregunta de investigación definida",
      if (tiene_texto(x$research$question)) "PASS" else "BLOCK",
      if (tiene_texto(x$research$question)) {
        x$research$question
      } else {
        "La pregunta de investigación todavía no está definida."
      }
    )


  # ----------------------------------------------------------
  # 4. GAP
  # ----------------------------------------------------------

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Contribución",
      "GAP científico definido",
      if (tiene_texto(x$research$gap)) "PASS" else "BLOCK",
      if (tiene_texto(x$research$gap)) {
        x$research$gap
      } else {
        "Falta definir el vacío concreto en la literatura."
      }
    )


  # ----------------------------------------------------------
  # 5. CONTRIBUCIÓN
  # ----------------------------------------------------------

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Contribución",
      "Contribución explícita",
      if (tiene_texto(x$research$contribution)) "PASS" else "BLOCK",
      if (tiene_texto(x$research$contribution)) {
        x$research$contribution
      } else {
        "No se ha especificado la contribución del artículo."
      }
    )


  # ----------------------------------------------------------
  # 6. DATOS
  # ----------------------------------------------------------

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Datos",
      "Fuente de datos documentada",
      if (tiene_texto(x$data$source)) "PASS" else "BLOCK",
      if (tiene_texto(x$data$source)) {
        x$data$source
      } else {
        "Falta documentar la fuente de los datos."
      }
    )


  controles[[length(controles) + 1]] <-
    agregar_control(
      "Datos",
      "Versión de datos documentada",
      if (tiene_texto(x$data$version)) "PASS" else "REVIEW",
      if (tiene_texto(x$data$version)) {
        x$data$version
      } else {
        "Conviene registrar versión o fecha de descarga."
      }
    )


  controles[[length(controles) + 1]] <-
    agregar_control(
      "Datos",
      "Unidad de análisis definida",
      if (tiene_texto(x$data$unit_of_analysis)) "PASS" else "BLOCK",
      if (tiene_texto(x$data$unit_of_analysis)) {
        x$data$unit_of_analysis
      } else {
        "La unidad de análisis no está definida."
      }
    )


  # ----------------------------------------------------------
  # 7. MÉTODO
  # ----------------------------------------------------------

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Metodología",
      "Método principal definido",
      if (tiene_texto(x$method$main_method)) "PASS" else "BLOCK",
      if (tiene_texto(x$method$main_method)) {
        x$method$main_method
      } else {
        "Falta especificar el método principal."
      }
    )


  controles[[length(controles) + 1]] <-
    agregar_control(
      "Metodología",
      "Robustez planificada",
      if (tiene_texto(x$method$robustness)) "PASS" else "REVIEW",
      if (tiene_texto(x$method$robustness)) {
        x$method$robustness
      } else {
        "Definir únicamente las comprobaciones de robustez pertinentes."
      }
    )


  # ----------------------------------------------------------
  # 8. INFERENCIA CAUSAL
  # ----------------------------------------------------------

  causal <- isTRUE(x$method$causal_design)

controles[[length(controles) + 1]] <-
  agregar_control(
    "Metodología",
    "Tipo de inferencia declarado",
    if (causal) "REVIEW" else "PASS",
    if (causal) {
      paste(
        "El proyecto declara diseño causal.",
        "La estrategia de identificación debe ser validada por los autores."
      )
    } else {
      paste(
        "Diseño no causal declarado.",
        "Mantener interpretación asociativa y evitar lenguaje causal."
      )
    }
  )


  # ----------------------------------------------------------
  # 9. LITERATURA
  # ----------------------------------------------------------

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Evidencia",
      "Literatura auditada",
      if (isTRUE(x$evidence$literature_audited)) {
        "PASS"
      } else {
        "REVIEW"
      },
      if (isTRUE(x$evidence$literature_audited)) {
        "La auditoría bibliográfica está registrada."
      } else {
        "La literatura todavía no consta como auditada."
      }
    )


  # ----------------------------------------------------------
  # 10. DATOS AUDITADOS
  # ----------------------------------------------------------

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Evidencia",
      "Datos auditados",
      if (isTRUE(x$evidence$data_audited)) {
        "PASS"
      } else {
        "REVIEW"
      },
      if (isTRUE(x$evidence$data_audited)) {
        "La auditoría de datos está registrada."
      } else {
        "Los datos todavía no constan como auditados."
      }
    )


  # ----------------------------------------------------------
  # 11. REPRODUCIBILIDAD
  # ----------------------------------------------------------

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Reproducibilidad",
      "Resultados reproducibles",
      if (isTRUE(x$evidence$results_reproducible)) {
        "PASS"
      } else {
        "BLOCK"
      },
      if (isTRUE(x$evidence$results_reproducible)) {
        "Los resultados constan como reproducibles."
      } else {
        "No se ha confirmado reproducibilidad de resultados."
      }
    )


  # ----------------------------------------------------------
  # 12. REVISTA OBJETIVO
  # ----------------------------------------------------------

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Editorial",
      "Revista objetivo definida",
      if (tiene_texto(x$editorial$target_journal)) {
        "PASS"
      } else {
        "REVIEW"
      },
      if (tiene_texto(x$editorial$target_journal)) {
        x$editorial$target_journal
      } else {
        "Todavía no se ha definido revista objetivo."
      }
    )


  # ----------------------------------------------------------
  # 13. AUDITORÍA EDITORIAL
  # ----------------------------------------------------------

  controles_editoriales <- list(
    scope_checked =
      "Scope de la revista revisado",

    ai_policy_checked =
      "Política de IA revisada",

    data_policy_checked =
      "Política de datos revisada",

    code_policy_checked =
      "Política de código revisada",

    blind_review_checked =
      "Revisión ciega comprobada",

    costs_checked =
      "Costos de publicación comprobados",

    publication_time_checked =
      "Tiempos editoriales comprobados"
  )


  for (campo in names(controles_editoriales)) {

    valor <- isTRUE(
      x$editorial[[campo]]
    )

    controles[[length(controles) + 1]] <-
      agregar_control(
        "Editorial",
        controles_editoriales[[campo]],
        if (valor) "PASS" else "REVIEW",
        if (valor) {
          "Comprobado."
        } else {
          "Pendiente de comprobación."
        }
      )
  }


  # ----------------------------------------------------------
  # 14. ARCHIVOS ESENCIALES
  # ----------------------------------------------------------

  archivos_esenciales <- c(
    "index.qmd",
    "references.bib",
    "paper_status.yml",
    "renv.lock"
  )


  for (archivo in archivos_esenciales) {

    existe <- file.exists(
      file.path(
        project_dir,
        archivo
      )
    )

    controles[[length(controles) + 1]] <-
      agregar_control(
        "Infraestructura",
        paste("Existe", archivo),
        if (existe) "PASS" else "BLOCK",
        if (existe) {
          "Archivo localizado."
        } else {
          paste("Falta", archivo)
        }
      )
  }


  # ----------------------------------------------------------
  # 15. CONSOLIDAR
  # ----------------------------------------------------------

  auditoria <- do.call(
    rbind,
    controles
  )


  orden_estado <- c(
    "BLOCK",
    "REVIEW",
    "PASS"
  )

  auditoria$estado <- factor(
    auditoria$estado,
    levels = orden_estado
  )

  auditoria <- auditoria[
    order(auditoria$estado),
  ]

  auditoria$estado <- as.character(
    auditoria$estado
  )


  # ----------------------------------------------------------
  # 16. GUARDAR RESULTADO
  # ----------------------------------------------------------

  output_dir <- file.path(
    project_dir,
    "05_results"
  )

  dir.create(
    output_dir,
    showWarnings = FALSE,
    recursive = TRUE
  )

  output_file <- file.path(
    output_dir,
    "auditoria_cientifica.csv"
  )

  write.csv(
    auditoria,
    output_file,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )


  # ----------------------------------------------------------
  # 17. RESUMEN
  # ----------------------------------------------------------

  n_pass <- sum(
    auditoria$estado == "PASS"
  )

  n_review <- sum(
    auditoria$estado == "REVIEW"
  )

  n_block <- sum(
    auditoria$estado == "BLOCK"
  )


  cat("\n")
  cat("============================================================\n")
  cat("AUDITORÍA CIENTÍFICA\n")
  cat("============================================================\n")
  cat("PASS:  ", n_pass, "\n")
  cat("REVIEW:", n_review, "\n")
  cat("BLOCK: ", n_block, "\n")
  cat("------------------------------------------------------------\n")


  if (n_block > 0) {

    cat("ESTADO GENERAL: BLOCK\n")
    cat(
      "El artículo NO puede considerarse SUBMISSION READY.\n"
    )

  } else if (n_review > 0) {

    cat("ESTADO GENERAL: REVIEW\n")
    cat(
      "No existen bloqueos críticos, pero quedan revisiones pendientes.\n"
    )

  } else {

    cat("ESTADO GENERAL: PASS\n")
    cat(
      "Los controles implementados han sido superados.\n"
    )
  }


  cat("------------------------------------------------------------\n")
  cat(
    "Resultado guardado en:\n",
    output_file,
    "\n"
  )
  cat("============================================================\n\n")


  invisible(auditoria)
}


# ============================================================
# EJECUCIÓN DIRECTA
# ============================================================

if (sys.nframe() == 0) {
  auditar_articulo(".")
}