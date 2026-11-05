# ============================================================
# AUDITOR DEL MANUSCRITO Y BIBLIOGRAFÍA
# ============================================================

auditar_manuscrito <- function(project_dir = ".") {

  project_dir <- normalizePath(
    project_dir,
    winslash = "/",
    mustWork = TRUE
  )

  manuscript_file <- file.path(project_dir, "index.qmd")
  bib_file <- file.path(project_dir, "references.bib")

  agregar_control <- function(area, control, estado, detalle) {
    data.frame(
      area = area,
      control = control,
      estado = estado,
      detalle = detalle,
      stringsAsFactors = FALSE
    )
  }

  contar_palabras <- function(x) {

    texto <- paste(x, collapse = " ")
    texto <- trimws(texto)

    if (!nzchar(texto)) {
      return(0L)
    }

    length(
      strsplit(
        texto,
        "\\s+"
      )[[1]]
    )
  }

  extraer_seccion <- function(lineas, nombre) {

    patron <- paste0(
      "^#+\\s+.*",
      nombre,
      ".*$"
    )

    inicio <- which(
      grepl(
        patron,
        lineas,
        ignore.case = TRUE
      )
    )

    if (length(inicio) == 0) {
      return(NULL)
    }

    inicio <- inicio[1]

    siguientes <- which(
      seq_along(lineas) > inicio &
        grepl("^#+\\s+", lineas)
    )

    if (length(siguientes) == 0) {
      fin <- length(lineas)
    } else {
      fin <- siguientes[1] - 1
    }

    if (fin <= inicio) {
      return(character())
    }

    contenido <- lineas[
      (inicio + 1):fin
    ]

    contenido <- contenido[
      nzchar(trimws(contenido))
    ]

    contenido
  }

  controles <- list()

  # ----------------------------------------------------------
  # 1. MANUSCRITO
  # ----------------------------------------------------------

  if (!file.exists(manuscript_file)) {

    controles[[length(controles) + 1]] <-
      agregar_control(
        "Manuscrito",
        "index.qmd existe",
        "BLOCK",
        "No existe index.qmd."
      )

    manuscript <- character()

  } else {

    manuscript <- readLines(
      manuscript_file,
      warn = FALSE,
      encoding = "UTF-8"
    )

    controles[[length(controles) + 1]] <-
      agregar_control(
        "Manuscrito",
        "index.qmd existe",
        "PASS",
        "Archivo localizado."
      )
  }

  # ----------------------------------------------------------
  # 2. BIBLIOGRAFÍA
  # ----------------------------------------------------------

  bib_exists <- file.exists(bib_file)

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Bibliografía",
      "references.bib existe",
      if (bib_exists) "PASS" else "BLOCK",
      if (bib_exists) {
        "Archivo localizado."
      } else {
        "No existe references.bib."
      }
    )

  if (bib_exists) {

    bib_lines <- readLines(
      bib_file,
      warn = FALSE,
      encoding = "UTF-8"
    )

    bib_nonempty <- any(
      nzchar(trimws(bib_lines))
    )

  } else {

    bib_lines <- character()
    bib_nonempty <- FALSE
  }

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Bibliografía",
      "Bibliografía no vacía",
      if (bib_nonempty) "PASS" else "BLOCK",
      if (bib_nonempty) {
        "references.bib contiene registros."
      } else {
        "references.bib está vacío."
      }
    )

  # ----------------------------------------------------------
  # 3. CLAVES BIBTEX
  # ----------------------------------------------------------

  bib_keys <- character()

  if (length(bib_lines) > 0) {

    entradas <- grep(
      "^\\s*@[A-Za-z]+\\s*\\{",
      bib_lines,
      value = TRUE
    )

    if (length(entradas) > 0) {

      bib_keys <- sub(
        "^\\s*@[A-Za-z]+\\s*\\{\\s*([^,]+),.*$",
        "\\1",
        entradas
      )

      bib_keys <- unique(
        trimws(bib_keys)
      )
    }
  }

  # ----------------------------------------------------------
  # 4. CITAS DEL MANUSCRITO
  # ----------------------------------------------------------

  texto <- paste(
    manuscript,
    collapse = "\n"
  )

  citas <- regmatches(
    texto,
    gregexpr(
      "@[A-Za-z0-9_:\\-]+",
      texto,
      perl = TRUE
    )
  )[[1]]

  if (
    length(citas) == 1 &&
    identical(citas, "")
  ) {
    citas <- character()
  }

  citas <- unique(
    sub("^@", "", citas)
  )

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Bibliografía",
      "El manuscrito contiene citas",
      if (length(citas) > 0) "PASS" else "REVIEW",
      if (length(citas) > 0) {
        paste(length(citas), "claves de citación detectadas.")
      } else {
        "No se detectaron citas en index.qmd."
      }
    )

  # ----------------------------------------------------------
  # 5. CITAS SIN REFERENCIA
  # ----------------------------------------------------------

  citas_faltantes <- setdiff(
    citas,
    bib_keys
  )

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Bibliografía",
      "Todas las citas existen en references.bib",
      if (length(citas_faltantes) == 0) "PASS" else "BLOCK",
      if (length(citas_faltantes) == 0) {
        "No se detectaron citas sin referencia."
      } else {
        paste(
          "Faltan:",
          paste(citas_faltantes, collapse = ", ")
        )
      }
    )

  # ----------------------------------------------------------
  # 6. REFERENCIAS NO CITADAS
  # ----------------------------------------------------------

  refs_no_usadas <- setdiff(
    bib_keys,
    citas
  )

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Bibliografía",
      "Referencias utilizadas",
      if (length(refs_no_usadas) == 0) "PASS" else "REVIEW",
      if (length(refs_no_usadas) == 0) {
        "Todas las referencias BibTeX están citadas."
      } else {
        paste(
          length(refs_no_usadas),
          "referencias no están citadas."
        )
      }
    )

  # ----------------------------------------------------------
  # 7. CONTENIDO REAL DE LAS SECCIONES
  # ----------------------------------------------------------

  secciones <- c(
    "Resumen",
    "Introducción",
    "Revisión de literatura",
    "Datos",
    "Metodología",
    "Resultados",
    "Discusión",
    "Conclusiones"
  )

  for (seccion in secciones) {

    contenido <- extraer_seccion(
      manuscript,
      seccion
    )

    if (is.null(contenido)) {

      estado <- "BLOCK"
      detalle <- paste(
        "No se encontró la sección",
        seccion
      )

    } else {

      n_palabras <- contar_palabras(
        contenido
      )

      if (n_palabras == 0) {

        estado <- "BLOCK"
        detalle <- "La sección existe pero está vacía."

      } else if (n_palabras < 20) {

        estado <- "REVIEW"
        detalle <- paste(
          "Contenido muy breve:",
          n_palabras,
          "palabras."
        )

      } else {

        estado <- "PASS"
        detalle <- paste(
          n_palabras,
          "palabras detectadas."
        )
      }
    }

    controles[[length(controles) + 1]] <-
      agregar_control(
        "Estructura",
        paste("Contenido:", seccion),
        estado,
        detalle
      )
  }

  # ----------------------------------------------------------
  # 8. PALABRAS CLAVE
  # ----------------------------------------------------------

  linea_keywords <- grep(
    "palabras clave\\s*:|keywords\\s*:",
    manuscript,
    value = TRUE,
    ignore.case = TRUE
  )

  if (length(linea_keywords) == 0) {

    keyword_status <- "REVIEW"
    keyword_detail <- "No se encontró el campo de palabras clave."

  } else {

    contenido_keywords <- sub(
      "^.*?:",
      "",
      linea_keywords[1]
    )

    contenido_keywords <- trimws(
      gsub("\\*+", "", contenido_keywords)
    )

    if (!nzchar(contenido_keywords)) {

      keyword_status <- "REVIEW"
      keyword_detail <- "El campo de palabras clave existe pero está vacío."

    } else {

      keywords <- unlist(
        strsplit(
          contenido_keywords,
          "[,;]"
        )
      )

      keywords <- trimws(
        keywords
      )

      keywords <- keywords[
        nzchar(keywords)
      ]

      if (length(keywords) >= 3) {

        keyword_status <- "PASS"
        keyword_detail <- paste(
          length(keywords),
          "palabras clave detectadas."
        )

      } else {

        keyword_status <- "REVIEW"
        keyword_detail <- paste(
          "Solo",
          length(keywords),
          "palabras clave detectadas."
        )
      }
    }
  }

  controles[[length(controles) + 1]] <-
    agregar_control(
      "Estructura",
      "Palabras clave",
      keyword_status,
      keyword_detail
    )

  # ----------------------------------------------------------
  # 9. CONSOLIDAR
  # ----------------------------------------------------------

  auditoria <- do.call(
    rbind,
    controles
  )

  auditoria$estado <- factor(
    auditoria$estado,
    levels = c(
      "BLOCK",
      "REVIEW",
      "PASS"
    )
  )

  auditoria <- auditoria[
    order(auditoria$estado),
  ]

  auditoria$estado <- as.character(
    auditoria$estado
  )

  # ----------------------------------------------------------
  # 10. GUARDAR RESULTADOS
  # ----------------------------------------------------------

  output_dir <- file.path(
    project_dir,
    "05_results"
  )

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  output_file <- file.path(
    output_dir,
    "auditoria_manuscrito.csv"
  )

  write.csv(
    auditoria,
    output_file,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  # ----------------------------------------------------------
  # 11. RESUMEN
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
  cat("AUDITORÍA DEL MANUSCRITO\n")
  cat("============================================================\n")
  cat("PASS:  ", n_pass, "\n")
  cat("REVIEW:", n_review, "\n")
  cat("BLOCK: ", n_block, "\n")
  cat("------------------------------------------------------------\n")

  if (n_block > 0) {

    cat("ESTADO GENERAL: BLOCK\n")

  } else if (n_review > 0) {

    cat("ESTADO GENERAL: REVIEW\n")

  } else {

    cat("ESTADO GENERAL: PASS\n")
  }

  cat("------------------------------------------------------------\n")
  cat("Resultado guardado en:\n")
  cat(output_file, "\n")
  cat("============================================================\n\n")

  invisible(auditoria)
}


if (sys.nframe() == 0) {
  auditar_manuscrito(".")
}