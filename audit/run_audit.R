# ============================================================
# AUDITORÍA INTEGRAL — LAND RENTAL / ERAE
# ============================================================

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Falta el paquete 'yaml'.")
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Falta el paquete 'jsonlite'.")
}

# ------------------------------------------------------------
# 1. CONFIGURACIÓN
# ------------------------------------------------------------

config <- yaml::read_yaml(
  "audit/audit_config.yml"
)

manuscript_file <- config$manuscript
bib_file <- config$bibliography
results_dir <- config$results_dir

dir.create(
  results_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

agregar <- function(area, control, estado, detalle) {
  data.frame(
    area = area,
    control = control,
    estado = estado,
    detalle = detalle,
    stringsAsFactors = FALSE
  )
}

controles <- list()

# ------------------------------------------------------------
# 2. ARCHIVOS PRINCIPALES
# ------------------------------------------------------------

for (x in list(
  c("Manuscrito", manuscript_file),
  c("Bibliografía", bib_file),
  c("Protocolo", "01_protocol/registro_cientifico.yml")
)) {

  existe <- file.exists(x[2])

  controles[[length(controles) + 1]] <- agregar(
    "Infraestructura",
    paste(x[1], "disponible"),
    if (existe) "PASS" else "BLOCK",
    x[2]
  )
}

if (!file.exists(manuscript_file)) {
  stop("No se encontró el manuscrito configurado.")
}

if (!file.exists(bib_file)) {
  stop("No se encontró la bibliografía configurada.")
}

# ------------------------------------------------------------
# 3. LEER MANUSCRITO
# ------------------------------------------------------------

manuscript <- readLines(
  manuscript_file,
  warn = FALSE,
  encoding = "UTF-8"
)

texto <- paste(
  manuscript,
  collapse = "\n"
)

# ------------------------------------------------------------
# 4. ESTRUCTURA REAL DEL MANUSCRITO ERAE
# ------------------------------------------------------------

secciones <- c(
  "Introduction",
  "Materials and methods",
  "Results",
  "Discussion and policy implications",
  "Conclusion",
  "Data availability",
  "References"
)

extraer_seccion <- function(lineas, nombre) {

  patron <- paste0(
    "^#\\s+[0-9\\.]*\\s*",
    gsub(
      "([\\W])",
      "\\\\\\1",
      nombre,
      perl = TRUE
    ),
    "\\s*$"
  )

  if (nombre %in% c(
    "Data availability",
    "References"
  )) {
    patron <- paste0(
      "^#\\s+",
      nombre,
      "\\s*$"
    )
  }

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
      grepl("^#\\s+", lineas)
  )

  fin <- if (
    length(siguientes) == 0
  ) {
    length(lineas)
  } else {
    siguientes[1] - 1
  }

  contenido <- lineas[
    (inicio + 1):fin
  ]

  contenido[
    nzchar(
      trimws(contenido)
    )
  ]
}

for (seccion in secciones) {

  contenido <- extraer_seccion(
    manuscript,
    seccion
  )

  if (is.null(contenido)) {

    estado <- "BLOCK"
    detalle <- "Sección no localizada."

  } else {

    palabras <- unlist(
      strsplit(
        paste(
          contenido,
          collapse = " "
        ),
        "\\s+"
      )
    )

    palabras <- palabras[
      nzchar(palabras)
    ]

    n <- length(palabras)

    if (
      seccion == "References"
    ) {

      estado <- "PASS"
      detalle <- "Encabezado localizado; bibliografía auditada por separado."

    } else if (
      n == 0
    ) {

      estado <- "BLOCK"
      detalle <- "La sección está vacía."

    } else if (
      n < 20
    ) {

      estado <- "REVIEW"
      detalle <- paste(
        n,
        "palabras."
      )

    } else {

      estado <- "PASS"
      detalle <- paste(
        n,
        "palabras."
      )
    }
  }

  controles[[length(controles) + 1]] <- agregar(
    "Manuscrito",
    seccion,
    estado,
    detalle
  )
}

# ------------------------------------------------------------
# 5. ABSTRACT Y KEYWORDS EN YAML
# ------------------------------------------------------------

abstract_ok <- grepl(
  "(?m)^abstract:\\s*\\|",
  texto,
  perl = TRUE
)

keywords_ok <- grepl(
  "(?m)^keywords:\\s*\\[.+\\]",
  texto,
  perl = TRUE
)

controles[[length(controles) + 1]] <- agregar(
  "Manuscrito",
  "Abstract declarado",
  if (abstract_ok) "PASS" else "BLOCK",
  if (abstract_ok) {
    "Abstract localizado en YAML."
  } else {
    "No se detectó abstract en YAML."
  }
)

controles[[length(controles) + 1]] <- agregar(
  "Manuscrito",
  "Keywords declaradas",
  if (keywords_ok) "PASS" else "REVIEW",
  if (keywords_ok) {
    "Keywords localizadas en YAML."
  } else {
    "Revisar keywords."
  }
)

# ------------------------------------------------------------
# 6. CITAS DEL MANUSCRITO
# ------------------------------------------------------------

citas <- regmatches(
  texto,
  gregexpr(
    "@[A-Za-z0-9_:\\.-]+",
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

# ------------------------------------------------------------
# 7. CLAVES BIBTEX
# ------------------------------------------------------------

bib <- readLines(
  bib_file,
  warn = FALSE,
  encoding = "UTF-8"
)

entradas <- grep(
  "^\\s*@[A-Za-z]+\\s*\\{",
  bib,
  value = TRUE
)

bib_keys <- sub(
  "^\\s*@[A-Za-z]+\\s*\\{\\s*([^,]+),.*$",
  "\\1",
  entradas
)

bib_keys <- unique(
  trimws(bib_keys)
)

faltantes <- setdiff(
  citas,
  bib_keys
)

no_usadas <- setdiff(
  bib_keys,
  citas
)

controles[[length(controles) + 1]] <- agregar(
  "Bibliografía",
  "Citas resueltas",
  if (length(faltantes) == 0) "PASS" else "BLOCK",
  if (length(faltantes) == 0) {
    paste(length(citas), "claves citadas y resueltas.")
  } else {
    paste(
      "Faltan:",
      paste(faltantes, collapse = ", ")
    )
  }
)

controles[[length(controles) + 1]] <- agregar(
  "Bibliografía",
  "Referencias no utilizadas",
  if (length(no_usadas) == 0) "PASS" else "REVIEW",
  paste(
    length(no_usadas),
    "referencias no citadas."
  )
)

# ------------------------------------------------------------
# 8. PROTOCOLO CIENTÍFICO
# ------------------------------------------------------------

registro <- yaml::read_yaml(
  "01_protocol/registro_cientifico.yml"
)

campos_criticos <- list(
  "Pregunta" = registro$research$question,
  "GAP" = registro$research$gap,
  "Contribución" = registro$research$contribution,
  "Fuente de datos" = registro$data$source,
  "Unidad de análisis" = registro$data$unit_of_analysis,
  "Método" = registro$method$main_method
)

for (nombre in names(campos_criticos)) {

  valor <- campos_criticos[[nombre]]

  ok <- !is.null(valor) &&
    nzchar(trimws(as.character(valor)))

  controles[[length(controles) + 1]] <- agregar(
    "Integridad científica",
    nombre,
    if (ok) "PASS" else "BLOCK",
    if (ok) {
      as.character(valor)
    } else {
      "No documentado."
    }
  )
}

# ------------------------------------------------------------
# 9. DISEÑO CAUSAL
# ------------------------------------------------------------

causal <- isTRUE(
  registro$method$causal_design
)

controles[[length(controles) + 1]] <- agregar(
  "Metodología",
  "Lenguaje causal",
  if (!causal) "PASS" else "REVIEW",
  if (!causal) {
    "Diseño no causal declarado; mantener interpretación asociativa."
  } else {
    "La identificación causal requiere revisión científica."
  }
)

# ------------------------------------------------------------
# 10. DATOS: VERSIÓN Y PERIODO
# ------------------------------------------------------------

for (campo in c(
  "version",
  "period"
)) {

  valor <- registro$data[[campo]]

  ok <- !is.null(valor) &&
    nzchar(trimws(as.character(valor)))

  controles[[length(controles) + 1]] <- agregar(
    "Datos",
    paste("Datos:", campo),
    if (ok) "PASS" else "REVIEW",
    if (ok) {
      as.character(valor)
    } else {
      "Pendiente de documentar."
    }
  )
}

# ------------------------------------------------------------
# 11. EDITORIAL
# ------------------------------------------------------------

editorial_fields <- c(
  "scope_checked",
  "ai_policy_checked",
  "data_policy_checked",
  "code_policy_checked",
  "blind_review_checked",
  "costs_checked",
  "publication_time_checked"
)

for (campo in editorial_fields) {

  ok <- isTRUE(
    registro$editorial[[campo]]
  )

  controles[[length(controles) + 1]] <- agregar(
    "Editorial",
    campo,
    if (ok) "PASS" else "REVIEW",
    if (ok) {
      "Verificación registrada."
    } else {
      "Pendiente de verificación."
    }
  )
}

# ------------------------------------------------------------
# 12. CODIFICACIÓN
# ------------------------------------------------------------

mojibake <- grepl(
  "â€”|â€“|â†’|Ã",
  texto
)

controles[[length(controles) + 1]] <- agregar(
  "Integridad",
  "Codificación UTF-8",
  if (mojibake) "REVIEW" else "PASS",
  if (mojibake) {
    "Se detectaron secuencias compatibles con mojibake; verificar archivo/render."
  } else {
    "No se detectaron patrones evidentes de corrupción de caracteres."
  }
)

# ------------------------------------------------------------
# 13. CONSOLIDAR
# ------------------------------------------------------------

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

write.csv(
  auditoria,
  file.path(
    results_dir,
    "auditoria_land_rental.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

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
cat("AUDITORÍA — LAND RENTAL / ERAE\n")
cat("============================================================\n")
cat("PASS:  ", n_pass, "\n")
cat("REVIEW:", n_review, "\n")
cat("BLOCK: ", n_block, "\n")
cat("------------------------------------------------------------\n")

if (n_block > 0) {

  cat("ESTADO: BLOCK\n")
  cat("SUBMISSION READY: NO\n")

} else if (n_review > 0) {

  cat("ESTADO: REVIEW\n")
  cat("SUBMISSION READY: NO\n")

} else {

  cat("ESTADO: PASS\n")
  cat("CORE AUDIT: PASS\n")
  cat("SUBMISSION READY: PENDIENTE DE AUDITORÍA FINAL\n")
}

cat("------------------------------------------------------------\n")
cat(
  "Reporte: ",
  file.path(
    results_dir,
    "auditoria_land_rental.csv"
  ),
  "\n",
  sep = ""
)
cat("============================================================\n")