# ============================================================
# AUDITOR EXTERNO DE REFERENCIAS
# ============================================================

auditar_referencias_externas <- function(project_dir = ".") {
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Falta 'jsonlite'. Ejecute renv::install('jsonlite').")
  }
  
  project_dir <- normalizePath(
    project_dir,
    winslash = "/",
    mustWork = TRUE
  )
  
  bib_file <- file.path(
    project_dir,
    "references.bib"
  )
  
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
  
  limpiar_texto <- function(x) {
    
    if (is.null(x) || length(x) == 0) {
      return("")
    }
    
    x <- paste(x, collapse = " ")
    x <- tolower(x)
    
    x <- iconv(
      x,
      from = "",
      to = "ASCII//TRANSLIT"
    )
    
    x <- gsub(
      "[^a-z0-9 ]",
      " ",
      x
    )
    
    x <- gsub(
      "\\s+",
      " ",
      x
    )
    
    trimws(x)
  }
  
  similitud_jaccard <- function(a, b) {
    
    a <- limpiar_texto(a)
    b <- limpiar_texto(b)
    
    if (!nzchar(a) || !nzchar(b)) {
      return(NA_real_)
    }
    
    wa <- unique(
      strsplit(a, "\\s+")[[1]]
    )
    
    wb <- unique(
      strsplit(b, "\\s+")[[1]]
    )
    
    union_words <- union(
      wa,
      wb
    )
    
    if (length(union_words) == 0) {
      return(NA_real_)
    }
    
    length(
      intersect(
        wa,
        wb
      )
    ) / length(union_words)
  }
  
  extraer_campo <- function(
    bloque,
    campo
  ) {
    
    lineas <- strsplit(
      bloque,
      "\n",
      fixed = TRUE
    )[[1]]
    
    patron <- paste0(
      "^\\s*",
      campo,
      "\\s*="
    )
    
    linea <- grep(
      patron,
      lineas,
      value = TRUE,
      ignore.case = TRUE
    )
    
    if (length(linea) == 0) {
      return("")
    }
    
    valor <- sub(
      paste0(
        "^\\s*",
        campo,
        "\\s*=\\s*[\\{\"]?"
      ),
      "",
      linea[1],
      ignore.case = TRUE
    )
    
    valor <- sub(
      "[\\}\"],?\\s*$",
      "",
      valor
    )
    
    trimws(valor)
  }
  
  extraer_apellido_local <- function(
    author_field
  ) {
    
    if (!nzchar(author_field)) {
      return("")
    }
    
    author_field <- gsub(
      "\\s+AND\\s+",
      " and ",
      author_field,
      ignore.case = TRUE
    )
    
    autores <- strsplit(
      author_field,
      "\\s+and\\s+"
    )[[1]]
    
    primer_autor <- trimws(
      autores[1]
    )
    
    if (
      grepl(
        ",",
        primer_autor,
        fixed = TRUE
      )
    ) {
      
      apellido <- strsplit(
        primer_autor,
        ",",
        fixed = TRUE
      )[[1]][1]
      
      return(
        limpiar_texto(
          apellido
        )
      )
    }
    
    partes <- strsplit(
      primer_autor,
      "\\s+"
    )[[1]]
    
    limpiar_texto(
      partes[
        length(partes)
      ]
    )
  }
  
  extraer_anio_crossref <- function(
    mensaje
  ) {
    
    candidatos <- list(
      mensaje$published.print,
      mensaje$published.online,
      mensaje$published,
      mensaje$issued,
      mensaje$created
    )
    
    for (obj in candidatos) {
      
      if (
        !is.null(obj) &&
        !is.null(obj$`date-parts`) &&
        length(obj$`date-parts`) > 0
      ) {
        
        return(
          suppressWarnings(
            as.integer(
              obj$`date-parts`[[1]][1]
            )
          )
        )
      }
    }
    
    NA_integer_
  }
  
  controles <- list()
  
  # ----------------------------------------------------------
  # 1. COMPROBAR BIBLIOGRAFÍA
  # ----------------------------------------------------------
  
  if (!file.exists(bib_file)) {
    
    controles[[1]] <- agregar_control(
      "Referencias externas",
      "references.bib existe",
      "BLOCK",
      "No existe references.bib."
    )
    
  } else {
    
    bib <- readLines(
      bib_file,
      warn = FALSE,
      encoding = "UTF-8"
    )
    
    entradas_inicio <- grep(
      "^\\s*@[A-Za-z]+\\s*\\{",
      bib
    )
    
    if (
      length(entradas_inicio) == 0
    ) {
      
      controles[[1]] <- agregar_control(
        "Referencias externas",
        "Referencias disponibles para validación",
        "BLOCK",
        "references.bib no contiene registros."
      )
      
    } else {
      
      # ------------------------------------------------------
      # 2. EXTRAER BLOQUES BIBTEX
      # ------------------------------------------------------
      
      bloques <- character(
        length(entradas_inicio)
      )
      
      for (
        i in seq_along(
          entradas_inicio
        )
      ) {
        
        inicio <- entradas_inicio[i]
        
        fin <- if (
          i < length(
            entradas_inicio
          )
        ) {
          entradas_inicio[i + 1] - 1
        } else {
          length(bib)
        }
        
        bloques[i] <- paste(
          bib[inicio:fin],
          collapse = "\n"
        )
      }
      
      # ------------------------------------------------------
      # 3. EXTRAER CLAVES
      # ------------------------------------------------------
      
      keys <- sub(
        "^\\s*@[A-Za-z]+\\s*\\{\\s*([^,]+),.*$",
        "\\1",
        bib[entradas_inicio]
      )
      
      keys <- trimws(
        keys
      )
      
      # ------------------------------------------------------
      # 4. VALIDAR REFERENCIA POR REFERENCIA
      # ------------------------------------------------------
      
      for (
        i in seq_along(
          bloques
        )
      ) {
        
        key <- keys[i]
        
        doi <- extraer_campo(
          bloques[i],
          "doi"
        )
        
        titulo_local <- extraer_campo(
          bloques[i],
          "title"
        )
        
        autor_local <- extraer_campo(
          bloques[i],
          "author"
        )
        
        anio_local <- suppressWarnings(
          as.integer(
            extraer_campo(
              bloques[i],
              "year"
            )
          )
        )
        
        # ----------------------------------------------------
        # SIN DOI
        # ----------------------------------------------------
        
        if (!nzchar(doi)) {
          
          controles[[
            length(controles) + 1
          ]] <- agregar_control(
            "Referencias externas",
            paste0(
              "Validación externa: ",
              key
            ),
            "REVIEW",
            paste(
              "La referencia no incluye DOI;",
              "no puede validarse automáticamente contra Crossref."
            )
          )
          
          next
        }
        
        # ----------------------------------------------------
        # CONSULTA CROSSREF
        # ----------------------------------------------------
        
        url <- paste0(
          "https://api.crossref.org/works/",
          utils::URLencode(
            doi,
            reserved = TRUE
          )
        )
        
        respuesta <- tryCatch(
          jsonlite::fromJSON(
            url
          ),
          error = function(e) {
            NULL
          }
        )
        
        if (
          is.null(respuesta)
        ) {
          
          controles[[
            length(controles) + 1
          ]] <- agregar_control(
            "Referencias externas",
            paste0(
              "Validación externa: ",
              key
            ),
            "BLOCK",
            paste(
              "No fue posible resolver el DOI:",
              doi
            )
          )
          
          next
        }
        
        mensaje <- respuesta$message
        
        # ----------------------------------------------------
        # TÍTULO CROSSREF
        # ----------------------------------------------------
        
        titulo_crossref <- ""
        
        if (
          !is.null(
            mensaje$title
          ) &&
          length(
            mensaje$title
          ) > 0
        ) {
          
          titulo_crossref <- mensaje$title[1]
        }
        
        # ----------------------------------------------------
        # AUTOR CROSSREF
        # ----------------------------------------------------
        
        apellido_crossref <- ""
        
        if (
          !is.null(
            mensaje$author
          ) &&
          is.data.frame(
            mensaje$author
          ) &&
          nrow(
            mensaje$author
          ) > 0 &&
          "family" %in%
          names(
            mensaje$author
          )
        ) {
          
          apellido_crossref <- limpiar_texto(
            mensaje$author$family[1]
          )
        }
        
        # ----------------------------------------------------
        # AÑO CROSSREF
        # ----------------------------------------------------
        
        anio_crossref <- extraer_anio_crossref(
          mensaje
        )
        
        # ----------------------------------------------------
        # COMPARACIONES
        # ----------------------------------------------------
        
        score_titulo <- similitud_jaccard(
          titulo_local,
          titulo_crossref
        )
        
        apellido_local <- extraer_apellido_local(
          autor_local
        )
        
        autor_ok <- (
          nzchar(
            apellido_local
          ) &&
            nzchar(
              apellido_crossref
            ) &&
            identical(
              apellido_local,
              apellido_crossref
            )
        )
        
        anio_ok <- (
          !is.na(
            anio_local
          ) &&
            !is.na(
              anio_crossref
            ) &&
            anio_local ==
            anio_crossref
        )
        
        titulo_ok <- (
          !is.na(
            score_titulo
          ) &&
            score_titulo >= 0.70
        )
        
        titulo_review <- (
          !is.na(
            score_titulo
          ) &&
            score_titulo >= 0.40 &&
            score_titulo < 0.70
        )
        
        # ----------------------------------------------------
        # DECISIÓN
        # ----------------------------------------------------
        
        if (
          titulo_ok &&
          autor_ok &&
          anio_ok
        ) {
          
          estado <- "PASS"
          
          detalle <- paste(
            "DOI resuelto;",
            "título, primer autor y año coinciden.",
            "Similitud título:",
            round(
              score_titulo,
              2
            )
          )
          
        } else if (
          titulo_review ||
          !autor_ok ||
          !anio_ok
        ) {
          
          estado <- "REVIEW"
          
          detalle <- paste0(
            "DOI resuelto. ",
            "Título=",
            ifelse(
              is.na(
                score_titulo
              ),
              "NA",
              round(
                score_titulo,
                2
              )
            ),
            "; autor=",
            ifelse(
              autor_ok,
              "OK",
              "REVISAR"
            ),
            "; año=",
            ifelse(
              anio_ok,
              "OK",
              "REVISAR"
            ),
            "."
          )
          
        } else {
          
          estado <- "BLOCK"
          
          detalle <- paste0(
            "DOI resuelto, pero la referencia local ",
            "no coincide suficientemente. ",
            "Título=",
            ifelse(
              is.na(
                score_titulo
              ),
              "NA",
              round(
                score_titulo,
                2
              )
            ),
            "; autor=",
            ifelse(
              autor_ok,
              "OK",
              "NO"
            ),
            "; año=",
            ifelse(
              anio_ok,
              "OK",
              "NO"
            ),
            "."
          )
        }
        
        controles[[
          length(controles) + 1
        ]] <- agregar_control(
          "Referencias externas",
          paste0(
            "Validación externa: ",
            key
          ),
          estado,
          detalle
        )
      }
    }
  }
  
  # ----------------------------------------------------------
  # 5. CONSOLIDAR
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
    order(
      auditoria$estado
    ),
  ]
  
  auditoria$estado <- as.character(
    auditoria$estado
  )
  
  # ----------------------------------------------------------
  # 6. GUARDAR
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
    "auditoria_referencias_externas.csv"
  )
  
  write.csv(
    auditoria,
    output_file,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  # ----------------------------------------------------------
  # 7. RESUMEN
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
  cat("AUDITORÍA EXTERNA DE REFERENCIAS\n")
  cat("============================================================\n")
  cat("PASS:  ", n_pass, "\n")
  cat("REVIEW:", n_review, "\n")
  cat("BLOCK: ", n_block, "\n")
  cat("------------------------------------------------------------\n")
  
  if (
    n_block > 0
  ) {
    
    cat(
      "ESTADO GENERAL: BLOCK\n"
    )
    
  } else if (
    n_review > 0
  ) {
    
    cat(
      "ESTADO GENERAL: REVIEW\n"
    )
    
  } else {
    
    cat(
      "ESTADO GENERAL: PASS\n"
    )
  }
  
  cat("------------------------------------------------------------\n")
  cat("Resultado guardado en:\n")
  cat(output_file, "\n")
  cat("============================================================\n\n")
  
  invisible(
    auditoria
  )
}


# ============================================================
# EJECUCIÓN DIRECTA
# ============================================================

if (
  sys.nframe() == 0
) {
  auditar_referencias_externas(".")
}