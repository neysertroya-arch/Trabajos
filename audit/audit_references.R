# ============================================================
# AUDITOR DE REFERENCIAS
# ============================================================

auditar_referencias <- function(project_dir = ".") {
  
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
  
  controles <- list()
  
  # ----------------------------------------------------------
  # 1. EXISTENCIA DE references.bib
  # ----------------------------------------------------------
  
  if (!file.exists(bib_file)) {
    
    controles[[length(controles) + 1]] <-
      agregar_control(
        "Referencias",
        "references.bib existe",
        "BLOCK",
        "No existe references.bib."
      )
    
    auditoria <- do.call(
      rbind,
      controles
    )
    
  } else {
    
    controles[[length(controles) + 1]] <-
      agregar_control(
        "Referencias",
        "references.bib existe",
        "PASS",
        "Archivo localizado."
      )
    
    bib <- readLines(
      bib_file,
      warn = FALSE,
      encoding = "UTF-8"
    )
    
    # --------------------------------------------------------
    # 2. DETECTAR REGISTROS BIBTEX
    # --------------------------------------------------------
    
    entradas_inicio <- grep(
      "^\\s*@[A-Za-z]+\\s*\\{",
      bib
    )
    
    n_refs <- length(
      entradas_inicio
    )
    
    controles[[length(controles) + 1]] <-
      agregar_control(
        "Referencias",
        "Bibliografía contiene registros",
        if (n_refs > 0) "PASS" else "BLOCK",
        paste(
          n_refs,
          "referencias detectadas."
        )
      )
    
    # --------------------------------------------------------
    # 3. SOLO AUDITAR DETALLES SI EXISTEN REFERENCIAS
    # --------------------------------------------------------
    
    if (n_refs > 0) {
      
      # ------------------------------------------------------
      # EXTRAER BLOQUES BIBTEX
      # ------------------------------------------------------
      
      bloques <- character(
        length(entradas_inicio)
      )
      
      for (i in seq_along(entradas_inicio)) {
        
        inicio <- entradas_inicio[i]
        
        fin <- if (
          i < length(entradas_inicio)
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
      # EXTRAER CLAVES
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
      # CLAVES DUPLICADAS
      # ------------------------------------------------------
      
      duplicadas <- unique(
        keys[
          duplicated(keys)
        ]
      )
      
      controles[[length(controles) + 1]] <-
        agregar_control(
          "Referencias",
          "Claves BibTeX únicas",
          if (
            length(duplicadas) == 0
          ) {
            "PASS"
          } else {
            "BLOCK"
          },
          if (
            length(duplicadas) == 0
          ) {
            "No se detectaron claves BibTeX duplicadas."
          } else {
            paste(
              "Claves duplicadas:",
              paste(
                duplicadas,
                collapse = ", "
              )
            )
          }
        )
      
      # ------------------------------------------------------
      # CAMPOS BIBLIOGRÁFICOS MÍNIMOS
      # ------------------------------------------------------
      
      tiene_author <- grepl(
        "\\bauthor\\s*=",
        bloques,
        ignore.case = TRUE
      )
      
      tiene_title <- grepl(
        "\\btitle\\s*=",
        bloques,
        ignore.case = TRUE
      )
      
      tiene_year <- grepl(
        "\\byear\\s*=",
        bloques,
        ignore.case = TRUE
      )
      
      completas <- (
        tiene_author &
          tiene_title &
          tiene_year
      )
      
      incompletas <- keys[
        !completas
      ]
      
      controles[[length(controles) + 1]] <-
        agregar_control(
          "Referencias",
          "Campos bibliográficos mínimos",
          if (
            length(incompletas) == 0
          ) {
            "PASS"
          } else {
            "REVIEW"
          },
          if (
            length(incompletas) == 0
          ) {
            paste(
              "Las",
              n_refs,
              "referencias contienen author, title y year."
            )
          } else {
            paste(
              length(incompletas),
              "referencias requieren revisión de author, title o year."
            )
          }
        )
      
      # ------------------------------------------------------
      # EXTRAER DOI
      # ------------------------------------------------------
      
      doi_values <- rep(
        NA_character_,
        length(bloques)
      )
      
      for (i in seq_along(bloques)) {
        
        lineas_doi <- grep(
          "^\\s*doi\\s*=",
          strsplit(
            bloques[i],
            "\n",
            fixed = TRUE
          )[[1]],
          value = TRUE,
          ignore.case = TRUE
        )
        
        if (
          length(lineas_doi) > 0
        ) {
          
          doi <- sub(
            "^\\s*doi\\s*=\\s*[\\{\"]?",
            "",
            lineas_doi[1],
            ignore.case = TRUE
          )
          
          doi <- sub(
            "[\\}\"],?\\s*$",
            "",
            doi
          )
          
          doi <- trimws(
            doi
          )
          
          doi_values[i] <- doi
        }
      }
      
      # ------------------------------------------------------
      # FORMATO DOI
      # ------------------------------------------------------
      
      doi_presentes <- !is.na(
        doi_values
      ) &
        nzchar(
          doi_values
        )
      
      doi_validos <- rep(
        TRUE,
        length(doi_values)
      )
      
      doi_validos[
        doi_presentes
      ] <- grepl(
        "^10\\.[0-9]{4,9}/\\S+$",
        doi_values[
          doi_presentes
        ],
        perl = TRUE
      )
      
      doi_invalidos <- keys[
        doi_presentes &
          !doi_validos
      ]
      
      controles[[length(controles) + 1]] <-
        agregar_control(
          "Referencias",
          "Formato DOI",
          if (
            length(doi_invalidos) == 0
          ) {
            "PASS"
          } else {
            "BLOCK"
          },
          if (
            length(doi_invalidos) == 0
          ) {
            paste(
              sum(doi_presentes),
              "DOI detectados y ninguno presenta formato inválido."
            )
          } else {
            paste(
              length(doi_invalidos),
              "referencias presentan DOI con formato inválido."
            )
          }
        )
      
      # ------------------------------------------------------
      # DOI DUPLICADOS
      # ------------------------------------------------------
      
      doi_no_na <- doi_values[
        doi_presentes
      ]
      
      doi_no_na_norm <- tolower(
        trimws(
          doi_no_na
        )
      )
      
      doi_duplicados <- unique(
        doi_no_na_norm[
          duplicated(
            doi_no_na_norm
          )
        ]
      )
      
      controles[[length(controles) + 1]] <-
        agregar_control(
          "Referencias",
          "DOI únicos",
          if (
            length(doi_duplicados) == 0
          ) {
            "PASS"
          } else {
            "BLOCK"
          },
          if (
            length(doi_duplicados) == 0
          ) {
            "No se detectaron DOI duplicados."
          } else {
            paste(
              "DOI duplicados:",
              paste(
                doi_duplicados,
                collapse = ", "
              )
            )
          }
        )
      
      # ------------------------------------------------------
      # DOI AUSENTE
      # ------------------------------------------------------
      
      sin_doi <- keys[
        !doi_presentes
      ]
      
      controles[[length(controles) + 1]] <-
        agregar_control(
          "Referencias",
          "DOI documentado cuando corresponde",
          if (
            length(sin_doi) == 0
          ) {
            "PASS"
          } else {
            "REVIEW"
          },
          if (
            length(sin_doi) == 0
          ) {
            "Todas las referencias incluyen DOI."
          } else {
            paste(
              length(sin_doi),
              "referencias no incluyen DOI. Debe verificarse si realmente tienen uno."
            )
          }
        )
      
      # ------------------------------------------------------
      # RESUMEN DE REFERENCIAS
      # ------------------------------------------------------
      
      controles[[length(controles) + 1]] <-
        agregar_control(
          "Referencias",
          "Total de referencias auditadas",
          "PASS",
          paste(
            n_refs,
            "referencias procesadas."
          )
        )
      
    } else {
      
      # ------------------------------------------------------
      # references.bib EXISTE PERO ESTÁ VACÍO
      # ------------------------------------------------------
      
      controles[[length(controles) + 1]] <-
        agregar_control(
          "Referencias",
          "Auditoría bibliográfica detallada",
          "BLOCK",
          paste(
            "No puede ejecutarse la auditoría detallada",
            "porque references.bib no contiene registros."
          )
        )
    }
    
    auditoria <- do.call(
      rbind,
      controles
    )
  }
  
  # ----------------------------------------------------------
  # 4. ORDENAR ESTADOS
  # ----------------------------------------------------------
  
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
  # 5. GUARDAR RESULTADO
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
    "auditoria_referencias.csv"
  )
  
  write.csv(
    auditoria,
    output_file,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  # ----------------------------------------------------------
  # 6. RESUMEN
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
  cat("AUDITORÍA DE REFERENCIAS\n")
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
  
  invisible(
    auditoria
  )
}


# ============================================================
# EJECUCIÓN DIRECTA
# ============================================================

if (sys.nframe() == 0) {
  auditar_referencias(".")
}