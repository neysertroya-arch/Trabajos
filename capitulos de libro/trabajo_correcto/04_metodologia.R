variables <- data.frame(
  Variable = c(
    "Crecimiento de la TFP agrícola",
    "Prevalencia del arrendamiento",
    "Explotaciones pequeñas",
    "Tamaño medio de la explotación",
    "PIB per cápita",
    "Peso de la agricultura"
  ),
  Medición = c(
    "Cambio logarítmico anualizado durante los cinco años posteriores al censo",
    "Porcentaje del área agrícola arrendada a terceros (efecto por 10 pp)",
    "Proporción de explotaciones menores de dos hectáreas",
    "Logaritmo",
    "Logaritmo",
    "Valor agregado agrícola, % del PIB"
  ),
  Fuente = c("USDA ERS", "FAO", "FAO", "FAO", "WDI", "WDI"),
  Rol = c(
    "Dependiente",
    "Independiente de interés",
    "Moderadora (interacción)",
    "Control",
    "Control",
    "Control"
  ),
  check.names = FALSE
)

knitr::kable(
  variables,
  booktabs = TRUE,
  longtable = TRUE,
  align = "llll",
  linesep = ""
)
