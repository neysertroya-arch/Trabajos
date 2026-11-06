# Capítulo: Correlaciones y estadísticos
# Versión en script R de 06_correlaciones_estadisticos.qmd
# Genera las tablas y las figuras a partir de la base unificada.

encontrar_base <- function() {
  candidatos <- c(
    file.path("..", "..", "data_raw", "base_arrendamiento_unificada.rds"),
    file.path("..", "data_raw", "base_arrendamiento_unificada.rds"),
    file.path("data_raw", "base_arrendamiento_unificada.rds"),
    "C:/Users/LENOVO/Documents/land-rental-productivity-global/data_raw/base_arrendamiento_unificada.rds"
  )
  for (p in candidatos) {
    if (file.exists(p)) return(normalizePath(p))
  }
  stop("No se encontró base_arrendamiento_unificada.rds")
}

d <- readRDS(encontrar_base())
if (any(!is.finite(d$average_holding_area_ha) | d$average_holding_area_ha <= 0, na.rm = TRUE)) {
  stop("El tamaÃ±o medio debe ser positivo y finito para calcular su logaritmo.")
}
d$log_average_holding_area <- log(d$average_holding_area_ha)

dir.create("salidas/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/figuras", recursive = TRUE, showWarnings = FALSE)

br_rental <- c(0, 4, 10, 20, 30, 40, 50, 60, 70, 71)
h_rental <- hist(d$rental_area_share, breaks = br_rental, plot = FALSE)

cor_vars <- c(
  "rental_area_share",
  "small_holdings_share_lt2",
  "average_holding_area_ha",
  "log_gdp_per_capita",
  "agriculture_value_added_pct_gdp",
  "tfp_growth_post5"
)

# ---------------------------------------------------------------------------
# Estadística descriptiva
# ---------------------------------------------------------------------------
desc_vars <- c(
  "rental_area_share",
  "small_holdings_share_lt2",
  "average_holding_area_ha",
  "log_gdp_per_capita",
  "agriculture_value_added_pct_gdp",
  "tfp_growth_post5",
  "tfp_growth_post3",
  "tfp_growth_pre5"
)

desc <- do.call(rbind, lapply(desc_vars, function(v) {
  x <- d[[v]]
  x <- x[!is.na(x)]
  data.frame(
    variable = v,
    n = length(x),
    media = mean(x),
    mediana = median(x),
    sd = sd(x),
    min = min(x),
    p25 = quantile(x, 0.25, names = FALSE),
    p75 = quantile(x, 0.75, names = FALSE),
    max = max(x)
  )
}))

desc[] <- lapply(desc, function(x) if (is.numeric(x)) round(x, 4) else x)
write.csv(desc, "salidas/tablas/tabla4_descriptivos.csv", row.names = FALSE)
print(desc)

# ---------------------------------------------------------------------------
# Figura 1: distribución del arrendamiento
# ---------------------------------------------------------------------------
png("salidas/figuras/figura1_distribucion_arrendamiento.png", width = 1600, height = 1000, res = 200)
par(mar = c(4.5, 4.5, 2, 1))
hist(
  d$rental_area_share,
  breaks = br_rental,
  main = "",
  xlab = "Área arrendada a terceros (%)",
  ylab = "Frecuencia",
  col = "lightblue",
  border = "grey35"
)
abline(v = mean(d$rental_area_share, na.rm = TRUE), col = "#c0392b", lwd = 2)
abline(v = median(d$rental_area_share, na.rm = TRUE), col = "#2471a3", lwd = 2, lty = 2)
legend("topright", legend = c("Media", "Mediana"),
       col = c("#c0392b", "#2471a3"), lwd = 2, lty = c(1, 2), bty = "n")
dev.off()

# ---------------------------------------------------------------------------
# Figura 2: tamaño medio vs arrendamiento
# ---------------------------------------------------------------------------
x <- d$average_holding_area_ha
y <- d$rental_area_share
ok <- complete.cases(x, y)
x <- x[ok]
y <- y[ok]

ajuste <- lm(y ~ x)
nuevo <- data.frame(x = seq(min(x), max(x), length.out = 200))
pred <- predict(ajuste, newdata = nuevo, interval = "confidence")

png("salidas/figuras/figura2_tamano_vs_arrendamiento.png", width = 1600, height = 1000, res = 200)
plot(
  x, y,
  log = "x",
  pch = 19,
  col = adjustcolor("#2471a3", alpha.f = 0.65),
  xlab = "Tamaño medio de la explotación (ha, escala logarítmica)",
  ylab = "Área arrendada a terceros (%)",
  main = ""
)
polygon(
  c(nuevo$x, rev(nuevo$x)),
  c(pred[, "lwr"], rev(pred[, "upr"])),
  col = adjustcolor("grey50", alpha.f = 0.25),
  border = NA
)
lines(nuevo$x, pred[, "fit"], col = "#c0392b", lwd = 2)
legend("topright", legend = sprintf("r = %.2f", cor(x, y)), bty = "n")
dev.off()

# ---------------------------------------------------------------------------
# Figura 3: arrendamiento vs crecimiento de la TFP
# ---------------------------------------------------------------------------
x <- d$rental_area_share
y <- d$tfp_growth_post5
ok <- complete.cases(x, y)
x <- x[ok]
y <- y[ok]

ajuste <- lm(y ~ x)
nuevo <- data.frame(x = seq(min(x), max(x), length.out = 200))
pred <- predict(ajuste, newdata = nuevo, interval = "confidence")

png("salidas/figuras/figura3_arrendamiento_vs_tfp.png", width = 1600, height = 1000, res = 200)
plot(
  x, y,
  pch = 19,
  col = adjustcolor("#2471a3", alpha.f = 0.65),
  xlab = "Área arrendada a terceros (%)",
  ylab = "Crecimiento anualizado de la TFP",
  main = ""
)
polygon(
  c(nuevo$x, rev(nuevo$x)),
  c(pred[, "lwr"], rev(pred[, "upr"])),
  col = adjustcolor("grey50", alpha.f = 0.25),
  border = NA
)
lines(nuevo$x, pred[, "fit"], col = "#c0392b", lwd = 2)
abline(h = 0, col = "grey80", lty = 2)
legend("topright", legend = sprintf("r = %.2f", cor(x, y)), bty = "n")
dev.off()

# ---------------------------------------------------------------------------
# Tabla 5: correlaciones
# ---------------------------------------------------------------------------
cor_mat <- cor(d[, cor_vars], use = "pairwise.complete.obs")
cor_df <- data.frame(Variable = rownames(cor_mat), round(cor_mat, 3), check.names = FALSE)
rownames(cor_df) <- NULL
write.csv(cor_df, "salidas/tablas/tabla5_correlaciones.csv", row.names = FALSE)
print(cor_df)

# ---------------------------------------------------------------------------
# Figura 4: matriz de dispersión
# ---------------------------------------------------------------------------
panel_cor <- function(x, y, digits = 2, ...) {
  usr <- par("usr")
  on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")
  text(0.5, 0.5, format(round(r, digits), nsmall = digits), cex = 1.1)
}

png("salidas/figuras/figura4_matriz_correlaciones.png", width = 1600, height = 1400, res = 200)
pairs(
  d[, cor_vars],
  upper.panel = panel_cor,
  pch = 19,
  col = adjustcolor("black", alpha.f = 0.5),
  cex = 0.7
)
dev.off()

# ---------------------------------------------------------------------------
# ANOVA por cuartiles
# ---------------------------------------------------------------------------
d$quart_rental <- cut(
  d$rental_area_share,
  breaks = quantile(d$rental_area_share, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Q1", "Q2", "Q3", "Q4")
)

png("salidas/figuras/figura5_anova_cuartiles.png", width = 1600, height = 1000, res = 200)
boxplot(
  tfp_growth_post5 ~ quart_rental,
  data = d,
  main = "",
  xlab = "Cuartil de arrendamiento",
  ylab = "Crecimiento anualizado de la TFP",
  col = "lightgray",
  border = "grey35"
)
points(
  seq_len(nlevels(d$quart_rental)),
  tapply(d$tfp_growth_post5, d$quart_rental, median, na.rm = TRUE),
  pch = 23,
  bg = "#c0392b",
  col = "#c0392b",
  cex = 1.2
)
legend("topright", legend = "Mediana", pch = 23, pt.bg = "#c0392b",
       col = "#c0392b", bty = "n")
dev.off()

anova_res <- aov(tfp_growth_post5 ~ quart_rental, data = d)
anova_sum <- summary(anova_res)[[1]]

anova_tab <- data.frame(
  Fuente = c("Cuartiles de arrendamiento", "Residuos"),
  Df = anova_sum$Df,
  `Suma de cuadrados` = anova_sum$`Sum Sq`,
  `Media cuadrática` = anova_sum$`Mean Sq`,
  `F` = anova_sum$`F value`,
  `p` = anova_sum$`Pr(>F)`,
  check.names = FALSE
)
anova_tab[] <- lapply(anova_tab, function(x) if (is.numeric(x)) round(x, 4) else x)
write.csv(anova_tab, "salidas/tablas/tabla6_anova.csv", row.names = FALSE)
print(anova_tab)

tukey <- TukeyHSD(anova_res)$quart_rental
tukey_tab <- data.frame(
  Comparación = rownames(tukey),
  Diferencia = tukey[, "diff"],
  `IC 95% inferior` = tukey[, "lwr"],
  `IC 95% superior` = tukey[, "upr"],
  `p ajustado` = tukey[, "p adj"],
  check.names = FALSE
)
tukey_tab[] <- lapply(tukey_tab, function(x) if (is.numeric(x)) round(x, 4) else x)
write.csv(tukey_tab, "salidas/tablas/tabla7_tukey.csv", row.names = FALSE)
print(tukey_tab)

