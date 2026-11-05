# 4. Resultados

base_path <- "C:/Users/LENOVO/Documents/land-rental-productivity-global"
d <- readRDS(file.path(base_path, "data_raw/base_arrendamiento_unificada.rds"))
out_dir <- file.path(base_path, "capitulos de libro")
dir.create(file.path(out_dir, "tablas_r"), showWarnings = FALSE)
dir.create(file.path(out_dir, "graficos_r"), showWarnings = FALSE)

# 4.1 Estadística descriptiva
desc_vars <- c("rental_area_share", "small_holdings_share_lt2", "average_holding_area_ha",
               "log_gdp_per_capita", "agriculture_value_added_pct_gdp", "tfp_growth_post5")
desc_list <- lapply(desc_vars, function(v){
  x <- d[[v]]; x <- x[!is.na(x)]
  data.frame(variable=v, n=length(x), media=mean(x), mediana=median(x), sd=sd(x),
             min=min(x), p25=quantile(x,0.25), p75=quantile(x,0.75), max=max(x))
})
desc <- do.call(rbind, desc_list)
write.csv(desc, file.path(out_dir, "tablas_r", "tabla1_descriptivos_r.csv"), row.names=FALSE)
print(desc)

# 4.2 Distribución del arrendamiento
png(file.path(out_dir, "graficos_r", "figura1_distribucion_arrendamiento_r.png"), width=800, height=600)
hist(d$rental_area_share, main="Distribución de la participación de tierra arrendada a terceros",
     xlab="Área arrendada a terceros (%)", ylab="Frecuencia", col="lightblue", border="black")
dev.off()

png(file.path(out_dir, "graficos_r", "figura2_tamano_vs_arrendamiento_r.png"), width=800, height=600)
plot(d$average_holding_area_ha, d$rental_area_share, main="Tamaño medio vs participación de tierra arrendada",
     xlab="Tamaño medio de explotación (ha)", ylab="Área arrendada a terceros (%)", pch=19)
abline(lm(rental_area_share ~ average_holding_area_ha, data=d), col="red")
dev.off()

png(file.path(out_dir, "graficos_r", "figura6_tamano_medio_hist_r.png"), width=800, height=600)
hist(d$average_holding_area_ha, main="Distribución del tamaño medio de explotación",
     xlab="Tamaño medio (ha)", ylab="Frecuencia", col="lightgreen", border="black")
dev.off()

png(file.path(out_dir, "graficos_r", "figura7_pequenas_hist_r.png"), width=800, height=600)
hist(d$small_holdings_share_lt2, main="Distribución de explotaciones menores a 2 hectáreas",
     xlab="Explotaciones < 2 ha (%)", ylab="Frecuencia", col="orange", border="black")
dev.off()

# 4.3 Estructura agraria y desarrollo
png(file.path(out_dir, "graficos_r", "figura8_arrend_vs_pequenas_r.png"), width=800, height=600)
plot(d$rental_area_share, d$small_holdings_share_lt2, main="Arrendamiento vs explotaciones menores a 2 ha",
     xlab="Área arrendada a terceros (%)", ylab="Explotaciones < 2 ha (%)", pch=19)
abline(lm(small_holdings_share_lt2 ~ rental_area_share, data=d), col="red")
dev.off()

png(file.path(out_dir, "graficos_r", "figura9_pib_vs_arrend_r.png"), width=800, height=600)
plot(d$log_gdp_per_capita, d$rental_area_share, main="PIB per cápita vs participación de tierra arrendada",
     xlab="Log PIB per cápita", ylab="Área arrendada a terceros (%)", pch=19)
abline(lm(rental_area_share ~ log_gdp_per_capita, data=d), col="red")
dev.off()

# 4.4 Arrendamiento y TFP
png(file.path(out_dir, "graficos_r", "figura3_arrendamiento_vs_tfp_post5_r.png"), width=800, height=600)
plot(d$rental_area_share, d$tfp_growth_post5, main="Arrendamiento vs crecimiento TFP a 5 años post-censo",
     xlab="Área arrendada a terceros (%)", ylab="Crecimiento TFP anualizado post5", pch=19)
abline(lm(tfp_growth_post5 ~ rental_area_share, data=d), col="red")
dev.off()

# 4.5 Correlaciones
num_vars <- c("rental_area_share","small_holdings_share_lt2","average_holding_area_ha",
              "log_gdp_per_capita","agriculture_value_added_pct_gdp","tfp_growth_post5")
cor_mat <- cor(d[,num_vars], use="pairwise.complete.obs")
write.csv(cor_mat, file.path(out_dir, "tablas_r", "tabla2_correlaciones_r.csv"))
png(file.path(out_dir, "graficos_r", "figura4_matriz_correlaciones_r.png"), width=800, height=700)
pairs(d[,num_vars], main="Matriz de correlaciones")
dev.off()

# 4.6 ANOVA
d$quart_rental <- cut(d$rental_area_share, breaks=quantile(d$rental_area_share, probs=c(0,0.25,0.5,0.75,1), na.rm=TRUE), include.lowest=TRUE, labels=c("Q1","Q2","Q3","Q4"))
aov_res <- aov(tfp_growth_post5 ~ quart_rental, data=d)
sink(file.path(out_dir, "tablas_r", "anova_cuartiles_r.txt"))
print(summary(aov_res))
print(TukeyHSD(aov_res))
sink()
png(file.path(out_dir, "graficos_r", "figura5_anova_cuartiles_r.png"), width=800, height=600)
boxplot(tfp_growth_post5 ~ quart_rental, data=d, main="TFP post5 según cuartiles de arrendamiento",
        xlab="Cuartil de arrendamiento", ylab="Crecimiento TFP anualizado", col="lightgray")
dev.off()

# 4.7 Modelo MCO
m1 <- lm(tfp_growth_post5 ~ rental_area_share + average_holding_area_ha + log_gdp_per_capita + agriculture_value_added_pct_gdp, data=d)
sink(file.path(out_dir, "tablas_r", "modelo_mco_r.txt"))
print(summary(m1))
sink()
