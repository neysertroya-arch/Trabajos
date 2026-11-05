# 3. Metodología

# 3.1 Fuentes de información
# FAO World Census of Agriculture (WCAD), USDA ERS International Agricultural Productivity (IAP),
# World Bank World Development Indicators (WDI).

# 3.2 Formación de la base unificada
base_path <- "C:/Users/LENOVO/Documents/land-rental-productivity-global"
d <- readRDS(file.path(base_path, "data_raw/base_arrendamiento_unificada.rds"))

# Variables utilizadas:
# rental_area_share, small_holdings_share_lt2, average_holding_area_ha,
# log_gdp_per_capita, agriculture_value_added_pct_gdp, tfp_growth_post5

# 3.3 Técnicas de análisis
# Estadística descriptiva, diagramas de dispersión, correlaciones de Pearson,
# ANOVA por cuartiles de rental_area_share, modelo lineal por MCO
