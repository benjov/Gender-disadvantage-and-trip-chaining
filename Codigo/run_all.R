# ============================================================
# run_all.R — Ejecuta el pipeline completo en orden
# Requiere: datos crudos en Data/ (ver README) y paquetes:
#   data.table, survey, readxl, ggplot2, sf, MASS, logistf,
#   scales, stargazer
# ============================================================
t0 <- Sys.time()
source("Script_0.R")   # EOD 2017 -> base analítica
source("Script_1.R")   # Índice de desventaja, modelos M1-M7, tablas, figuras ES
source("Script_2.R")   # Robustez versión de revista + figuras EN
cat("\nPipeline completo en", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutos\n")
