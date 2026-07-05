# ============================================================
# Script 1: ENOE + CONEVAL + Modelos finales, tablas, figura, mapas
# Requiere: Script_0 ejecutado (objeto "base" en memoria)
# Autor: Jésica Tapia y Benjamín Oliva Vázquez
# ============================================================

library(data.table)
library(readxl)
library(survey)
library(ggplot2)
library(sf)
library(MASS)       # polr() para ordered logit

source("00_config.R")
source("00_funciones.R")

# ============================================================
# PARTE A: MERGE CONEVAL + ENOE (parche definitivo)
# ============================================================

if (!exists("base")) stop("Objeto 'base' no encontrado. Ejecuta Script_0 v4 primero.")
cat("\n========== SCRIPT 2: Inicio ==========\n")

# --- Limpia integraciones previas ---
remove_cols <- c(
  "cve_mun", "pobreza_pct", "pobreza_multi", "pobreza_multi_p75",
  "pobreza_multi_40", "t_desoc", "alta_desoc", "n_crit_desv",
  "desventaja", "desventaja_4", "mujer_x_desventaja",
  "mujer_x_menores", "mujer_x_sinauto_x_perif", "enoe_source"
)
drop_if_exists(base, remove_cols)

# --- Clave municipal ---
base[, cve_mun := make_cve_mun(ent, mun)]
base_muns <- unique(na.omit(base$cve_mun))
cat("\nMunicipios EOD (primeros 20):\n")
print(head(sort(base_muns), 20))
cat("Total municipios EOD:", length(base_muns), "\n")

# --- CONEVAL ---
path_coneval <- file.path(repo, "Data/Concentrado_Pobreza/Concentrado_indicadores_de_pobreza_2020.xlsx")
# Usamos 2015 (col 9) por cercanía temporal a EOD 2017
anio_pobreza <- 2015
col_pobreza <- switch(as.character(anio_pobreza), "2015" = 9L, "2020" = 10L)

coneval_raw <- as.data.table(read_excel(
  path_coneval, sheet = "Concentrado municipal", skip = 5, .name_repair = "unique"
))
nm_coneval <- names(coneval_raw)
coneval_dt <- coneval_raw[, .(
  cve_mun     = make_cve_mun_full(get(nm_coneval[3])),
  pobreza_pct = num_clean(get(nm_coneval[col_pobreza]))
)]
coneval_dt <- coneval_dt[!is.na(cve_mun) & !is.na(pobreza_pct)]
if (max(coneval_dt$pobreza_pct, na.rm = TRUE) <= 1.5) coneval_dt[, pobreza_pct := pobreza_pct * 100]
coneval_dt <- unique(coneval_dt[, .(cve_mun, pobreza_pct)])

# Corte p75 SOLO entre municipios ZMVM
coneval_zmvm <- coneval_dt[cve_mun %in% base_muns]
stopifnot("CONEVAL no hizo match con EOD" = nrow(coneval_zmvm) > 0)
p75_pobreza_zmvm <- quantile(coneval_zmvm$pobreza_pct, 0.75, na.rm = TRUE)
cat("\nCONEVAL: corte p75 ZMVM =", round(p75_pobreza_zmvm, 1), "% pobreza\n")
cat("Municipios ZMVM en CONEVAL:", nrow(coneval_zmvm), "de", length(base_muns), "\n")

coneval_dt[, pobreza_multi_p75 := as.integer(pobreza_pct >= p75_pobreza_zmvm)]
coneval_dt[, pobreza_multi_40  := as.integer(pobreza_pct >= 40)]
coneval_dt[, pobreza_multi     := pobreza_multi_p75]

base <- merge(base,
  coneval_dt[, .(cve_mun, pobreza_pct, pobreza_multi, pobreza_multi_p75, pobreza_multi_40)],
  by = "cve_mun", all.x = TRUE
)

# --- ENOE ---
read_enoe_desoc <- function() {
  path_eap  <- file.path(repo, "Data/ILMM/ilmm_2017_bd_xlsx/base_datos/EAP_ESTIMACIONES_PEA_OCU_INF_2017_T1.xlsx")
  path_rate <- file.path(repo, "Data/ILMM/ilmm_2017_bd_xlsx/base_datos/ENOE_PEA_OCU_2017.xlsx")
  
  if (file.exists(path_eap)) {
    eap <- as.data.table(read_excel(path_eap))
    setnames(eap, names(eap), gsub("\\s+", "_", names(eap)))
    nm <- names(eap)
    llave_col <- nm[match("LLAVE", toupper(nm))]
    pea_col   <- nm[match("PEA",   toupper(nm))]
    ocu_col   <- nm[match("OCUPADOS", toupper(nm))]
    if (any(is.na(c(llave_col, pea_col, ocu_col)))) stop("EAP: falta LLAVE, PEA u OCUPADOS.")
    out <- eap[, .(
      cve_mun  = make_cve_mun_full(get(llave_col)),
      pea      = num_clean(get(pea_col)),
      ocupados = num_clean(get(ocu_col))
    )]
    out <- out[!is.na(cve_mun) & !is.na(pea) & pea > 0]
    out[, t_desoc := (pea - ocupados) / pea * 100]
    return(out[, .(cve_mun, t_desoc, enoe_source = "EAP_counts")])
  }
  
  enoe_raw <- as.data.table(read_excel(path_rate))
  setnames(enoe_raw, names(enoe_raw), gsub("\\s+", "_", names(enoe_raw)))
  nm <- names(enoe_raw)
  find_col <- function(candidates) {
    idx <- match(toupper(candidates), toupper(nm))
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0L) stop(paste("Falta columna ENOE:", paste(candidates, collapse = ", ")))
    nm[idx[1]]
  }
  col_ent <- find_col(c("ENTIDAD", "CVE_ENT", "ENT"))
  col_mun <- find_col(c("MUNICIP", "MUNICIPIO", "CVE_MUN", "MUN"))
  col_pea <- find_col(c("PEA"))
  col_ocu <- find_col(c("OCUPADOS", "OCUPADO", "OCU"))
  out <- enoe_raw[, .(
    ent = as.integer(num_clean(get(col_ent))),
    mun = as.integer(num_clean(get(col_mun))),
    pea      = num_clean(get(col_pea)),
    ocupados = num_clean(get(col_ocu))
  )]
  out <- out[!is.na(ent) & !is.na(mun) & !is.na(pea) & pea > 0]
  out[, cve_mun := make_cve_mun(ent, mun)]
  out[, t_desoc := (pea - ocupados) / pea * 100]
  out[, .(t_desoc = mean(t_desoc, na.rm = TRUE), enoe_source = "rate_file"), by = cve_mun]
}

enoe_dt <- read_enoe_desoc()
enoe_zmvm <- enoe_dt[cve_mun %in% base_muns]
stopifnot("ENOE no hizo match con EOD" = nrow(enoe_zmvm) > 0)
p75_desoc_zmvm <- quantile(enoe_zmvm$t_desoc, 0.75, na.rm = TRUE)
cat("ENOE: corte p75 ZMVM =", round(p75_desoc_zmvm, 2), "% desocupación\n")

enoe_dt[, alta_desoc := as.integer(t_desoc >= p75_desoc_zmvm)]
base <- merge(base, enoe_dt[, .(cve_mun, t_desoc, alta_desoc, enoe_source)],
              by = "cve_mun", all.x = TRUE)

# --- Índice de desventaja: 4 criterios, umbral ≥2 ---
base[, pobreza_multi := fcoalesce(pobreza_multi, 0L)]
base[, alta_desoc    := fcoalesce(alta_desoc, 0L)]
base[, n_crit_desv   := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("sin_auto", "periferia", "pobreza_multi", "alta_desoc")]
base[, desventaja    := as.integer(n_crit_desv >= 2)]

# --- Interacciones ---
base[, mujer_x_desventaja    := mujer * desventaja]
base[, mujer_x_menores       := mujer * hay_menores12]
base[, mujer_x_sinauto_x_perif := mujer * sin_auto * periferia]
if (!"ent_f" %in% names(base)) base[, ent_f := factor(ent)]
if (!"niv_f" %in% names(base)) base[, niv_f := make_factor_with_missing(niv)]

# --- Diagnóstico de integración ---
base_model    <- base[!is.na(factor_per) & factor_per > 0 & !is.na(edad) & !is.na(mujer) & !is.na(sin_auto)]
base_travelers <- base_model[n_viajes > 0]

diag_desv <- base_model[, .(
  n_personas            = .N,
  municipios_base       = length(base_muns),
  match_coneval_pct     = round(weighted.mean(!is.na(pobreza_pct), factor_per, na.rm = TRUE) * 100, 1),
  match_enoe_pct        = round(weighted.mean(!is.na(t_desoc), factor_per, na.rm = TRUE) * 100, 1),
  corte_pobreza_p75     = round(as.numeric(p75_pobreza_zmvm), 1),
  corte_desoc_p75       = round(as.numeric(p75_desoc_zmvm), 2),
  pct_sin_auto          = round(weighted.mean(sin_auto == 1,       factor_per, na.rm = TRUE) * 100, 1),
  pct_periferia         = round(weighted.mean(periferia == 1,      factor_per, na.rm = TRUE) * 100, 1),
  pct_pobreza_multi     = round(weighted.mean(pobreza_multi == 1,  factor_per, na.rm = TRUE) * 100, 1),
  pct_alta_desoc        = round(weighted.mean(alta_desoc == 1,     factor_per, na.rm = TRUE) * 100, 1),
  pct_desventaja        = round(weighted.mean(desventaja == 1,     factor_per, na.rm = TRUE) * 100, 1),
  pct_chain_tour_all    = round(weighted.mean(trip_chain_tour == 1, factor_per, na.rm = TRUE) * 100, 1),
  pct_chain_tour_trav   = round(weighted.mean(trip_chain_tour[n_viajes > 0] == 1,
                                              factor_per[n_viajes > 0], na.rm = TRUE) * 100, 1)
)]
cat("\n===== DIAGNÓSTICO INTEGRACIÓN =====\n")
print(diag_desv)
fwrite(diag_desv, file.path(out_dir, "diag_desventaja_script2.csv"))

# GUARDIA: si pobreza_multi sigue en 0 algo salió mal
if (diag_desv$pct_pobreza_multi == 0) {
  stop("pobreza_multi = 0%. Revisa que el merge CONEVAL hizo match. Claves EOD vs CONEVAL no empatan.")
}

cat("\n===== PARTE A COMPLETA: Desventaja 4 criterios integrada =====\n")

# ============================================================
# PARTE B: TABLA 1 — Descriptivos cruzados género × desventaja
# ============================================================

cat("\n===== TABLA 1: DESCRIPTIVOS =====\n")

des_all <- make_svy_design(base_model, "factor_per")

# Tabla 1: Descriptivos por género × desventaja
grupo_vars <- base_model[, .(
  genero      = ifelse(mujer == 1, "Mujer", "Hombre"),
  desv_grupo  = ifelse(desventaja == 1, "Desventaja", "Sin desventaja")
)]
base_model[, grupo := paste(grupo_vars$genero, grupo_vars$desv_grupo, sep = " | ")]

tabla1_list <- list()
for (g in sort(unique(base_model$grupo))) {
  sub <- base_model[grupo == g]
  w   <- sub$factor_per
  tabla1_list[[g]] <- data.table(
    Grupo            = g,
    N_obs            = nrow(sub),
    N_expandido      = round(sum(w)),
    Edad_media       = round(weighted.mean(sub$edad, w, na.rm = TRUE), 1),
    Pct_trabaja      = round(weighted.mean(sub$trabaja, w, na.rm = TRUE) * 100, 1),
    Pct_sin_auto     = round(weighted.mean(sub$sin_auto, w, na.rm = TRUE) * 100, 1),
    Pct_periferia    = round(weighted.mean(sub$periferia, w, na.rm = TRUE) * 100, 1),
    Pct_hay_menores  = round(weighted.mean(sub$hay_menores12, w, na.rm = TRUE) * 100, 1),
    Viajes_dia       = round(weighted.mean(sub$n_viajes, w, na.rm = TRUE), 2),
    Pct_trip_chain   = round(weighted.mean(sub$trip_chain_tour, w, na.rm = TRUE) * 100, 1),
    Paradas_media    = round(weighted.mean(sub$num_paradas_tour, w, na.rm = TRUE), 2),
    Pct_care_trip    = round(weighted.mean(sub$care_trip, w, na.rm = TRUE) * 100, 1)
  )
}
tabla1 <- rbindlist(tabla1_list)
print(tabla1)
fwrite(tabla1, file.path(out_dir, "tabla1_descriptivos_genero_desventaja.csv"))

# Tabla 1b: Test t de diferencia de medias en trip_chain por género (ponderado)
t_chain <- svyttest(trip_chain_tour ~ mujer, design = des_all)
cat("\nTest t ponderado trip_chain Mujer vs Hombre:\n")
print(t_chain)

# Limpiar columna temporal
base_model[, grupo := NULL]

# ============================================================
# PARTE C: MODELOS (Tabla 2 y 3)
# ============================================================

cat("\n===== ESTIMACIÓN DE MODELOS =====\n")

des_all      <- make_svy_design(base_model, "factor_per")
des_travelers <- make_svy_design(base_travelers, "factor_per")

# --- Especificaciones ---
rhs_core <- "mujer * desventaja + mujer * hay_menores12 + edad + I(edad^2) + trabaja + niv_f + adultos_65mas + jefa_mujer + ent_f"
rhs_comp <- "mujer + sin_auto + periferia + pobreza_multi + alta_desoc + mujer:sin_auto + mujer:periferia + mujer:hay_menores12 + hay_menores12 + edad + I(edad^2) + trabaja + niv_f + adultos_65mas + jefa_mujer + ent_f"

models <- list()

# M1: Generación de viajes (conteo)
cat("  Estimando M1: Generación viajes (Poisson)...\n")
models$gen_count <- svyglm(as.formula(paste("n_viajes ~", rhs_core)),
                           design = des_all, family = quasipoisson())

# M2: Encadenamiento por tour (logit) — MODELO PRINCIPAL
cat("  Estimando M2: Trip chain tour (logit)...\n")
models$chain_tour <- svyglm(as.formula(paste("trip_chain_tour ~", rhs_core)),
                            design = des_all, family = quasibinomial())

# M3: Encadenamiento diario (logit) — robustez
cat("  Estimando M3: Trip chain día (logit)...\n")
models$chain_day <- svyglm(as.formula(paste("trip_chain_dia ~", rhs_core)),
                           design = des_all, family = quasibinomial())

# M4: Paradas por tour (Poisson) — margen intensivo
cat("  Estimando M4: Paradas por tour (Poisson)...\n")
models$stops_tour <- svyglm(as.formula(paste("num_paradas_tour ~", rhs_core)),
                            design = des_all, family = quasipoisson())

# M5: Componentes de desventaja desagregados
cat("  Estimando M5: Componentes desventaja...\n")
models$chain_comp <- svyglm(as.formula(paste("trip_chain_tour ~", rhs_comp)),
                            design = des_all, family = quasibinomial())

# M6: Condicional en viajeros (robustez)
cat("  Estimando M6: Chain tour | viajeros...\n")
models$chain_travelers <- svyglm(as.formula(paste("trip_chain_tour ~", rhs_core)),
                                 design = des_travelers, family = quasibinomial())

# M7: Ordered logit tour_complex (robustez VD3)
cat("  Estimando M7: Tour complexity (ordered logit)...\n")
# polr no acepta svydesign, usamos pesos directamente
base_model[, tour_complex_f := factor(tour_complex, ordered = TRUE)]
m_ologit <- tryCatch({
  polr(as.formula(paste("tour_complex_f ~", gsub("\\+ ent_f", "", rhs_core))),
       data = base_model, weights = factor_per, Hess = TRUE, method = "logistic")
}, error = function(e) {
  cat("  ADVERTENCIA: ordered logit falló:", e$message, "\n")
  NULL
})

# --- Extraer coeficientes ---
coef_all <- rbindlist(Map(extract_svy, models, names(models)), fill = TRUE)

# Agregar ordered logit si corrió
if (!is.null(m_ologit)) {
  sm_ol <- summary(m_ologit)
  cf_ol <- as.data.table(coef(sm_ol), keep.rownames = "term")
  setnames(cf_ol, names(cf_ol), c("term", "estimate", "std_error", "t_value"))
  cf_ol[, `:=`(
    model = "ologit_complex",
    p_value = 2 * pnorm(-abs(t_value)),
    exp_estimate = exp(estimate),
    ci_l = estimate - 1.96 * std_error,
    ci_u = estimate + 1.96 * std_error
  )]
  cf_ol[, sig := fifelse(p_value < 0.01, "***",
                 fifelse(p_value < 0.05, "**",
                 fifelse(p_value < 0.1, "*", "")))]
  cf_ol[, t_value := NULL]
  coef_all <- rbindlist(list(coef_all, cf_ol), fill = TRUE)
}

fwrite(coef_all, file.path(out_dir, "coeficientes_todos_script2.csv"))

# --- Tabla compacta ---
key_terms <- c(
  "mujer", "desventaja", "mujer:desventaja",
  "hay_menores12", "mujer:hay_menores12",
  "sin_auto", "periferia", "pobreza_multi", "alta_desoc",
  "mujer:sin_auto", "mujer:periferia",
  "edad", "I(edad^2)", "trabaja", "adultos_65mas", "jefa_mujer"
)

model_order  <- names(models)
model_labels <- c(
  gen_count       = "Viajes",
  chain_tour      = "Chain tour",
  chain_day       = "Chain día",
  stops_tour      = "Paradas",
  chain_comp      = "Componentes",
  chain_travelers = "Viajeros"
)[model_order]

compact <- make_compact_table(coef_all, key_terms, model_order, model_labels)
write_txt_table(compact, file.path(out_dir, "tabla2_modelos_compacta.txt"))
cat("\n===== TABLA 2: Modelos compactos =====\n")
print(compact)

# Coeficientes exponenciados clave
cat("\n===== OR / IRR clave =====\n")
print(coef_all[term %in% key_terms[1:11],
  .(model, term, estimate, exp_estimate,
    exp_ci_l = exp(ci_l), exp_ci_u = exp(ci_u), sig)])

# ============================================================
# PARTE D: FIGURA 1 — Predicciones marginales
# ============================================================

cat("\n===== FIGURA 1 =====\n")

nd <- data.table(expand.grid(
  mujer        = c(0, 1),
  desventaja   = c(0, 1),
  hay_menores12 = c(0, 1),
  edad     = 40,
  trabaja  = 1,
  adultos_65mas = 0,
  jefa_mujer    = 0
))
niv_ref <- if ("8" %in% levels(base_model$niv_f)) "8" else levels(base_model$niv_f)[1]
ent_ref <- if ("9" %in% levels(base_model$ent_f)) "9" else levels(base_model$ent_f)[1]
nd[, `:=`(
  niv_f = factor(niv_ref, levels = levels(base_model$niv_f)),
  ent_f = factor(ent_ref, levels = levels(base_model$ent_f))
)]

pred_nd <- predict_logit_ci(models$chain_tour, nd)
nd <- cbind(nd, pred_nd)
fwrite(nd, file.path(out_dir, "predicciones_fig1_script2.csv"))

fig1 <- ggplot(nd, aes(
  x    = interaction(mujer, hay_menores12),
  y    = fit,
  fill = factor(desventaja)
)) +
  geom_col(position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u),
                position = position_dodge(.9), width = .2) +
  scale_x_discrete(labels = c(
    "Hombre\nSin menores", "Mujer\nSin menores",
    "Hombre\nCon menores", "Mujer\nCon menores"
  )) +
  scale_fill_manual(
    values = c("0" = "#4E79A7", "1" = "#E15759"),
    labels = c("Sin desventaja", "Con desventaja")
  ) +
  labs(
    y        = "Probabilidad predicha de encadenamiento",
    x        = "",
    fill     = "",
    title    = "Probabilidad de encadenamiento de viajes por género, cuidado y desventaja",
    subtitle = "Logit ponderado, persona de 40 años, ocupada, preparatoria, CDMX. IC 95%.",
    caption  = "Fuente: EOD 2017, INEGI. Elaboración propia."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "top",
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    panel.grid.minor = element_blank()
  )
print(fig1)
ggsave(file.path(out_dir, "fig1_margenes_script2.png"), fig1, width = 9, height = 5.5, dpi = 300)

# ============================================================
# PARTE E: MAPA — Encadenamiento por distrito y género
# ============================================================

cat("\n===== MAPA DE DISTRITOS =====\n")

if (file.exists(shp_path)) {
  distritos_sf <- st_read(shp_path, quiet = TRUE)
  
  # Identificar columna de distrito en shapefile
  shp_names <- tolower(names(distritos_sf))
  dist_col <- names(distritos_sf)[which(shp_names %in% c("distrito", "cvegeo", "id", "cve_dist"))[1]]
  if (is.na(dist_col)) {
    cat("Columnas del shapefile:", paste(names(distritos_sf), collapse = ", "), "\n")
    cat("ADVERTENCIA: No encontré columna de distrito. Revisa nombres manualmente.\n")
    dist_col <- names(distritos_sf)[1]  # fallback
  }
  cat("Usando columna shapefile:", dist_col, "\n")
  
  # Agregar datos por distrito
  mapa_datos <- base_model[n_viajes > 0, .(
    pct_chain_total    = weighted.mean(trip_chain_tour, factor_per, na.rm = TRUE) * 100,
    pct_chain_mujer    = weighted.mean(trip_chain_tour[mujer == 1], factor_per[mujer == 1], na.rm = TRUE) * 100,
    pct_chain_hombre   = weighted.mean(trip_chain_tour[mujer == 0], factor_per[mujer == 0], na.rm = TRUE) * 100,
    pct_desventaja     = weighted.mean(desventaja, factor_per, na.rm = TRUE) * 100,
    tiempo_med_dist    = mean(tiempo_med, na.rm = TRUE),
    n_viajeros         = .N
  ), by = distrito]
  
  # Brecha de género
  mapa_datos[, brecha_genero := pct_chain_mujer - pct_chain_hombre]
  
  # Merge
  distritos_sf[[dist_col]] <- as.integer(distritos_sf[[dist_col]])
  mapa_merged <- merge(distritos_sf, mapa_datos, by.x = dist_col, by.y = "distrito", all.x = TRUE)
  
  # Mapa 1: % encadenamiento total
  fig_mapa1 <- ggplot(mapa_merged) +
    geom_sf(aes(fill = pct_chain_total), color = "gray70", size = 0.15) +
    scale_fill_viridis_c(option = "C", na.value = "gray90",
                         name = "% Encadenamiento") +
    labs(
      title    = "Encadenamiento de viajes por distrito, ZMVM 2017",
      subtitle = "Porcentaje de viajeros entre semana con ≥2 paradas no-hogar en un tour",
      caption  = "Fuente: EOD 2017, INEGI. Elaboración propia."
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold"),
      legend.position = c(0.15, 0.25)
    )
  print(fig_mapa1)
  ggsave(file.path(out_dir, "mapa1_chain_total.png"), fig_mapa1, width = 10, height = 9, dpi = 300)
  
  # Mapa 2: Brecha de género (M - H)
  fig_mapa2 <- ggplot(mapa_merged) +
    geom_sf(aes(fill = brecha_genero), color = "gray70", size = 0.15) +
    scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
      na.value = "gray90",
      name = "Brecha\n(M − H, pp)"
    ) +
    labs(
      title    = "Brecha de género en encadenamiento por distrito, ZMVM 2017",
      subtitle = "Diferencia en pp: % mujeres que encadenan − % hombres. Rojo = mujeres encadenan más.",
      caption  = "Fuente: EOD 2017, INEGI. Elaboración propia."
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold"),
      legend.position = c(0.15, 0.25)
    )
  print(fig_mapa2)
  ggsave(file.path(out_dir, "mapa2_brecha_genero.png"), fig_mapa2, width = 10, height = 9, dpi = 300)
  
  # Mapa 3: % desventaja
  fig_mapa3 <- ggplot(mapa_merged) +
    geom_sf(aes(fill = pct_desventaja), color = "gray70", size = 0.15) +
    scale_fill_viridis_c(option = "D", na.value = "gray90",
                         name = "% Desventaja") +
    labs(
      title    = "Hogares en desventaja por distrito, ZMVM 2017",
      subtitle = "Porcentaje con ≥2 de: sin auto, periferia, pobreza municipal p75, alta desocupación p75",
      caption  = "Fuente: EOD 2017, CONEVAL 2015, ENOE 2017. Elaboración propia."
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold"),
      legend.position = c(0.15, 0.25)
    )
  print(fig_mapa3)
  ggsave(file.path(out_dir, "mapa3_desventaja.png"), fig_mapa3, width = 10, height = 9, dpi = 300)
  
  # Exportar datos del mapa
  fwrite(mapa_datos, file.path(out_dir, "datos_mapa_distritos.csv"))
  
  cat("Mapas guardados en:", out_dir, "\n")
  
} else {
  cat("SHAPEFILE NO ENCONTRADO en:", shp_path, "\n")
  cat("Ajusta la ruta en la variable shp_path.\n")
  cat("Generando datos agregados por distrito sin mapa...\n")
  
  mapa_datos <- base_model[n_viajes > 0, .(
    pct_chain_total  = round(weighted.mean(trip_chain_tour, factor_per, na.rm = TRUE) * 100, 1),
    pct_chain_mujer  = round(weighted.mean(trip_chain_tour[mujer == 1], factor_per[mujer == 1], na.rm = TRUE) * 100, 1),
    pct_chain_hombre = round(weighted.mean(trip_chain_tour[mujer == 0], factor_per[mujer == 0], na.rm = TRUE) * 100, 1),
    pct_desventaja   = round(weighted.mean(desventaja, factor_per, na.rm = TRUE) * 100, 1),
    n_viajeros       = .N
  ), by = distrito]
  mapa_datos[, brecha_genero := pct_chain_mujer - pct_chain_hombre]
  fwrite(mapa_datos, file.path(out_dir, "datos_mapa_distritos.csv"))
  cat("Datos por distrito exportados. Genera mapas cuando tengas el shapefile.\n")
}

# ============================================================
# PARTE F: EXPORTAR BASE FINAL
# ============================================================

fwrite(base, file.path(out_dir, "base_analitica_final_script2.csv"))

cat("\n===== SCRIPT 2 COMPLETO =====\n")
cat("Archivos generados en:", out_dir, "\n")
cat("  - diag_desventaja_script2.csv\n")
cat("  - tabla1_descriptivos_genero_desventaja.csv\n")
cat("  - coeficientes_todos_script2.csv\n")
cat("  - tabla2_modelos_compacta.txt\n")
cat("  - predicciones_fig1_script2.csv\n")
cat("  - fig1_margenes_script2.png\n")
cat("  - mapa1_chain_total.png  (si shapefile disponible)\n")
cat("  - mapa2_brecha_genero.png\n")
cat("  - mapa3_desventaja.png\n")
cat("  - datos_mapa_distritos.csv\n")
cat("  - base_analitica_final_script2.csv\n")

