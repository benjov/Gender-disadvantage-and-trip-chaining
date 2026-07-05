# ============================================================
# Script_2_robustez_journal.R
# Robustness checks and English-language figures for the
# journal version (Journal of Transport Geography submission)
#
# Self-contained: depends only on outputs of Script_1:
#   - base_analitica_final_script2.csv
#   - datos_mapa_distritos.csv
# plus the ZMVM district shapefile (shp_path in 00_config.R) for maps.
#
# Sections:
#   A. Setup and survey design (identical to Script_1)
#   B. Reference re-estimation of M2 (validation)
#   C. Robustness 1: Firth penalized logit (rare events)
#   D. Robustness 2: disadvantage-threshold sensitivity
#   E. Robustness 3: district-level weighted correlations
#   F. Figure 1 (EN): predicted probabilities
#   G. Figure 2 (EN): district maps
#
# Expected focal results (validated independently):
#   M2:   mujer 0.059 | desventaja -0.291 | mujer:desventaja 0.305
#         hay_menores12 0.182 | mujer:hay_menores12 0.364
#   Firth: max |Firth - MLE| on focal terms < 0.001
#   >=1:  mujer:desventaja ~ 0.226 (OR 1.25)
#   >=3:  mujer:desventaja ~ 0.374 (OR 1.45)
#   Corr: chain x desventaja  r ~ -0.24 (hombres -0.31, mujeres -0.16)
# ============================================================

library(data.table)
library(survey)
library(logistf)    # Firth penalized likelihood
library(ggplot2)
library(sf)
library(scales)

source("00_config.R")
source("00_funciones.R")

in_dir  <- out_dir       # entradas: salidas de Script_1
out_dir <- out_journal   # salidas de este script

options(survey.lonely.psu = "adjust")

# ============================================================
# A. LOAD BASE AND REBUILD DESIGN (as in Script_1)
# ============================================================

base <- fread(file.path(in_dir, "base_analitica_final_script2.csv"))

# Factors exactly as in Script_1
base[, niv_f := factor(fifelse(is.na(niv_f) | niv_f == "", "No especificado",
                               as.character(niv_f)))]
base[, ent_f := factor(ent_f)]

core_vars <- c("trip_chain_tour", "mujer", "desventaja", "hay_menores12",
               "edad", "trabaja", "niv_f", "adultos_65mas", "jefa_mujer",
               "ent_f", "factor_per", "id_hog", "n_crit_desv")
base_model <- base[complete.cases(base[, ..core_vars])]
cat("N estimation:", nrow(base_model),
    "| events:", sum(base_model$trip_chain_tour), "\n")


rhs_core <- "mujer * desventaja + mujer * hay_menores12 + edad + I(edad^2) + trabaja + niv_f + adultos_65mas + jefa_mujer + ent_f"
f_chain  <- as.formula(paste("trip_chain_tour ~", rhs_core))
focal    <- c("mujer", "desventaja", "mujer:desventaja",
              "hay_menores12", "mujer:hay_menores12")


# ============================================================
# B. REFERENCE M2 (must reproduce Script_1 / Table 2)
# ============================================================

cat("\n===== B. Reference M2 =====\n")
des_all <- make_svy_design(base_model)
m2 <- svyglm(f_chain, design = des_all, family = quasibinomial())
tab_ref <- extract_focal(m2, "M2 reference (>=2 criteria)")
print(tab_ref)

# ============================================================
# C. ROBUSTNESS 1: FIRTH PENALIZED LOGIT (rare events)
# ------------------------------------------------------------
# 8,486 events / 199,973 obs. Firth (1993) penalization removes
# small-sample event bias. logistf does not support the complex
# design, so we use normalized expansion weights (mean = 1) and
# compare POINT ESTIMATES against the weighted MLE; design-based
# inference remains that of M2.
# ============================================================

cat("\n===== C. Firth penalized logit =====\n")
base_model[, w_norm := factor_per / mean(factor_per)]

m_firth <- logistf(f_chain, data = base_model, weights = w_norm,
                   control = logistf.control(maxit = 100))

tab_firth <- data.table(spec = "Firth", term = names(coef(m_firth)),
                        beta = coef(m_firth))[term %in% focal]
comp <- merge(tab_ref[, .(term, beta_MLE = beta)],
              tab_firth[, .(term, beta_Firth = beta)], by = "term")
comp[, abs_diff := abs(beta_Firth - beta_MLE)]
print(comp)
cat("Max |Firth - MLE| on focal terms:", max(comp$abs_diff), "\n")
fwrite(comp, file.path(out_dir, "rob_firth_vs_mle.csv"))

# ============================================================
# D. ROBUSTNESS 2: DISADVANTAGE THRESHOLD (>=1, >=3 criteria)
# ============================================================

cat("\n===== D. Threshold sensitivity =====\n")
tabs_thr <- list(tab_ref)
for (k in c(1L, 3L)) {
  bk <- copy(base_model)
  bk[, desventaja := as.integer(n_crit_desv >= k)]
  pct <- with(bk, weighted.mean(desventaja, factor_per)) * 100
  cat(sprintf("  Threshold >= %d criteria: %.1f%% of expanded population\n",
              k, pct))
  des_k <- make_svy_design(bk)
  m_k <- svyglm(f_chain, design = des_k, family = quasibinomial())
  tabs_thr[[length(tabs_thr) + 1]] <-
    extract_focal(m_k, sprintf(">=%d criteria (%.1f%% pop.)", k, pct))
}
tab_thr <- rbindlist(tabs_thr)
print(tab_thr)
fwrite(tab_thr, file.path(out_dir, "rob_umbrales_desventaja.csv"))

# ============================================================
# E. ROBUSTNESS 3: DISTRICT-LEVEL WEIGHTED CORRELATIONS
# ============================================================

cat("\n===== E. District correlations =====\n")
mapa <- fread(file.path(in_dir, "datos_mapa_distritos.csv"))
mapa <- mapa[complete.cases(mapa)]

pairs <- list(
  c("pct_chain_total",  "pct_desventaja"),
  c("pct_chain_total",  "tiempo_med_dist"),
  c("pct_chain_hombre", "pct_desventaja"),
  c("pct_chain_mujer",  "pct_desventaja"),
  c("brecha_genero",    "pct_desventaja"),
  c("brecha_genero",    "pct_chain_total")
)
tab_corr <- rbindlist(lapply(pairs, function(p) {
  data.table(x = p[1], y = p[2],
             r_weighted = wcorr(mapa[[p[1]]], mapa[[p[2]]], mapa$n_viajeros))
}))
print(tab_corr)
fwrite(tab_corr, file.path(out_dir, "rob_corr_distritos.csv"))

# ============================================================
# F. FIGURE 1 (ENGLISH): predicted probabilities from M2
# ============================================================

cat("\n===== F. Figure 1 (EN) =====\n")
newdat <- CJ(mujer = 0:1, desventaja = 0:1, hay_menores12 = 0:1)
newdat[, `:=`(edad = 40, trabaja = 1, adultos_65mas = 0, jefa_mujer = 0,
              niv_f = factor("8", levels = levels(base_model$niv_f)),
              ent_f = factor("9", levels = levels(base_model$ent_f)))]

pr <- predict(m2, newdata = newdat, type = "link", se.fit = TRUE)
newdat[, `:=`(eta = as.numeric(pr), se_eta = sqrt(attr(pr, "var")))]
newdat[, `:=`(fit  = plogis(eta),
              ci_l = plogis(eta - 1.96 * se_eta),
              ci_u = plogis(eta + 1.96 * se_eta))]

newdat[, grp := factor(
  fifelse(mujer == 0 & hay_menores12 == 0, "Men\nno children <12",
  fifelse(mujer == 1 & hay_menores12 == 0, "Women\nno children <12",
  fifelse(mujer == 0 & hay_menores12 == 1, "Men\nwith children <12",
                                           "Women\nwith children <12"))),
  levels = c("Men\nno children <12", "Women\nno children <12",
             "Men\nwith children <12", "Women\nwith children <12"))]
newdat[, disadv := factor(desventaja, 0:1,
                          c("Non-disadvantaged household",
                            "Disadvantaged household"))]

p1 <- ggplot(newdat, aes(grp, fit, fill = disadv)) +
  geom_col(position = position_dodge(0.85), width = 0.8) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u),
                position = position_dodge(0.85), width = 0.2,
                linewidth = 0.4) +
  scale_fill_manual(values = c("#4C72B0", "#C44E52")) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(title = "Predicted probability of trip chaining by gender, care, and disadvantage",
       subtitle = "Survey-weighted logit; person aged 40, employed, upper-secondary education, Mexico City. 95% CI.",
       x = NULL, y = "Predicted probability of trip chaining",
       fill = NULL,
       caption = "Source: 2017 OD Survey (INEGI). Authors' elaboration.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.major.x = element_blank())

ggsave(file.path(out_dir, "fig1_predicted_probabilities_EN.png"),
       p1, width = 8.5, height = 5.2, dpi = 300, bg = "white")

# ============================================================
# G. FIGURE 2 (ENGLISH): district maps
# ------------------------------------------------------------
# Requires the district shapefile. Join key: distrito.
# Reproduces the four Spanish maps of Script_1 with EN labels.
# ============================================================

cat("\n===== G. Figure 2 maps (EN) =====\n")
if (file.exists(shp_path)) {
  shp <- st_read(shp_path, quiet = TRUE)
  # EDIT if the shapefile district id has another name:
  names(shp)[tolower(names(shp)) %in% c("distrito", "cvegeo", "cve_dist", "dto", "id_dist")] <- "distrito"
  shp$distrito <- as.integer(shp$distrito)
  g <- merge(shp, mapa, by = "distrito", all.x = TRUE)

  theme_map <- theme_void(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = c(0.12, 0.30))

  # Map 1: total chaining
  m1 <- ggplot(g) + geom_sf(aes(fill = pct_chain_total),
                            color = "grey85", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", name = "% Chaining") +
    labs(title = "Trip chaining by district, ZMVM 2017",
         subtitle = "Weekday travelers with \u22652 non-home destinations in a tour",
         caption = "Source: 2017 OD Survey (INEGI). Authors' elaboration.") +
    theme_map
  ggsave(file.path(out_dir, "map1_chain_total_EN.png"), m1,
         width = 9, height = 8, dpi = 300, bg = "white")

  # Map 2: gender gap
  m2g <- ggplot(g) + geom_sf(aes(fill = brecha_genero),
                             color = "grey85", linewidth = 0.1) +
    scale_fill_gradient2(low = "#4575B4", mid = "white", high = "#B2182B",
                         midpoint = 0, name = "Gap\n(W \u2212 M, pp)") +
    labs(title = "Gender gap in trip chaining by district, ZMVM 2017",
         subtitle = "Difference in pp: women \u2212 men. Red = women chain more.",
         caption = "Source: 2017 OD Survey (INEGI). Authors' elaboration.") +
    theme_map
  ggsave(file.path(out_dir, "map2_gender_gap_EN.png"), m2g,
         width = 9, height = 8, dpi = 300, bg = "white")

  # Map 3: disadvantage
  m3 <- ggplot(g) + geom_sf(aes(fill = pct_desventaja),
                            color = "grey85", linewidth = 0.1) +
    scale_fill_viridis_c(name = "% Disadvantaged") +
    labs(title = "Disadvantaged households by district, ZMVM 2017",
         subtitle = "\u22652 criteria: no car, periphery, poverty p75, high unemployment p75",
         caption = "Source: 2017 OD Survey, CONEVAL 2015, ENOE 2017. Authors' elaboration.") +
    theme_map
  ggsave(file.path(out_dir, "map3_disadvantage_EN.png"), m3,
         width = 9, height = 8, dpi = 300, bg = "white")

  # Map 4: panel by gender
  # (rbind.sf requiere nombres identicos: renombrar antes de apilar)
  g_men <- g[, c("distrito", "pct_chain_hombre")]
  names(g_men)[names(g_men) == "pct_chain_hombre"] <- "pct"
  g_men$sexo <- "Men"
  g_women <- g[, c("distrito", "pct_chain_mujer")]
  names(g_women)[names(g_women) == "pct_chain_mujer"] <- "pct"
  g_women$sexo <- "Women"
  g_long <- rbind(g_men, g_women)
  g_long$sexo <- factor(g_long$sexo, levels = c("Men", "Women"))
  m4 <- ggplot(g_long) + geom_sf(aes(fill = pct),
                                 color = "grey90", linewidth = 0.05) +
    facet_wrap(~sexo) +
    scale_fill_viridis_c(option = "plasma", name = "% Chain") +
    labs(title = "Trip chaining by gender and district, ZMVM 2017",
         subtitle = "Tour-based definition: \u22652 non-home destinations in a tour. Common color scale.",
         caption = "Source: 2017 OD Survey (INEGI). Authors' elaboration.") +
    theme_void(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom",
          strip.text = element_text(face = "bold", size = 13))
  ggsave(file.path(out_dir, "map4_panel_gender_EN.png"), m4,
         width = 14, height = 8, dpi = 300, bg = "white")

  cat("  Maps written to", out_dir, "\n")
} else {
  cat("  WARNING: shapefile not found at", shp_path,
      "- edit shp_path and re-run section G.\n")
}

cat("\n===== Script_2 finished =====\n")

