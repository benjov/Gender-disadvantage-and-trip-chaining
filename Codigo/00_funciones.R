# ============================================================
# 00_funciones.R — Funciones compartidas del pipeline
# Fuente única: Scripts 0, 1 y 2 las cargan con source().
# ============================================================

ensure_csv <- function(csv_path) {
  if (file.exists(csv_path)) return(csv_path)
  zip_path <- paste0(csv_path, ".zip")
  if (!file.exists(zip_path)) {
    stop(sprintf("No se encontró el CSV ni su .zip: %s", csv_path))
  }
  cat("[ensure_csv] Descomprimiendo", zip_path, "\n")
  unzip(zip_path, files = basename(csv_path), exdir = dirname(csv_path), junkpaths = TRUE)
  csv_path
}

clean_names <- function(dt) {
  setnames(dt, names(dt), tolower(names(dt)))
  invisible(dt)
}

as_num <- function(x) {
  x <- gsub(",", "", as.character(x))
  x <- trimws(x)
  x[x %in% c("", "b", "B", "NA", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

assert_cols <- function(dt, cols, dt_name = deparse(substitute(dt))) {
  miss <- setdiff(cols, names(dt))
  if (length(miss) > 0L) stop(sprintf("Missing columns in %s: %s", dt_name, paste(miss, collapse = ", ")))
}

num_clean <- function(x) {
  x <- gsub(",", "", as.character(x))
  x <- gsub("%", "", x)
  x <- trimws(x)
  x[x %in% c("", "b", "B", "NA", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

make_cve_mun <- function(ent, mun) {
  ent_num <- as.integer(ent)
  mun_num <- as.integer(mun)
  out <- ifelse(
    !is.na(mun_num) & mun_num >= 1000,
    sprintf("%05d", mun_num),
    sprintf("%02d%03d", ent_num, mun_num)
  )
  out[is.na(ent_num) | is.na(mun_num)] <- NA_character_
  out
}

make_cve_mun_full <- function(x) {
  x_num <- as.integer(num_clean(x))
  out <- sprintf("%05d", x_num)
  out[is.na(x_num)] <- NA_character_
  out
}

drop_if_exists <- function(dt, cols) {
  cols <- intersect(cols, names(dt))
  if (length(cols) > 0L) dt[, (cols) := NULL]
  invisible(dt)
}

make_factor_with_missing <- function(x, missing_label = "No especificado") {
  z <- as.character(x)
  z[is.na(z) | z == ""] <- missing_label
  factor(z)
}

make_svy_design <- function(dt, weight_var = "factor_per") {
  options(survey.lonely.psu = "adjust")
  if (all(c("upm_dis", "est_dis") %in% names(dt))) {
    return(svydesign(
      ids = ~upm_dis, strata = ~est_dis,
      weights = as.formula(paste0("~", weight_var)),
      nest = TRUE, data = dt
    ))
  }
  svydesign(ids = ~id_hog, weights = as.formula(paste0("~", weight_var)), data = dt)
}

predict_logit_ci <- function(model, newdata, level = 0.95) {
  tt  <- delete.response(terms(model))
  mm  <- model.matrix(tt, newdata, contrasts.arg = model$contrasts)
  beta <- coef(model)
  V   <- vcov(model)
  # Alinear columnas
  missing_cols <- setdiff(names(beta), colnames(mm))
  if (length(missing_cols) > 0L) {
    zeros <- matrix(0, nrow = nrow(mm), ncol = length(missing_cols))
    colnames(zeros) <- missing_cols
    mm <- cbind(mm, zeros)
  }
  extra_cols <- setdiff(colnames(mm), names(beta))
  if (length(extra_cols) > 0L) mm <- mm[, setdiff(colnames(mm), extra_cols), drop = FALSE]
  mm <- mm[, names(beta), drop = FALSE]
  eta    <- as.numeric(mm %*% beta)
  se_eta <- sqrt(pmax(0, diag(mm %*% V %*% t(mm))))
  z <- qnorm(1 - (1 - level) / 2)
  data.table(eta = eta, se_eta = se_eta, fit = plogis(eta),
             ci_l = plogis(eta - z * se_eta), ci_u = plogis(eta + z * se_eta))
}

extract_svy <- function(model, model_name) {
  sm <- summary(model)
  cf <- as.data.table(sm$coefficients, keep.rownames = "term")
  setnames(cf, names(cf), gsub(" ", "_", names(cf), fixed = TRUE))
  nm <- names(cf)
  est_col  <- intersect(c("Estimate"), nm)[1]
  se_col   <- intersect(c("Std._Error", "Std..Error"), nm)[1]
  p_col    <- grep("Pr", nm, value = TRUE)[1]
  out <- data.table(
    model     = model_name,
    term      = cf$term,
    estimate  = as.numeric(cf[[est_col]]),
    std_error = as.numeric(cf[[se_col]]),
    p_value   = if (!is.na(p_col)) as.numeric(cf[[p_col]]) else NA_real_
  )
  out[, exp_estimate := exp(estimate)]
  out[, ci_l := estimate - 1.96 * std_error]
  out[, ci_u := estimate + 1.96 * std_error]
  out[, sig := fifelse(p_value < 0.01, "***",
               fifelse(p_value < 0.05, "**",
               fifelse(p_value < 0.1,  "*", "")))]
  out[]
}

make_compact_table <- function(coef_dt, key_terms, model_order, model_labels) {
  tmp <- coef_dt[term %in% key_terms]
  tmp[, model := factor(model, levels = model_order, labels = model_labels)]
  tmp[, term  := factor(term, levels = key_terms)]
  tmp[, cell  := sprintf("%.3f%s (%.3f)", estimate, sig, std_error)]
  wide <- dcast(tmp, term ~ model, value.var = "cell")
  setorder(wide, term)
  wide[]
}

write_txt_table <- function(dt, file) {
  old_width <- options(width = 200)
  on.exit(options(old_width))
  lines <- capture.output(print(as.data.frame(dt), row.names = FALSE))
  writeLines(lines, con = file)
}

extract_focal <- function(model, label) {
  co <- summary(model)$coefficients
  keep <- intersect(focal, rownames(co))
  data.table(spec = label, term = keep,
             beta = co[keep, 1], se = co[keep, 2],
             p = co[keep, 4], OR = exp(co[keep, 1]))
}


wcorr <- function(x, y, w) {
  mx <- weighted.mean(x, w); my <- weighted.mean(y, w)
  cov <- weighted.mean((x - mx) * (y - my), w)
  cov / sqrt(weighted.mean((x - mx)^2, w) * weighted.mean((y - my)^2, w))
}

