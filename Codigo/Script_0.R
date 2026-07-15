# ============================================================
# Script 0: EOD 2017 -> 
# Autor: Jésica Tapia y Benjamín Oliva Vázquez
# ============================================================

library(data.table)
library(survey)
library(ggplot2)
library(stargazer)

source("00_config.R")
source("00_funciones.R")

# inputs (tviaje y ttransporte se distribuyen como .zip en el repo;
# ensure_csv() los descomprime la primera vez que se corre el pipeline):
tviaje <- fread(ensure_csv(file.path(ruta, "tviaje_eod2017/conjunto_de_datos/tviaje.csv")))
tsdem <- fread(file.path(ruta, "tsdem_eod2017/conjunto_de_datos/tsdem.csv"))
thogar <- fread(file.path(ruta, "thogar_eod2017/conjunto_de_datos/thogar.csv"))
ttrans <- fread(ensure_csv(file.path(ruta, "ttransporte_eod2017/conjunto_de_datos/ttransporte.csv")))

# clean data:
clean_names(tviaje)
clean_names(tsdem)
clean_names(thogar)
clean_names(ttrans)

assert_cols(tviaje, c("id_via", "id_soc", "p5_3", "n_via", "p5_6", "p5_13", "dto_origen", "dto_dest", "factor"), "tviaje")
assert_cols(tsdem, c("id_soc", "id_hog", "sexo", "edad", "parentesco", "niv", "p3_7", "p3_8", "p4_2", "p5_4", "factor"), "tsdem")
assert_cols(thogar, c("id_hog", "distrito", "p2_1_1", "factor", "ent", "mun"), "thogar")
assert_cols(ttrans, c("id_via", "p5_3", "p5_14", "p5_16", "p5_16_1_1", "p5_16_1_2", "factor"), "ttrans")

optional_tviaje <- intersect(c("p5_11a", "p5_9_1", "p5_9_2", "p5_10_1", "p5_10_2"), names(tviaje))
optional_tsdem <- intersect(c("gra", "upm_dis", "est_dis", "tloc"), names(tsdem))

for (v in intersect(c("p5_3", "n_via", "p5_6", "p5_11a", "p5_13", "dto_origen", "dto_dest", "p5_9_1", "p5_9_2", "p5_10_1", "p5_10_2", "factor"), 
                    names(tviaje))) tviaje[, (v) := as_num(get(v))]
for (v in intersect(c("sexo", "edad", "parentesco", "niv", "gra", "p3_7", "p3_8", "p4_2", "p5_4", "factor", "upm_dis", "est_dis", "tloc"), 
                    names(tsdem))) tsdem[, (v) := as_num(get(v))]
for (v in intersect(c("distrito", "p2_1_1", "factor", "ent", "mun"), 
                    names(thogar))) thogar[, (v) := as_num(get(v))]
for (v in intersect(c("p5_3", "p5_14", "p5_16", "p5_16_1_1", "p5_16_1_2", "factor"), 
                    names(ttrans))) ttrans[, (v) := as_num(get(v))]

tsdem[edad == 99, edad := NA_real_]
tsdem[niv == 99, niv := NA_real_]

cat("\nDictionary audit reminders:\n")
cat("p5_13 = trip purpose; p5_6 = origin place type.\n")
cat("p5_16 = mode order; time = p5_16_1_1*60 + p5_16_1_2.\n")
cat("p2_1_1 = cars/vans; p2_2 = hologram.\n")
cat("p3_7/p3_8 = labor; p4_3 = Saturday travel.\n\n")

cat("Codes p5_13, weekday and Saturday combined:\n")
print(table(tviaje$p5_13, useNA = "ifany"))

viajes_es <- tviaje[p5_3 == 1]
ttrans_es <- ttrans[p5_3 == 1]

ttrans_es[, tiempo_seg_min := fifelse(
  !is.na(p5_16_1_1) & !is.na(p5_16_1_2) &
    p5_16_1_1 >= 0 & p5_16_1_1 <= 23 &
    p5_16_1_2 >= 0 & p5_16_1_2 <= 59,
  p5_16_1_1 * 60 + p5_16_1_2,
  NA_real_
)]

trip_time <- ttrans_es[, .(
  tiempo_viaje_min = if (sum(!is.na(tiempo_seg_min)) > 0L) sum(tiempo_seg_min, na.rm = TRUE) else NA_real_,
  n_modos = uniqueN(p5_14[!is.na(p5_14)]),
  n_registros_transporte = .N
), by = id_via]
trip_time[tiempo_viaje_min < 0 | tiempo_viaje_min >= 300, tiempo_viaje_min := NA_real_]

tviaje_keep <- c("id_via", "id_soc", "n_via", "p5_6", "p5_13", "dto_origen", "dto_dest", "factor", optional_tviaje)
viajes_model <- merge(viajes_es[, ..tviaje_keep], trip_time, by = "id_via", all.x = TRUE)
setorder(viajes_model, id_soc, n_via)

if (all(c("p5_9_1", "p5_9_2", "p5_10_1", "p5_10_2") %in% names(viajes_model))) {
  viajes_model[, start_min := fifelse(p5_9_1 <= 23 & p5_9_2 <= 59, p5_9_1 * 60 + p5_9_2, NA_real_)]
  viajes_model[, end_min := fifelse(p5_10_1 <= 23 & p5_10_2 <= 59, p5_10_1 * 60 + p5_10_2, NA_real_)]
  viajes_model[, dur_clock_min := end_min - start_min]
  viajes_model[!is.na(dur_clock_min) & dur_clock_min < 0, dur_clock_min := dur_clock_min + 24 * 60]
  diag_time <- viajes_model[!is.na(tiempo_viaje_min) & !is.na(dur_clock_min), .(
    n = .N,
    median_modal_time = median(tiempo_viaje_min),
    median_clock_time = median(dur_clock_min),
    median_abs_diff = median(abs(tiempo_viaje_min - dur_clock_min))
  )]
  print(diag_time)
  fwrite(diag_time, file.path(out_dir, "diag_time_modal_vs_clock_v4.csv"))
}

if ("p5_11a" %in% names(viajes_model)) {
  viajes_model[, dest_home := as.integer(p5_13 == 1 | p5_11a == 1)]
} else {
  viajes_model[, dest_home := as.integer(p5_13 == 1)]
}
viajes_model[is.na(dest_home), dest_home := 0L]
viajes_model[, valid_dest := as.integer(!is.na(p5_13) & p5_13 != 99)]
viajes_model[, nonhome_dest := as.integer(valid_dest == 1 & dest_home == 0)]
viajes_model[, nonmandatory_dest := as.integer(p5_13 %in% 4:10)]
viajes_model[, care_dest := as.integer(p5_13 %in% c(6, 8))]
viajes_model[, tour_id := cumsum(shift(dest_home == 1, fill = FALSE)) + 1L, by = id_soc]

tours_by_person_tour <- viajes_model[, .(
  n_trips_tour = .N,
  n_nonhome = sum(nonhome_dest, na.rm = TRUE),
  n_nonmandatory = sum(nonmandatory_dest, na.rm = TRUE),
  n_care = sum(care_dest, na.rm = TRUE),
  time_tour_min = if (sum(!is.na(tiempo_viaje_min)) > 0L) sum(tiempo_viaje_min, na.rm = TRUE) else NA_real_
), by = .(id_soc, tour_id)]

tours_by_person_tour[, tour_chain := as.integer(n_nonhome >= 2)]
tours_by_person_tour[, paradas_intermedias_tour := pmax(n_nonhome - 1L, 0L)]

tours_person <- viajes_model[, .(
  n_viajes = .N,
  num_paradas_dia = sum(nonmandatory_dest, na.rm = TRUE),
  care_stops = sum(care_dest, na.rm = TRUE),
  tiempo_dia_min = if (sum(!is.na(tiempo_viaje_min)) > 0L) sum(tiempo_viaje_min, na.rm = TRUE) else NA_real_,
  props = paste(p5_13, collapse = "-")
), by = id_soc]

tour_person_summary <- tours_by_person_tour[, .(
  trip_chain_tour = as.integer(any(tour_chain == 1, na.rm = TRUE)),
  num_paradas_tour = sum(paradas_intermedias_tour, na.rm = TRUE),
  max_nonhome_in_tour = max(n_nonhome, na.rm = TRUE),
  n_tours = .N
), by = id_soc]

tours_person <- merge(tours_person, tour_person_summary, by = "id_soc", all.x = TRUE)
tours_person[, trip_chain_dia := as.integer(n_viajes >= 2 & num_paradas_dia >= 1)]
tours_person[, trip_chain := trip_chain_tour]
tours_person[, tour_complex := fcase(
  num_paradas_tour == 0, 0L,
  num_paradas_tour == 1, 1L,
  num_paradas_tour == 2, 2L,
  num_paradas_tour >= 3, 3L,
  default = NA_integer_
)]
tours_person[, care_trip := as.integer(care_stops > 0)]

cat("\nOutcome comparison among weekday travelers:\n")
print(tours_person[, .(
  n = .N,
  trip_chain_tour = mean(trip_chain_tour, na.rm = TRUE),
  trip_chain_dia = mean(trip_chain_dia, na.rm = TRUE),
  paradas_tour_mean = mean(num_paradas_tour, na.rm = TRUE),
  paradas_dia_mean = mean(num_paradas_dia, na.rm = TRUE)
)])

cat("\nTSdem/person coverage diagnostic:\n")
ids_tours <- unique(tours_person$id_soc)
diag_person_coverage <- data.table(
  n_tsdem = nrow(tsdem),
  ids_tsdem = uniqueN(tsdem$id_soc),
  n_tours_person = nrow(tours_person),
  ids_tours_person = uniqueN(tours_person$id_soc),
  tsdem_ids_without_weekday_trips = sum(!(unique(tsdem$id_soc) %in% ids_tours)),
  p5_4_zero = sum(tsdem$p5_4 == 0, na.rm = TRUE),
  p5_4_positive = sum(tsdem$p5_4 > 0, na.rm = TRUE),
  p5_4_missing = sum(is.na(tsdem$p5_4))
)
print(diag_person_coverage)
fwrite(diag_person_coverage, file.path(out_dir, "diag_person_coverage_v4.csv"))

tsdem[, trabaja := as.integer(p3_7 %in% c(1, 2) | p3_8 %in% c(1, 2, 3))]
tsdem[is.na(trabaja), trabaja := 0L]
tsdem[, mujer := as.integer(sexo == 2)]
tsdem[, niv_f := make_factor_with_missing(niv)]
tsdem[, viaja_es_reportado := as.integer(!is.na(p5_4) & p5_4 > 0)]

tsdem_keep <- c("id_soc", "id_hog", "sexo", "mujer", "edad", "parentesco", "niv", "niv_f", "p3_7", "p3_8", 
                "trabaja", "p4_2", "p5_4", "viaja_es_reportado", optional_tsdem)
tsdem_keep <- unique(c(tsdem_keep, "factor"))
base <- merge(
  tsdem[, ..tsdem_keep],
  tours_person,
  by = "id_soc",
  all.x = TRUE
)
setnames(base, "factor", "factor_per")
if (!"viaja_es_reportado" %in% names(base)) {
  base[, viaja_es_reportado := as.integer(!is.na(p5_4) & p5_4 > 0)]
}

zero_cols <- c("n_viajes", "num_paradas_dia", "care_stops", "trip_chain_tour", "num_paradas_tour", "max_nonhome_in_tour", 
               "n_tours", "trip_chain_dia", "trip_chain", "care_trip")
for (v in intersect(zero_cols, names(base))) base[is.na(get(v)), (v) := 0L]
base[is.na(tour_complex), tour_complex := 0L]
base[, viaja_es := as.integer(n_viajes > 0 | viaja_es_reportado == 1)]
base[is.na(viaja_es), viaja_es := 0L]

diag_generacion <- base[, .(
  n_tsdem = .N,
  n_personas_con_viajes_en_tviaje = sum(n_viajes > 0, na.rm = TRUE),
  n_p5_4_cero = sum(!is.na(p5_4) & p5_4 == 0, na.rm = TRUE),
  n_viaja_es_cero = sum(viaja_es == 0, na.rm = TRUE),
  pct_viaja_es = weighted.mean(viaja_es == 1, factor_per, na.rm = TRUE) * 100,
  min_p5_4 = suppressWarnings(min(p5_4, na.rm = TRUE)),
  max_p5_4 = suppressWarnings(max(p5_4, na.rm = TRUE))
)]
print(diag_generacion)
fwrite(diag_generacion, file.path(out_dir, "diag_generacion_script0_v4.csv"))
if (diag_generacion$n_viaja_es_cero == 0L) {
  warning("viaja_es has no zero values in this extract. Do not estimate a binary travel-generation model; use n_viajes as trip-generation count.")
}

trip_count_check <- base[!is.na(p5_4), .(
  n = .N,
  pct_equal_p5_4 = mean(n_viajes == p5_4, na.rm = TRUE),
  mean_diff_nviajes_p5_4 = mean(n_viajes - p5_4, na.rm = TRUE)
)]
print(trip_count_check)
fwrite(trip_count_check, file.path(out_dir, "diag_trip_count_vs_p5_4_v4.csv"))

base <- merge(
  base,
  thogar[, .(id_hog, distrito, autos_hogar = p2_1_1, factor_hog = factor, ent, mun)],
  by = "id_hog",
  all.x = TRUE
)
base[!(autos_hogar %in% 0:8), autos_hogar := NA_real_]
base[, sin_auto := as.integer(autos_hogar == 0)]

base[, ent_f := factor(ent)]
base[, mun_f := factor(sprintf("%02d%03d", as.integer(ent), as.integer(mun)))]

hogar_comp <- tsdem[, .(
  menores_12 = sum(!is.na(edad) & edad < 12, na.rm = TRUE),
  adultos_65mas = sum(!is.na(edad) & edad >= 65, na.rm = TRUE),
  jefa_mujer = as.integer(sexo[parentesco == 1][1] == 2)
), by = id_hog]
hogar_comp[is.na(jefa_mujer), jefa_mujer := 0L]
hogar_comp[, hay_menores12 := as.integer(menores_12 > 0)]
hogar_comp[, hay_adultos65 := as.integer(adultos_65mas > 0)]

base <- merge(base, hogar_comp, by = "id_hog", all.x = TRUE)
for (v in c("menores_12", "adultos_65mas", "jefa_mujer", "hay_menores12", "hay_adultos65")) base[is.na(get(v)), (v) := 0L]

valid_district <- function(x) !is.na(x) & x >= 1 & x <= 300

tiempo_dist_work <- viajes_model[
  p5_13 == 2 & valid_district(dto_origen) & !is.na(tiempo_viaje_min) & tiempo_viaje_min > 0 & tiempo_viaje_min < 300,
  .(tiempo_med_work = weighted.mean(tiempo_viaje_min, w = factor, na.rm = TRUE), n_work_trips = .N),
  by = .(distrito = dto_origen)
]

tiempo_dist_all <- viajes_model[
  valid_district(dto_origen) & !is.na(tiempo_viaje_min) & tiempo_viaje_min > 0 & tiempo_viaje_min < 300,
  .(tiempo_med_all = weighted.mean(tiempo_viaje_min, w = factor, na.rm = TRUE), n_all_trips = .N),
  by = .(distrito = dto_origen)
]

tiempo_dist <- merge(tiempo_dist_all, tiempo_dist_work, by = "distrito", all = TRUE)
tiempo_dist[, tiempo_med := fcoalesce(tiempo_med_work, tiempo_med_all)]
p75_tiempo <- quantile(tiempo_dist$tiempo_med, 0.75, na.rm = TRUE)
tiempo_dist[, periferia := as.integer(tiempo_med >= p75_tiempo)]

base <- merge(base, tiempo_dist[, .(distrito, tiempo_med, tiempo_med_work, tiempo_med_all, periferia)], 
              by = "distrito", all.x = TRUE)
base[is.na(periferia), periferia := 0L]

cat("\nDistrict-level p75 for periferia:\n")
print(p75_tiempo)

base[, pobreza_multi := 0L]
base[, alta_desoc := 0L]
base[, n_crit_desv_prelim := rowSums(.SD, na.rm = TRUE), .SDcols = c("sin_auto", "periferia")]
base[, desventaja := as.integer(n_crit_desv_prelim >= 1)]
base[, mujer_x_desventaja := mujer * desventaja]
base[, mujer_x_menores := mujer * hay_menores12]
base[, mujer_x_sinauto_x_perif := mujer * sin_auto * periferia]

base_model <- base[!is.na(factor_per) & factor_per > 0 & !is.na(edad) & !is.na(mujer) & !is.na(sin_auto)]

diag_base <- base_model[, .(
  n_persons = .N,
  pct_weekday_travel = weighted.mean(viaja_es == 1, factor_per, na.rm = TRUE) * 100,
  pct_trip_chain_tour_all = weighted.mean(trip_chain_tour == 1, factor_per, na.rm = TRUE) * 100,
  pct_trip_chain_dia_all = weighted.mean(trip_chain_dia == 1, factor_per, na.rm = TRUE) * 100,
  pct_trip_chain_tour_travelers = weighted.mean(trip_chain_tour[n_viajes > 0] == 1, factor_per[n_viajes > 0], na.rm = TRUE) * 100,
  pct_sin_auto = weighted.mean(sin_auto == 1, factor_per, na.rm = TRUE) * 100,
  pct_periferia = weighted.mean(periferia == 1, factor_per, na.rm = TRUE) * 100
)]
print(diag_base)
fwrite(diag_base, file.path(out_dir, "diag_base_script0_v4.csv"))

fwrite(base, file.path(out_dir, "base_analitica_eod2017_script0_v4.csv"))
fwrite(viajes_model, file.path(out_dir, "viajes_model_script0_v4.csv"))
fwrite(tours_by_person_tour, file.path(out_dir, "tours_by_person_tour_script0_v4.csv"))

cat("\nScript 0 v4 finished. Main objects: base, viajes_model, tours_by_person_tour.\n")

