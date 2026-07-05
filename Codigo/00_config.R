# ============================================================
# 00_config.R — Configuración única del repositorio
#
# Resolución de la raíz del repo (en este orden):
#   1. Variable de entorno REPO_ZMVM, si está definida.
#   2. El directorio de trabajo, si contiene la carpeta Data/.
#   3. El directorio padre, si contiene Data/ (caso típico:
#      los scripts viven en Repo/Codigo/ y los datos en Repo/Data/).
#   4. Si nada de lo anterior aplica, edita manualmente abajo.
# ============================================================

repo <- Sys.getenv("REPO_ZMVM", unset = "")
if (!nzchar(repo)) {
  candidatos <- c(getwd(), dirname(getwd()))
  hit <- candidatos[dir.exists(file.path(candidatos, "Data"))]
  repo <- if (length(hit) > 0L) hit[1] else getwd()
  # Alternativa manual: repo <- "/ruta/a/tu/Repo"
}
if (!dir.exists(file.path(repo, "Data"))) {
  warning("No se encontró la carpeta Data/ bajo la raíz resuelta: ", repo,
          "\nDefine REPO_ZMVM o edita 00_config.R.")
}
cat("[config] Raíz del repo:", repo, "\n")

# --- Datos crudos (públicos, no redistribuidos; ver README) ---
ruta     <- file.path(repo, "Data/eod_2017_csv")                     # EOD 2017 (INEGI)
shp_path <- file.path(repo, "Data/Distritos_EOD_2017/DistritosEODHogaresZMVM2017.shp")
# CONEVAL y ENOE: rutas construidas dentro de Script_1 a partir de `repo`

# --- Salidas ---
out_dir     <- file.path(repo, "Output")          # Scripts 0 y 1
out_journal <- file.path(repo, "Output_journal")  # Script 2 (versión de revista)
dir.create(out_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(out_journal, showWarnings = FALSE, recursive = TRUE)

# --- Parámetros del análisis ---
UMBRAL_DESVENTAJA <- 2L     # criterios (de 4) para clasificar hogar en desventaja
PCTL_CORTES       <- 0.75   # percentil para cortes de pobreza y desocupación
