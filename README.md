# Gender, disadvantage, and trip chaining in the ZMVM — Replication repository

Replication materials for: Oliva Vázquez, B. & Tapia Reyes, J.E., "Care constraints override material disadvantage: gender, trip chaining, and household disadvantage in the Mexico City Metropolitan Area" (manuscript submitted to *Journal of Transport Geography*).

## Repository structure

```
00_config.R       Single configuration file: paths and parameters. EDIT ONLY THIS.
00_funciones.R    Shared helper functions (sourced by all scripts).
Script_0.R        EOD 2017 raw tables -> analytical base (tours, sociodemographics).
Script_1.R        CONEVAL + ENOE integration; disadvantage index (>=2 of 4 criteria);
                  models M1-M7 (survey::svyglm, complex design); tables; Spanish figures.
Script_2.R        Journal-version robustness + English figures (self-contained on
                  Script_1 CSV outputs): Firth logit, threshold sensitivity,
                  district correlations, Figure 1 EN, district maps EN.
run_all.R         Runs the full pipeline in order.
robustez.py       Optional: independent Python cross-validation of Script_2
                  (reproduces M2 focal coefficients to 4 decimals).
Data/             Raw public data (not redistributed; see below).
Output/           Outputs of Scripts 0-1.
Output_journal/   Outputs of Script 2.
```

Set the repo root once: either run R with the working directory at the repo root, or set the environment variable `REPO_ZMVM`, or edit `00_config.R`.

## Expected focal results

| Check | Result |
|---|---|
| M2 (>=2 criteria) | woman x disadvantage 0.3046 (SE 0.0509, OR 1.36); woman x children 0.3645 (OR 1.44) |
| Firth vs MLE | max abs. difference on focal terms = 0.00036 |
| Threshold >=1 (78.8% pop.) | woman x disadvantage 0.226 (OR 1.25), p < 0.001 |
| Threshold >=3 (9.0% pop.) | woman x disadvantage 0.374 (OR 1.45), p < 0.001 |
| District correlations (194 districts, traveler-weighted) | chain x disadvantage r = -0.24 (men -0.31, women -0.16); gender gap x disadvantage r = +0.16 |

## Data sources (public, not redistributed)

Place under `Data/` as follows:

- `Data/eod_2017_csv/` — EOD 2017, INEGI: tables `tviaje`, `tsdem`, `thogar`, `ttransporte`. [download link]
- `Data/Distritos_EOD_2017/` — shapefile of the 194 ZMVM transport districts. [download link]
- CONEVAL municipal poverty (2015 measurement) and ENOE Municipal Labor Indicators 2017-Q1: paths as referenced in `Script_1.R`. [download links]

## Software

- R >= 4.2: `data.table`, `survey`, `readxl`, `ggplot2`, `sf`, `MASS`, `scales`, `stargazer`, `logistf`.
- Python >= 3.10 (optional cross-validation): `numpy`, `pandas`, `patsy`.

## Status

- Scripts 0-2 fully executed end-to-end, including Section G: the four English maps and Figure 1 EN are in `Output_journal/`; Firth validated (max diff 0.00036).
- Pending: mint the repository DOI (Zenodo/OSF) and insert it in the manuscript Data availability statement.
