# Spain Solar PV Fleet

An R/Shiny dashboard of Spain's solar PV installations, registered and pre-registered, built on the national production registry with no API keys.

Created by Fernando ([@jfer2pi](https://github.com/jfer2pi)), with coding help from Claude (Anthropic).

It is the fifth app in a series, alongside the [US](https://github.com/jfer2pi/eia_solar_pv), [Mexico](https://github.com/jfer2pi/mexico_pv), [Brazil](https://github.com/jfer2pi/brazil_pv) and [Australia](https://github.com/jfer2pi/australia_pv) Solar PV Fleet apps, with the same Linear-style dark theme. The whole app is one self-contained `app.R` that downloads its own data, like the US app.

## Data

| Source | Used for | Access |
|---|---|---|
| [RAIPEE](https://datos.gob.es/es/catalogo/e05068001-registro-de-productores-de-energia-electrica), the Registro Administrativo de Instalaciones de Producción de Energía Eléctrica | Every registered solar PV installation: holder, MW, municipality, province, dates, registration type | Daily CSV/XLSX export from the Ministry's ELECTRA service, downloaded at startup |
| [Wikidata](https://query.wikidata.org) | Coordinates of every Spanish municipality, by INE code | Keyless SPARQL, cached in `es_municipios.csv` |
| [Red Eléctrica REData](https://www.ree.es/es/apidatos) | National installed solar PV by year, as a reference line | Keyless API |

**Stages:** inscripción definitiva is *Registered*; inscripción previa is *Pre-registered* (authorised, not yet in service). Deregistered and cancelled entries are dropped.

**Locations:** RAIPEE gives the municipality, not coordinates, so installations are placed at their municipality's centroid and the map shows one bubble per municipality and stage. Municipalities are matched by INE code when the export has one, otherwise by name within the province, allowing for bilingual names and articles ("Puerto, El").

**Startup:** Posit Connect Cloud gives an app 60 seconds to start, so at startup the app reads committed files (the pre-built `es_solar.rds`, `raipee_produccion.csv`, `es_municipios.csv`, `es_ree_pv.csv`) and only downloads what is missing. The sidebar button and the monthly *Refresh data* GitHub Action download everything again and commit the results.

**Downloads and fallbacks:** the app tries the ELECTRA CSV, then the Excel export. If neither answers, it reads the newest `raipee*.csv` or `raipee*.xlsx` next to `app.R`. The parsed data is cached in `es_solar.rds` for 24 h; the sidebar button re-downloads it. If no data can be loaded, the app opens a page explaining what to add instead of failing to start.

## How it was built

1. **Choosing the source.** RAIPEE is Spain's official registry of generating installations, published daily as open data, so it plays the role EIA-860M plays for the US app. Fernando asked for the app to fetch it at runtime the same way.
2. **Not seen yet.** The ELECTRA export couldn't be downloaded from the build environment (its proxy blocks the Ministry's servers), so the reader matches columns by pattern (potencia, municipio, provincia, titular, fecha...) and detects delimiter, encoding and kW/MW. The first real export will be checked against it, as was done for the Brazil and Australia apps.
3. **Locations without geocoding.** Rather than geocoding thousands of installations, the app uses municipality centroids from Wikidata, one keyless query for all ~8,100 municipalities.

4. **Startup time.** The first Connect Cloud deploy still missed the 60-second limit because, with no data in the repo, the RAIPEE download ran before Shiny was listening. Now the app only warms up at startup when cached data is already there; otherwise it starts at once, shows a loading page, builds the data in the session and reloads into the dashboard.
5. **Startup time (Australia).** The Australia app's deploy showed that waiting on blocked downloads at startup breaks Connect Cloud's 60-second limit, so this app starts from committed files and pre-built data, with 8-second connection timeouts.

## Run locally

```r
install.packages(c("shiny", "bslib", "httr2", "jsonlite", "readxl", "dplyr", "tidyr",
                   "leaflet", "plotly", "DT", "htmltools"))
shiny::runApp()
```

`options(es_pv.offline = TRUE)` skips all downloads and reads local files only.

## Deploy on Posit Connect Cloud

ELECTRA and Wikidata may refuse cloud servers, as AEMO and ANEEL did for the sibling apps. After the first local run, commit `es_municipios.csv` and a copy of the RAIPEE export (`raipee_produccion.csv`) so the deployed app always has data. The *Write manifest* GitHub Action regenerates `manifest.json` on every push that changes `app.R` or a data file. To write it by hand:

```r
rsconnect::writeManifest(appFiles = c("app.R", list.files(
  pattern = "(^es_municipios\\.csv|raipee.*\\.(csv|xlsx))$", ignore.case = TRUE)))
```

## Credits

Installations: Ministerio para la Transición Ecológica y el Reto Demográfico / Ministerio de Industria y Turismo, RAIPEE via ELECTRA. Municipality coordinates: Wikidata (CC0). National installed capacity: Red Eléctrica (REData). Map data © [OpenStreetMap](https://www.openstreetmap.org/copyright) contributors, ODbL.
