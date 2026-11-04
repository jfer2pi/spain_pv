# Spain Solar PV Fleet

An R/Shiny dashboard of Spain's utility-scale solar PV fleet and development pipeline, built on open data with no API keys.

Created by Fernando ([@jfer2pi](https://github.com/jfer2pi)), with coding help from Claude (Anthropic).

It is the fifth app in a series, alongside the [US](https://github.com/jfer2pi/eia_solar_pv), [Mexico](https://github.com/jfer2pi/mexico_pv), [Brazil](https://github.com/jfer2pi/brazil_pv) and [Australia](https://github.com/jfer2pi/australia_pv) Solar PV Fleet apps. It shares their Linear-style dark theme and layout: KPI cards, a bubble map of sites by stage, the capacity trajectory, capacity by autonomous community, site size distribution, capacity by owner and a site table. The whole app is one self-contained `app.R`.

## Data

| Source | Used for | Access |
|---|---|---|
| [Global Energy Monitor, Global Solar Power Tracker](https://globalenergymonitor.org/projects/global-solar-power-tracker/) | Every Spanish solar PV phase GEM tracks: status, MW, owner, operator, start year, coordinates, province and autonomous community | Form-gated .xlsx, committed next to `app.R` (CC BY 4.0) |
| [Red Eléctrica REData](https://www.ree.es/es/apidatos) | National installed solar PV by year (all sizes, including rooftop), as a reference line on the trajectory chart | Keyless API, cached in `es_ree_pv.csv` |

The February 2026 tracker lists 3,990 Spanish utility-scale PV phases (1 MW and up): 36.2 GW operating, 7.6 GW under construction, 91.7 GW in pre-construction and 18.2 GW announced, plus shelved and cancelled projects. Solar thermal is excluded. About two thirds of entries have no owner, and most pre-construction projects have no start year, so they sit under "sites without a start year".

**Stages** follow GEM: Operating, Under construction, Pre-construction, Announced, Shelved and Cancelled. Phases are rolled up to one bubble per site and stage. Pipeline phases with a start year already past are shown in the current year.

**Startup.** Posit Connect Cloud allows 60 seconds for an app to start listening. The app reads the pre-built `es_solar.rds` when it matches the committed workbook; otherwise it starts at once, shows a loading page, reads the workbook in that session and reloads into the dashboard.

## How it was built

1. **First attempt: RAIPEE.** The app was first built on RAIPEE, Spain's registry of production installations, downloaded at runtime from the Ministry's ELECTRA service. The export turned out to be a thin list (an ID, the regime, the installation name and the autonomous community) with no capacity, technology, municipality or dates, so it can't drive a dashboard.
2. **Switch to GEM.** Global Energy Monitor's tracker has Spain plant by plant with coordinates, MW, stages and owners, and is already the source of the Mexico app, so the Spain app reuses that GEM reader without the Mexico app's permit matching.
3. **Startup time.** The first deploys missed Connect Cloud's 60-second start limit because data was built before Shiny was listening. The app now starts first and builds its data in the session when no pre-built file is available, and a GitHub Action pre-builds that file.

## Run locally

```r
install.packages(c("shiny", "bslib", "httr2", "readxl", "dplyr", "tidyr",
                   "leaflet", "plotly", "DT", "htmltools"))
shiny::runApp()
```

## Updating the data

GEM publishes new tracker releases a few times a year behind a short form. Download the new `.xlsx`, put it next to `app.R` (any name matching `*solar*tracker*.xlsx`; the newest is used), remove the old one and push. The *Refresh data* GitHub Action (monthly, or on demand from the Actions tab) refreshes Red Eléctrica's figures, pre-builds `es_solar.rds` and rewrites `manifest.json`. The *Write manifest* Action also rewrites the manifest on every push that changes `app.R` or a data file.

## Deploy on Posit Connect Cloud

Create a new Shiny app from this repo, branch `main`, primary file `app.R`. `manifest.json` lists `app.R`, the GEM workbook, `es_ree_pv.csv` and `es_solar.rds`.

## Credits

Plants: Global Energy Monitor, Global Solar Power Tracker, CC BY 4.0. National installed capacity: Red Eléctrica (REData). Map data © [OpenStreetMap](https://www.openstreetmap.org/copyright) contributors, ODbL.
