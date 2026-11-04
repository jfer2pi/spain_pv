# =============================================================================
# Spain Solar PV Fleet: operating plants and development pipeline
# -----------------------------------------------------------------------------
# One self-contained file. No API keys anywhere.
#
# Data (read from files in this folder, cached to es_solar.rds):
#   1. Global Energy Monitor, Global Solar Power Tracker (CC BY 4.0). Required.
#      Phase-level status, MW, owner, start year, coordinates, province and
#      autonomous community for utility-scale PV (1 MW+) and smaller sites.
#      GEM gates the download behind a short form, so it cannot be fetched
#      automatically. Download the .xlsx once from
#      https://globalenergymonitor.org/projects/global-solar-power-tracker/download-data/
#      and drop it in this folder (any file name matching *solar*tracker*.xlsx).
#      The newest such file is used; replacing it triggers a rebuild.
#   2. Red Eléctrica REData API (keyless): national installed solar PV by year
#      (all sizes, including rooftop), drawn on the trajectory chart as a
#      reference. Cached in es_ree_pv.csv; the app still runs without it.
#
# Startup: Posit Connect Cloud allows 60 s for the app to start listening, and
# reading the 16 MB GEM workbook can take a while, so when there is no fresh
# es_solar.rds the app starts at once, shows a loading page, builds the data
# in that session and reloads into the dashboard.
#
# Map:   OpenStreetMap standard tiles (dark-filtered + standard). Light use
#        only, per https://operations.osmfoundation.org/policies/tiles/
# Theme: Linear-style dark UI, matching the US Solar PV Fleet app.
#
# install.packages(c("shiny","bslib","httr2","readxl","dplyr","tidyr",
#                    "leaflet","plotly","DT","htmltools"))
# shiny::runApp()
# =============================================================================

library(shiny)
library(bslib)
library(httr2)
library(readxl)
library(dplyr)
library(tidyr)
library(leaflet)
library(plotly)
library(DT)
library(htmltools)

# ---- Config -----------------------------------------------------------------

CACHE_FILE    <- "es_solar.rds"
CACHE_TTL     <- 24 * 3600
CACHE_VERSION <- 1L   # bump when the cached structure changes
GEM_PATTERN   <- "solar.*tracker.*\\.xlsx$"
GEM_URL       <- "https://globalenergymonitor.org/projects/global-solar-power-tracker/download-data/"
REE_API       <- "https://apidatos.ree.es/es/datos/generacion/potencia-instalada"
REE_FILE      <- "es_ree_pv.csv"
CONNECT_TIMEOUT <- 8

STAGES <- c("Operating", "Under construction", "Pre-construction",
            "Announced", "Shelved", "Cancelled")
STAGE_COLS <- c(
  "Operating"          = "#5E6AD2",  # Linear indigo
  "Under construction" = "#F2994A",  # orange
  "Pre-construction"   = "#F2C94C",  # yellow
  "Announced"          = "#B59AF8",  # lavender
  "Shelved"            = "#8A8F98",  # grey
  "Cancelled"          = "#EB5757"   # red
)
PIPELINE       <- c("Under construction", "Pre-construction", "Announced")
STALLED        <- c("Shelved", "Cancelled")
DEFAULT_STAGES <- c("Operating", "Under construction", "Pre-construction")
CY             <- as.integer(format(Sys.Date(), "%Y"))

# ---- Small helpers ----------------------------------------------------------

clean_names <- function(x) {
  x <- iconv(as.character(x), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^a-z0-9]+", "_", tolower(trimws(x)))
  gsub("^_|_$", "", x)
}

norm_txt <- function(x) {
  x <- iconv(tolower(as.character(x)), "UTF-8", "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", x)))
}

# Numbers written either 1,234.5 or 1234,5
num_mx <- function(x) {
  x <- gsub("[^0-9,.-]", "", as.character(x))
  both <- grepl(",", x) & grepl("\\.", x)
  x[both] <- gsub(",", "", x[both])
  one <- grepl(",", x) & !both
  x[one] <- ifelse(grepl(",[0-9]{3}$", x[one]), gsub(",", "", x[one]), sub(",", ".", x[one]))
  suppressWarnings(as.numeric(x))
}

# First column whose cleaned name matches any of the regexes, in order
pickr <- function(d, ...) {
  for (p in c(...)) {
    nm <- grep(p, names(d), value = TRUE, perl = TRUE)
    if (length(nm)) return(as.character(d[[nm[1]]]))
  }
  rep(NA_character_, nrow(d))
}

na_dash <- function(x) ifelse(is.na(x) | !nzchar(trimws(x)), "-", x)

find_file <- function(pattern) {
  f <- list.files(".", pattern = pattern, ignore.case = TRUE, full.names = TRUE)
  f <- f[!grepl("^~\\$", basename(f))]          # skip Excel lock files
  if (!length(f)) return(NULL)
  f[which.max(file.mtime(f))]
}

# Name and size only: modification times change on every checkout or deploy
file_sig <- function(p) if (is.null(p)) "" else paste(basename(p), file.size(p))

# ---- 1. GEM Global Solar Power Tracker --------------------------------------

stage_from_gem <- function(s) {
  s <- trimws(tolower(s))
  out <- case_when(
    s == "operating"                             ~ "Operating",
    grepl("construction", s) & !grepl("^pre", s) ~ "Under construction",
    grepl("^pre", s)                             ~ "Pre-construction",
    s == "announced"                             ~ "Announced",
    grepl("^shelved|^mothballed", s)             ~ "Shelved",
    grepl("^cancel", s)                          ~ "Cancelled",
    TRUE                                         ~ NA_character_     # retired etc.
  )
  factor(out, levels = STAGES)
}

# The workbook has one data sheet per size band (e.g. "20 MW+", "1-20 MW") plus
# About sheets. Read every sheet that has a Latitude header.
read_gem <- function(path) {
  rows <- lapply(excel_sheets(path), function(sh) {
    head20 <- suppressMessages(read_excel(path, sheet = sh, col_names = FALSE, n_max = 20,
                                          col_types = "text", .name_repair = "minimal"))
    if (!nrow(head20)) return(NULL)
    hdr <- which(apply(head20, 1, \(r) any(tolower(trimws(r)) %in% "latitude")))[1]
    if (is.na(hdr)) return(NULL)
    d <- suppressMessages(read_excel(path, sheet = sh, skip = hdr - 1, col_types = "text",
                                     .name_repair = "minimal"))
    names(d) <- make.unique(clean_names(names(d)))
    d
  })
  d <- bind_rows(rows)
  if (!nrow(d)) stop("No data sheet with a Latitude column in ", basename(path), call. = FALSE)
  d <- d[norm_txt(pickr(d, "^country")) == "spain", ]

  g <- tibble(
    loc_id       = pickr(d, "gem_location_id", "location_id"),
    phase_id     = pickr(d, "gem_phase_id", "phase_id"),
    plant        = pickr(d, "^project_name$", "^project_name"),
    phase        = pickr(d, "^phase_name"),
    owner        = pickr(d, "^owner_s$", "^owner$", "^owner"),
    operator     = pickr(d, "^operator"),
    local_name   = pickr(d, "^project_name_in_local"),
    other_names  = pickr(d, "^other_name"),
    other_ids    = pickr(d, "^other_ids_unit", "^other_ids"),
    state        = pickr(d, "^state_province", "^subnational"),
    city         = pickr(d, "^city"),
    local_area   = pickr(d, "^local_area"),
    major_area   = pickr(d, "^major_area"),
    status       = tolower(trimws(pickr(d, "^status"))),
    tech         = pickr(d, "^technology"),
    mw           = num_mx(pickr(d, "^capacity_mw", "^capacity$", "^capacity(?!_rating)")),
    rating       = pickr(d, "^capacity_rating"),
    year         = as.integer(num_mx(pickr(d, "^start_year"))),
    lat          = num_mx(pickr(d, "^latitude")),
    lon          = num_mx(pickr(d, "^longitude")),
    accuracy     = tolower(trimws(pickr(d, "^location_accuracy"))),
    wiki         = pickr(d, "wiki")
  ) |>
    mutate(stage = stage_from_gem(status),
           province = coalesce(na_if(major_area, ""), NA_character_),
           municipality = coalesce(na_if(local_area, ""), na_if(city, "")),
           # "Iberdrola SA [100%]" -> "Iberdrola SA"; shared ownership keeps its shares
           owner = sub("\\s*\\[100(\\.0+)?%\\]$", "", trimws(owner)),
           owner = coalesce(na_if(owner, ""), na_if(trimws(operator), ""), "Unknown"),
           loc_id = coalesce(loc_id, paste(plant, round(lat, 3), round(lon, 3))),
           accuracy = coalesce(na_if(accuracy, ""), "approximate")) |>
    filter(!is.na(stage), !grepl("thermal", tolower(coalesce(tech, ""))),
           !is.na(mw), mw > 0, !is.na(lat), !is.na(lon))
  if (!nrow(g)) stop("GEM file has no Spanish PV rows: ", basename(path), call. = FALSE)
  g
}
# ---- 2. Red Eléctrica installed PV (reference) ------------------------------

# Committed copy first; the API only when there is none, or on a refresh
get_ree <- function(live = FALSE) {
  if (!live && file.exists(REE_FILE))
    return(tryCatch(as_tibble(read.csv(REE_FILE, colClasses = c("integer", "numeric"))),
                    error = \(e) NULL))
  r <- tryCatch(
    request(REE_API) |>
      req_url_query(start_date = "2010-01-01T00:00", end_date = sprintf("%d-12-31T23:59", CY),
                    time_trunc = "year") |>
      req_user_agent("es-solar-fleet-dashboard/1.0 (R/httr2)") |>
      req_options(connecttimeout = CONNECT_TIMEOUT) |> req_timeout(30) |>
      req_error(is_error = \(r) FALSE) |> req_perform(),
    error = \(e) NULL)
  if (is.null(r) || resp_status(r) != 200)
    return(if (file.exists(REE_FILE)) get_ree(FALSE) else NULL)
  inc <- tryCatch(resp_body_json(r, simplifyVector = FALSE)$included, error = \(e) NULL)
  k <- which(vapply(inc, \(x) grepl("fotovolt", tolower(paste(x$type, x$attributes$title))), TRUE))[1]
  if (is.na(k)) return(NULL)
  v <- inc[[k]]$attributes$values
  out <- tibble(year = as.integer(substr(vapply(v, \(x) x$datetime, ""), 1, 4)),
                mw = vapply(v, \(x) as.numeric(x$value), 0)) |>
    group_by(year) |> summarise(mw = last(mw), .groups = "drop")
  tryCatch(write.csv(out, REE_FILE, row.names = FALSE), error = \(e) NULL)
  out
}

# ---- Build + cache ----------------------------------------------------------

build_data <- function(live = FALSE) {
  gem_path <- find_file(GEM_PATTERN)
  if (is.null(gem_path))
    stop("No GEM Global Solar Power Tracker file in the app folder. Download the .xlsx from ",
         GEM_URL, " and save it next to app.R (name matching *solar*tracker*.xlsx).",
         call. = FALSE)
  list(version = CACHE_VERSION, gens = read_gem(gem_path), ree = get_ree(live),
       gem_file = basename(gem_path), gem_sig = file_sig(gem_path), built = Sys.time())
}

# Rebuild when the GEM file changes or the cache is older than a day
load_data <- function() {
  cached <- if (file.exists(CACHE_FILE)) tryCatch(readRDS(CACHE_FILE), error = \(e) NULL)
  fresh <- !is.null(cached) && identical(cached$version, CACHE_VERSION) &&
    identical(file_sig(find_file(GEM_PATTERN)), cached$gem_sig) &&
    difftime(Sys.time(), file.mtime(CACHE_FILE), units = "secs") < CACHE_TTL
  if (fresh) return(cached)
  out <- build_data()
  saveRDS(out, CACHE_FILE)
  out
}

# ---- Snapshot prep (everything the UI derives from one build) ---------------

prep_data <- function(s) {
  # Pipeline phases whose expected start year has already passed show as this year
  G <- s$gens |>
    mutate(year_plot = ifelse(stage %in% PIPELINE & !is.na(year) & year < CY, CY, year))
  yrs <- G$year_plot[!is.na(G$year_plot)]
  site_mw <- G |> group_by(loc_id, stage) |> summarise(mw = sum(mw), .groups = "drop") |> pull(mw)
  list(
    raw      = s,
    G        = G,
    yr_rng   = if (length(yrs)) range(yrs) else c(CY - 10, CY + 3),
    total_op = sum(G$mw[G$stage == "Operating"]),
    site_max = max(1, ceiling(max(c(site_mw, 1)))),
    states   = sort(unique(na.omit(G$state))),
    provs    = sort(unique(na.omit(G$province)))
  )
}

current_data <- local({
  key <- NULL; val <- NULL
  function(peek = FALSE) {
    if (peek) return(!is.null(val))
    s <- load_data()
    k <- format(s$built, "%Y-%m-%d %H:%M:%OS3")
    if (!identical(k, key)) { val <<- prep_data(s); key <<- k }
    val
  }
})
data_or_error <- function() tryCatch(current_data(), error = function(e) e)

# Data that is quick to get: already in memory, or a fresh cache for this GEM file
data_ready <- function() {
  current_data(peek = TRUE) || {
    c <- if (file.exists(CACHE_FILE)) tryCatch(readRDS(CACHE_FILE), error = \(e) NULL)
    !is.null(c) && identical(c$version, CACHE_VERSION) &&
      identical(file_sig(find_file(GEM_PATTERN)), c$gem_sig) &&
      difftime(Sys.time(), file.mtime(CACHE_FILE), units = "secs") < CACHE_TTL
  }
}

# Warm up at startup only when that is fast (see the header note)
if (data_ready()) invisible(data_or_error())

# ---- Linear design tokens ---------------------------------------------------

lin <- list(
  bg = "#08090A", surface = "#0F1011", raised = "#141516",
  border = "rgba(255,255,255,0.08)", text = "#F7F8F8",
  muted = "#8A8F98", faint = "#62666D", accent = "#5E6AD2"
)

SIZE_BREAKS <- c(0, 5, 20, 100, 250, Inf)
SIZE_LABELS <- c("< 5 MW", "5 to 20", "20 to 100", "100 to 250", "> 250 MW")

bubble_r <- function(mw) pmin(pmax(sqrt(mw) * 0.95, 2.5), 34)

theme <- bs_theme(
  version = 5, bg = lin$bg, fg = lin$text, primary = lin$accent,
  secondary = lin$raised,
  base_font    = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", local = FALSE),
  font_scale = 0.88,
  `border-radius` = "6px", `border-color` = lin$border,
  `card-bg` = lin$surface, `card-border-color` = lin$border,
  `input-bg` = lin$raised, `input-border-color` = lin$border
) |>
  bs_add_rules(sprintf("
  body { font-feature-settings:'cv11','ss01','tnum'; letter-spacing:-0.005em; }
  .navbar { background:%1$s !important; border-bottom:1px solid %4$s; }
  .brand { display:flex; align-items:center; gap:10px; font-weight:600; font-size:0.95rem; }
  .brand .logo { width:18px; height:18px; border-radius:5px;
                 background:linear-gradient(135deg,%5$s 0%%,#A5ABF2 100%%); }
  .brand .tag { color:%2$s; font-weight:400; font-size:0.78rem;
                border:1px solid %4$s; border-radius:999px; padding:1px 8px; }
  .bslib-sidebar-layout > .sidebar { border-right:1px solid %4$s !important; }
  .sec { color:%3$s; font-size:0.7rem; font-weight:500; text-transform:uppercase;
         letter-spacing:0.06em; margin:4px 0 6px; }
  .card { box-shadow:none; }
  .card-header { background:transparent; border-bottom:1px solid %4$s; font-size:0.8rem;
                 font-weight:500; color:%6$s; padding:10px 14px; }
  .card-header .sub { color:%3$s; font-weight:400; margin-left:6px; }
  .kpi { background:%7$s; border:1px solid %4$s; border-radius:8px; padding:14px 16px; height:100%%; }
  .kpi .lbl { color:%2$s; font-size:0.72rem; margin-bottom:6px; display:flex; align-items:center; gap:6px; }
  .kpi .dot { width:7px; height:7px; border-radius:50%%; display:inline-block; }
  .kpi .val { font-size:1.55rem; font-weight:600; letter-spacing:-0.02em; line-height:1.1; }
  .kpi .foot { color:%3$s; font-size:0.72rem; margin-top:6px; white-space:nowrap;
               overflow:hidden; text-overflow:ellipsis; }
  .form-label, .control-label { color:%2$s; font-size:0.75rem; font-weight:500; }
  .form-control, .selectize-input { font-size:0.82rem; }
  .selectize-input, .selectize-dropdown { background:#141516 !important; border-color:%4$s !important; }
  .selectize-input .item { background:rgba(94,106,210,0.18) !important; color:#C9CDFB !important;
                           border:0 !important; border-radius:4px !important; }
  .form-check-input { background-color:#141516; border-color:rgba(255,255,255,0.18); }
  .form-check-input:checked { background-color:%5$s; border-color:%5$s; }
  .form-check-label { font-size:0.8rem; }
  .stage-sw { display:inline-block; width:8px; height:8px; border-radius:50%%; margin-right:6px; }
  .irs--shiny .irs-bar { background:%5$s; border-color:%5$s; }
  .irs--shiny .irs-line { background:#1C1D1F; border-color:%4$s; }
  .irs--shiny .irs-handle { background:%6$s; border:0; box-shadow:0 0 0 3px rgba(94,106,210,0.35); }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background:%5$s; font-size:0.7rem; }
  .irs--shiny .irs-min, .irs--shiny .irs-max { background:#1C1D1F; color:%2$s; font-size:0.68rem; }
  .btn-lin { background:#141516; border:1px solid %4$s; color:%6$s; font-size:0.78rem; }
  .btn-lin:hover { background:#1C1D1F; color:%6$s; }
  .meta { color:%3$s; font-size:0.72rem; line-height:1.5; }
  .meta a { color:%2$s; }
  .osm-dark { filter: invert(100%%) hue-rotate(180deg) grayscale(55%%) brightness(0.82) contrast(1.05); }
  .leaflet-container { background:%1$s; font-family:Inter, sans-serif; }
  .leaflet-popup-content-wrapper, .leaflet-popup-tip { background:#141516; color:%6$s;
       border:1px solid %4$s; box-shadow:0 8px 24px rgba(0,0,0,0.5); }
  .leaflet-popup-content { margin:12px 14px; font-size:0.78rem; line-height:1.55; }
  .leaflet-popup-content a { color:#A5ABF2; }
  .pop-k { color:%2$s; }
  .pop-stage { display:inline-block; font-size:0.68rem; padding:1px 7px; border-radius:999px;
               border:1px solid currentColor; margin-bottom:4px; }
  .info.legend, .size-legend { background:rgba(15,16,17,0.92) !important; color:%6$s !important;
       border:1px solid %4$s; border-radius:6px; font-size:0.72rem; box-shadow:none; padding:8px 10px; }
  .size-legend .t { color:%2$s; margin-bottom:4px; }
  .leaflet-bar a, .leaflet-control-layers { background:#141516; color:%6$s; border-color:%4$s; }
  .leaflet-control-layers-expanded { font-size:0.75rem; }
  .leaflet-control-attribution { background:rgba(8,9,10,0.75) !important; color:%3$s; }
  .leaflet-control-attribution a { color:%2$s; }
  table.dataTable { font-size:0.78rem; }
  table.dataTable thead th { color:%2$s; font-weight:500; border-bottom:1px solid %4$s !important; }
  table.dataTable tbody td { border-top:1px solid rgba(255,255,255,0.04) !important; }
  table.dataTable tbody tr:hover > * { background:rgba(255,255,255,0.03) !important; }
  table.dataTable tbody tr.selected > * { box-shadow:inset 0 0 0 9999px rgba(94,106,210,0.22) !important;
       color:%6$s !important; }
  .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_filter label { color:%3$s; font-size:0.74rem; }
  .dt-buttons .btn { background:#141516; border:1px solid %4$s; color:%6$s; font-size:0.74rem; padding:3px 10px; }
  .page-link { background:transparent; border-color:%4$s; color:%2$s; }
  .active > .page-link { background:%5$s; border-color:%5$s; }
  ", lin$bg, lin$muted, lin$faint, lin$border, lin$accent, lin$text, lin$surface))

kpi <- function(label, color, value_id, foot_id) {
  div(class = "kpi",
      div(class = "lbl", span(class = "dot", style = paste0("background:", color)), label),
      div(class = "val", textOutput(value_id, inline = TRUE)),
      div(class = "foot", textOutput(foot_id, inline = TRUE)))
}

plotly_lin <- function(p, ...) {
  base <- list(
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, sans-serif", color = lin$muted, size = 11),
    margin = list(l = 8, r = 8, t = 6, b = 6),
    hoverlabel = list(bgcolor = lin$raised, bordercolor = "#2A2B2E",
                      font = list(color = lin$text, family = "Inter")),
    xaxis = list(gridcolor = "rgba(255,255,255,0.05)", zeroline = FALSE,
                 linecolor = lin$border, tickfont = list(color = lin$faint)),
    yaxis = list(gridcolor = "rgba(255,255,255,0.05)", zeroline = FALSE,
                 tickfont = list(color = lin$faint)),
    showlegend = FALSE
  )
  do.call(layout, c(list(p), modifyList(base, list(...)))) |>
    config(displayModeBar = FALSE)
}

fmt_mw <- function(x) {
  ifelse(x >= 1000, sprintf("%.1f GW", x / 1000),
         paste(formatC(x, format = "f", digits = 1, big.mark = ","), "MW"))
}

stage_labels <- lapply(STAGES, \(s) HTML(sprintf(
  "<span class='stage-sw' style='background:%s'></span>%s", STAGE_COLS[[s]], s)))

size_legend_html <- function() {
  refs <- c(10, 100, 500)
  r <- bubble_r(refs); h <- 2 * max(r) + 4
  circles <- paste0(sprintf(
    "<circle cx='%.1f' cy='%.1f' r='%.1f' fill='none' stroke='#8A8F98' stroke-width='1'/>
     <text x='%.1f' y='%.1f' fill='#8A8F98' font-size='10' text-anchor='middle'>%s</text>",
    max(r) + 2, h - r - 2, r, max(r) + 2, h - 2 * r - 5,
    c("10", "100", "500 MW")), collapse = "")
  sprintf("<div class='size-legend'><div class='t'>Bubble area = MW</div>
          <svg width='%d' height='%d'>%s</svg></div>", ceiling(2 * max(r) + 30), ceiling(h + 12), circles)
}

# ---- UI ---------------------------------------------------------------------

loading_page <- function() {
  page_fillable(
    theme = theme, title = "Spain Solar PV Fleet",
    card(card_header("Spain Solar PV Fleet"),
         card_body(
           p("Reading the Global Energy Monitor solar tracker. The first load after a deploy ",
             "can take a minute; the page reloads by itself when it's ready."),
           uiOutput("boot_status"))))
}

ui <- function(req) {
if (!data_ready()) return(loading_page())
d <- data_or_error()
if (inherits(d, "error")) return(loading_page())
yr_rng <- d$yr_rng; SITE_MW_MAX <- d$site_max

page_sidebar(
  theme = theme, fillable = FALSE, window_title = "Spain Solar PV Fleet",
  title = div(class = "brand", span(class = "logo"), "Spain Solar PV Fleet",
              span(class = "tag", "GEM Solar Tracker",
                   if (!is.null(d$raw$ree)) " · REE")),
  sidebar = sidebar(
    width = 290, bg = lin$bg,
    div(class = "sec", "Layers"),
    checkboxGroupInput("stages", NULL, choiceNames = stage_labels,
                       choiceValues = STAGES, selected = DEFAULT_STAGES),
    div(class = "sec", "Filters"),
    selectizeInput("state", "Comunidad autónoma", choices = d$states, multiple = TRUE,
                   options = list(placeholder = "All")),
    selectizeInput("prov", "Provincia", choices = d$provs, multiple = TRUE,
                   options = list(placeholder = "All")),
    sliderInput("mw", "Site size (MW)", min = 0, max = SITE_MW_MAX,
                value = c(1, SITE_MW_MAX), step = 1, ticks = FALSE),
    sliderInput("yr", "Start year (actual or expected)", min = yr_rng[1], max = yr_rng[2],
                value = yr_rng, step = 1, sep = "", ticks = FALSE),
    checkboxInput("no_year", "Include sites without a start year", value = TRUE),
    textInput("q", "Search site, owner or operator", placeholder = "e.g. Iberdrola"),
    actionButton("reset", "Reset filters", class = "btn-lin w-100"),
    hr(style = sprintf("border-color:%s; opacity:1; margin:14px 0 10px;", lin$border)),
    div(class = "sec", "Data"),
    uiOutput("meta")
  ),

  layout_columns(
    col_widths = c(3, 3, 3, 3), fill = FALSE,
    kpi("Operating",          STAGE_COLS[["Operating"]],          "k_op",  "k_op_f"),
    kpi("Under construction", STAGE_COLS[["Under construction"]], "k_uc",  "k_uc_f"),
    kpi("Active pipeline",    STAGE_COLS[["Pre-construction"]],   "k_pl",  "k_pl_f"),
    kpi("Largest operating site", "#D4D8FF",                      "k_big", "k_big_f")
  ),

  card(
    full_screen = TRUE, height = "640px",
    card_header("Sites", span(class = "sub",
      "one bubble per site and stage · filled = operating, dashed ring = pipeline")),
    card_body(padding = 0, leafletOutput("map", height = "100%"))
  ),

  card(
    full_screen = TRUE, height = "430px",
    card_header("Capacity trajectory", span(class = "sub",
      "annual additions by start year · cumulative fleet if pipeline stages complete · REE national total for reference")),
    card_body(plotlyOutput("trajectory", height = "100%"))
  ),

  layout_columns(
    col_widths = c(6, 6), fill = FALSE,
    card(height = "420px", full_screen = TRUE,
         card_header("Capacity by comunidad autónoma", span(class = "sub", "selected stages")),
         card_body(plotlyOutput("by_state", height = "100%"))),
    card(height = "420px", full_screen = TRUE,
         card_header("Site size distribution", span(class = "sub", "count of sites by stage")),
         card_body(plotlyOutput("by_size", height = "100%")))
  ),

  card(
    full_screen = TRUE,
    card_header("Capacity by owner", span(class = "sub",
      "GEM owner field · MW · respects all filters")),
    card_body(DTOutput("owners"))
  ),

  card(
    full_screen = TRUE,
    card_header("Sites", span(class = "sub", "select a row to fly to it on the map")),
    card_body(DTOutput("sites_tbl"))
  )
)
}

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {
  # Loading page: build the data after the page has been sent, then reload
  if (!data_ready()) {
    output$boot_status <- renderUI(div(class = "meta", "Working…"))
    session$onFlushed(function() {
      d <- data_or_error()
      if (inherits(d, "error")) {
        output$boot_status <- renderUI(tagList(
          p(tags$b("The data could not be loaded.")), tags$pre(conditionMessage(d)),
          p("Download the Global Solar Power Tracker .xlsx from ",
            a(GEM_URL, href = GEM_URL, target = "_blank"),
            " and save it next to app.R (name matching *solar*tracker*.xlsx).")))
      } else session$reload()
    }, once = TRUE)
    return(invisible())
  }
  d0 <- data_or_error()
  if (inherits(d0, "error")) return(invisible())
  D <- reactiveVal(d0)

  output$meta <- renderUI({
    s <- D()$raw
    div(class = "meta",
        "Plants: Global Energy Monitor, ",
        a("Global Solar Power Tracker", href = GEM_URL, target = "_blank"),
        " (", s$gem_file, ", CC BY 4.0). Phases rolled up to one bubble per site and stage. ",
        "MW as reported by GEM (MWac where known). Pipeline phases with a start year already ",
        "past are shown in ", CY, ".",
        br(), br(),
        if (!is.null(s$ree)) tagList("Reference line: national installed solar PV from ",
          a("Red Eléctrica REData", href = "https://www.ree.es/es/apidatos", target = "_blank"),
          " (all sizes, including rooftop), so it sits above the utility-scale GEM total.")
        else "Red Eléctrica's REData API was not available this build, so there is no reference line.",
        br(), br(),
        "Map tiles © ",
        a("OpenStreetMap", href = "https://www.openstreetmap.org/copyright", target = "_blank"),
        " contributors.",
        br(), sprintf("Built %s", format(s$built, "%Y-%m-%d %H:%M")))
  })

  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "stages", selected = DEFAULT_STAGES)
    updateSelectizeInput(session, "state", selected = character(0))
    updateSelectizeInput(session, "prov", selected = character(0))
    updateSliderInput(session, "mw", value = c(1, D()$site_max))
    updateSliderInput(session, "yr", value = D()$yr_rng)
    updateCheckboxInput(session, "no_year", value = TRUE)
    updateTextInput(session, "q", value = "")
  })

  q_d <- debounce(reactive(input$q), 300)

  # Phase-level rows after attribute filters (all stages)
  gens <- reactive({
    d <- D()$G |> filter((is.na(year_plot) & isTRUE(input$no_year)) |
                         (!is.na(year_plot) & year_plot >= input$yr[1] & year_plot <= input$yr[2]))
    if (length(input$state))  d <- filter(d, state %in% input$state)
    if (length(input$prov))   d <- filter(d, province %in% input$prov)
    q <- trimws(q_d())
    if (nzchar(q)) d <- filter(d, grepl(q, plant, ignore.case = TRUE) |
                                  grepl(q, owner, ignore.case = TRUE) |
                                  grepl(q, coalesce(operator, ""), ignore.case = TRUE))
    d
  })

  # One row per site and stage: the bubble map unit
  sites_all <- reactive({
    gens() |>
      arrange(year_plot) |>
      group_by(loc_id, stage) |>
      summarise(
        plant = first(plant), owner = first(owner), operator = first(operator),
        state = first(state), province = first(province), municipality = first(municipality),
        status = paste(unique(status), collapse = "; "),
        lat = first(lat), lon = first(lon), mw = sum(mw), n_ph = n(),
        rating = paste(unique(na.omit(rating)), collapse = "/"),
        yr_first = suppressWarnings(min(year, na.rm = TRUE)),
        yr_last  = suppressWarnings(max(year, na.rm = TRUE)),
        accuracy = first(accuracy), wiki = first(wiki), .groups = "drop") |>
      mutate(yr_first = ifelse(is.finite(yr_first), yr_first, NA_integer_),
             yr_last  = ifelse(is.finite(yr_last),  yr_last,  NA_integer_)) |>
      filter(mw >= input$mw[1], mw <= input$mw[2])
  })

  sites <- reactive({
    sites_all() |>
      filter(stage %in% input$stages) |>
      mutate(size_class = cut(mw, SIZE_BREAKS, SIZE_LABELS, right = FALSE),
             radius = bubble_r(mw),
             online = ifelse(is.na(yr_first), "-", as.character(yr_first))) |>
      arrange(desc(mw))
  })

  # KPIs (ignore the layer toggles so they always tell the full story)
  stage_sum <- function(st) {
    s <- sites_all() |> filter(stage %in% st)
    list(mw = sum(s$mw), n = nrow(s))
  }
  output$k_op   <- renderText(fmt_mw(stage_sum("Operating")$mw))
  output$k_op_f <- renderText(sprintf("%s sites · %.0f%% of ES utility PV",
    format(stage_sum("Operating")$n, big.mark = ","),
    100 * stage_sum("Operating")$mw / max(D()$total_op, 1)))
  output$k_uc   <- renderText(fmt_mw(stage_sum("Under construction")$mw))
  output$k_uc_f <- renderText({
    s <- sites_all() |> filter(stage == "Under construction")
    if (!nrow(s)) return("no sites")
    yrs <- na.omit(c(s$yr_first, s$yr_last))
    if (length(yrs)) sprintf("%s sites · start %s to %s", nrow(s),
                             max(min(yrs), CY), max(yrs))
    else sprintf("%s sites", nrow(s))
  })
  output$k_pl   <- renderText(fmt_mw(stage_sum(PIPELINE)$mw))
  output$k_pl_f <- renderText({
    s <- stage_sum(PIPELINE); op <- stage_sum("Operating")$mw
    st <- stage_sum(STALLED)
    sprintf("%s sites · %s of today's fleet · %s stalled",
            format(s$n, big.mark = ","),
            if (op > 0) sprintf("%.0f%%", 100 * s$mw / op) else "-", fmt_mw(st$mw))
  })
  big <- reactive(sites_all() |> filter(stage == "Operating") |>
                    slice_max(mw, n = 1, with_ties = FALSE))
  output$k_big   <- renderText(if (nrow(big())) fmt_mw(big()$mw) else "-")
  output$k_big_f <- renderText(if (nrow(big())) sprintf("%s, %s", big()$plant, big()$state) else "")

  # ---- Map (OpenStreetMap tiles, no key) ----
  output$map <- renderLeaflet({
    osm_attr <- '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    osm_url  <- "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addTiles(osm_url, attribution = osm_attr, group = "OSM dark",
               options = tileOptions(maxZoom = 19, className = "osm-dark")) |>
      addTiles(osm_url, attribution = osm_attr, group = "OSM standard",
               options = tileOptions(maxZoom = 19)) |>
      addLayersControl(baseGroups = c("OSM dark", "OSM standard"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      setView(-3.7, 39.6, zoom = 6) |>
      addLegend("bottomright", colors = unname(STAGE_COLS), labels = STAGES,
                title = "Stage", opacity = 1) |>
      addControl(html = HTML(size_legend_html()), position = "bottomleft",
                 className = "size-legend-wrap")
  })

  observe({
    d <- sites() |> mutate(stage = as.character(stage))
    proxy <- leafletProxy("map") |> clearGroup("sites") |> clearGroup("focus")
    if (!nrow(d)) return()
    col <- unname(STAGE_COLS[d$stage])
    wiki_link <- ifelse(is.na(d$wiki) | !nzchar(d$wiki), "",
                        sprintf("<br><a href='%s' target='_blank'>GEM wiki page</a>", d$wiki))
    loc <- vapply(seq_len(nrow(d)), \(i) {
      p <- c(d$municipality[i], d$province[i], d$state[i])
      p <- unique(p[!is.na(p) & nzchar(p)])
      if (length(p)) paste(p, collapse = ", ") else "-"
    }, "")
    pop <- sprintf(
      "<span class='pop-stage' style='color:%s'>%s</span><br>
       <b>%s</b><br>
       <span class='pop-k'>Owner</span> %s<br>
       <span class='pop-k'>Operator</span> %s<br>
       <span class='pop-k'>Location</span> %s<br>
       <span class='pop-k'>Capacity</span> %s MW%s &middot; %d phase%s<br>
       <span class='pop-k'>Start year</span> %s<br>
       <span class='pop-k'>Status</span> %s<br>
       <span class='pop-k'>Coordinates</span> %s%s",
      col, d$stage, htmlEscape(d$plant), htmlEscape(d$owner), htmlEscape(na_dash(d$operator)),
      htmlEscape(loc),
      formatC(d$mw, format = "f", digits = 1, big.mark = ","),
      ifelse(nzchar(d$rating), paste0(" (", d$rating, ")"), ""),
      d$n_ph, ifelse(d$n_ph == 1, "", "s"), d$online, htmlEscape(d$status),
      htmlEscape(na_dash(d$accuracy)), wiki_link)
    lab <- sprintf("%s · %s MW · %s", d$plant, formatC(d$mw, format = "f", digits = 1), d$stage)

    is_op <- d$stage == "Operating"
    if (any(is_op)) {
      proxy |> addCircleMarkers(
        lng = d$lon[is_op], lat = d$lat[is_op], radius = d$radius[is_op],
        group = "sites", layerId = paste0(d$loc_id[is_op], "_op"),
        stroke = TRUE, weight = 0.8, color = col[is_op], opacity = 0.9,
        fillColor = col[is_op], fillOpacity = 0.45,
        popup = pop[is_op], label = lab[is_op])
    }
    if (any(!is_op)) {
      proxy |> addCircleMarkers(
        lng = d$lon[!is_op], lat = d$lat[!is_op], radius = d$radius[!is_op],
        group = "sites", layerId = paste0(d$loc_id[!is_op], "_", d$stage[!is_op]),
        stroke = TRUE, weight = 1.4, dashArray = "3,3", color = col[!is_op], opacity = 0.95,
        fillColor = col[!is_op], fillOpacity = 0.10,
        popup = pop[!is_op], label = lab[!is_op])
    }
  })

  # ---- Capacity trajectory ----
  output$trajectory <- renderPlotly({
    stg <- setdiff(input$stages, STALLED)
    g <- gens() |> filter(!is.na(year_plot), stage %in% stg)
    shiny::validate(shiny::need(nrow(g) > 0,
                                "No capacity with a start year for the current filters."))

    yrs <- seq(min(g$year_plot), max(c(g$year_plot, CY)))
    annual <- g |>
      group_by(year_plot, stage) |> summarise(gw = sum(mw) / 1000, .groups = "drop") |>
      complete(year_plot = yrs, stage = factor(STAGES, STAGES), fill = list(gw = 0))

    p <- plot_ly()
    for (s in intersect(STAGES, stg)) {
      a <- filter(annual, stage == s, gw > 0)
      if (!nrow(a)) next
      p <- p |> add_bars(data = a, x = ~year_plot, y = ~gw, name = s,
                         marker = list(color = STAGE_COLS[[s]],
                                       opacity = if (s == "Operating") 1 else 0.85),
                         hovertemplate = paste0(s, " · %{x}: +%{y:.2f} GW<extra></extra>"))
    }

    op <- annual |> filter(stage == "Operating") |> arrange(year_plot) |>
      mutate(cum = cumsum(gw))
    hist_end <- max(c(g$year_plot[g$stage == "Operating"], CY - 1), na.rm = TRUE)
    base <- sum(op$gw)
    if ("Operating" %in% stg) {
      h <- filter(op, year_plot <= hist_end)
      p <- p |> add_lines(data = h, x = ~year_plot, y = ~cum, yaxis = "y2",
                          name = "Operating fleet (cumulative)",
                          line = list(color = "#D4D8FF", width = 2),
                          hovertemplate = "%{x}: %{y:.2f} GW installed<extra></extra>")
    } else base <- 0

    fut_years <- yrs[yrs >= hist_end]
    pipe_on <- intersect(PIPELINE, stg)
    scen <- list(c("Under construction"),
                 c("Under construction", "Pre-construction"),
                 PIPELINE)
    scen_name <- c("+ under construction", "+ pre-construction", "+ announced")
    dash <- c("dash", "dashdot", "dot")
    for (i in seq_along(scen)) {
      st <- intersect(scen[[i]], pipe_on)
      if (!length(st) || (i > 1 && setequal(st, intersect(scen[[i - 1]], pipe_on)))) next
      add <- annual |> filter(stage %in% st, year_plot >= hist_end) |>
        group_by(year_plot) |> summarise(gw = sum(gw), .groups = "drop") |>
        right_join(tibble(year_plot = fut_years), by = "year_plot") |>
        mutate(gw = coalesce(gw, 0)) |> arrange(year_plot) |>
        mutate(cum = base + cumsum(gw))
      add <- bind_rows(tibble(year_plot = hist_end, cum = base), add)
      p <- p |> add_lines(data = add, x = ~year_plot, y = ~cum, yaxis = "y2",
                          name = scen_name[i],
                          line = list(color = STAGE_COLS[[st[length(st)]]], width = 1.6,
                                      dash = dash[i]),
                          hovertemplate = paste0(scen_name[i], " · %{x}: %{y:.2f} GW<extra></extra>"))
    }

    # Red Eléctrica's national installed PV (all sizes) as a reference
    ree <- D()$raw$ree
    if (!is.null(ree) && nrow(ree))
      p <- p |> add_lines(data = filter(ree, year >= min(yrs), year <= max(yrs)),
                          x = ~year, y = ~I(mw / 1000), yaxis = "y2",
                          name = "REE national installed PV (all sizes)",
                          line = list(color = "#4CB7A5", width = 1.4, dash = "longdash"),
                          hovertemplate = "REE %{x}: %{y:.1f} GW<extra></extra>")

    p |> plotly_lin(
      barmode = "stack", bargap = 0.22, showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = 1.13, font = list(size = 10, color = lin$muted),
                    bgcolor = "rgba(0,0,0,0)"),
      xaxis = list(title = "", dtick = 2, tickfont = list(color = lin$faint),
                   range = c(max(min(yrs), CY - 16) - 0.5, max(yrs) + 0.5)),
      yaxis = list(title = list(text = "GW added per year", font = list(size = 10)),
                   gridcolor = "rgba(255,255,255,0.05)", tickfont = list(color = lin$faint)),
      yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE, rangemode = "tozero",
                    title = list(text = "cumulative GW", font = list(size = 10)),
                    tickfont = list(color = lin$faint)),
      shapes = list(list(type = "line", xref = "x", yref = "paper",
                         x0 = CY - 0.5, x1 = CY - 0.5, y0 = 0, y1 = 1,
                         line = list(color = "rgba(255,255,255,0.25)", width = 1, dash = "dot"))),
      annotations = list(list(x = CY - 0.5, y = 1, xref = "x", yref = "paper",
                              text = paste(CY, "→ pipeline"), showarrow = FALSE,
                              xanchor = "left", yanchor = "top", xshift = 4,
                              font = list(size = 10, color = lin$faint))),
      margin = list(l = 8, r = 44, t = 34, b = 6))
  })

  output$by_state <- renderPlotly({
    s <- sites_all() |> filter(stage %in% input$stages) |>
      group_by(state, stage) |> summarise(gw = sum(mw) / 1000, .groups = "drop")
    shiny::validate(shiny::need(nrow(s) > 0, "No sites for the current filters."))
    top <- s |> group_by(state) |> summarise(t = sum(gw)) |> slice_max(t, n = 15) |> arrange(t)
    s <- filter(s, state %in% top$state) |> mutate(state = factor(state, top$state))
    p <- plot_ly()
    for (st in intersect(STAGES, unique(as.character(s$stage)))) {
      p <- p |> add_bars(data = filter(s, stage == st), x = ~gw, y = ~state,
                         orientation = "h", name = st,
                         marker = list(color = STAGE_COLS[[st]]),
                         hovertemplate = paste0(st, " · %{y}: %{x:.2f} GW<extra></extra>"))
    }
    p |> plotly_lin(barmode = "stack", bargap = 0.3,
                    xaxis = list(title = list(text = "GW", font = list(size = 10))),
                    yaxis = list(title = "", tickfont = list(color = lin$muted)))
  })

  output$by_size <- renderPlotly({
    z <- sites() |> count(size_class, stage, .drop = FALSE) |>
      filter(stage %in% input$stages)
    shiny::validate(shiny::need(sum(z$n) > 0, "No sites for the current filters."))
    p <- plot_ly()
    for (st in intersect(STAGES, input$stages)) {
      p <- p |> add_bars(data = filter(z, stage == st), x = ~size_class, y = ~n, name = st,
                         marker = list(color = STAGE_COLS[[st]]),
                         hovertemplate = paste0(st, " · %{x}: %{y:,} sites<extra></extra>"))
    }
    p |> plotly_lin(barmode = "group", bargap = 0.25, bargroupgap = 0.08,
                    xaxis = list(title = "", tickfont = list(color = lin$muted)),
                    yaxis = list(title = ""))
  })

  # ---- Capacity by owner ----
  owner_tbl <- reactive({
    sites_all() |>
      group_by(Owner = owner) |>
      summarise(
        `Operating sites`     = sum(stage == "Operating"),
        `Operating MW`        = sum(mw[stage == "Operating"]),
        `Under constr. MW`    = sum(mw[stage == "Under construction"]),
        `Pre-construction MW` = sum(mw[stage == "Pre-construction"]),
        `Announced MW`        = sum(mw[stage == "Announced"]),
        `Stalled MW`          = sum(mw[stage %in% STALLED]),
        Communities = n_distinct(state),
        `Largest site` = {
          o <- stage == "Operating"
          if (any(o)) sprintf("%s (%s MW)", plant[o][which.max(mw[o])],
                              formatC(max(mw[o]), format = "f", digits = 0))
          else "-"
        },
        .groups = "drop") |>
      mutate(`Total active` = `Operating MW` + `Under constr. MW` +
               `Pre-construction MW` + `Announced MW`,
             `Share of operating` = `Operating MW` / max(sum(`Operating MW`), 1)) |>
      relocate(`Share of operating`, .after = `Operating MW`) |>
      arrange(desc(`Operating MW`), desc(`Total active`))
  })

  output$owners <- renderDT({
    d <- owner_tbl()
    datatable(
      d, rownames = FALSE, selection = "none", extensions = "Buttons",
      style = "bootstrap5", class = "compact hover",
      options = list(pageLength = 15, dom = "Bftip", buttons = list("csv", "excel"),
                     order = list(list(2, "desc")),
                     columnDefs = list(list(className = "dt-right", targets = c(1:8, 10))))
    ) |>
      formatRound(c("Operating MW", "Under constr. MW", "Pre-construction MW",
                    "Announced MW", "Stalled MW", "Total active"), digits = 0) |>
      formatPercentage("Share of operating", digits = 1) |>
      formatStyle("Operating MW",
                  background = styleColorBar(c(0, max(d$`Operating MW`, 1)),
                                             "rgba(94,106,210,0.35)"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") |>
      formatStyle("Under constr. MW",
                  background = styleColorBar(c(0, max(d$`Under constr. MW`, 1)),
                                             "rgba(242,153,74,0.30)"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # ---- Site table ----
  output$sites_tbl <- renderDT({
    datatable(
      sites() |> transmute(Site = plant, Stage = stage, Owner = owner, `C. A.` = state,
                           Province = province, Municipality = municipality, MW = round(mw, 1),
                           Rating = rating, Phases = n_ph, Start = online, Coordinates = accuracy),
      rownames = FALSE, selection = "single", style = "bootstrap5", class = "compact hover",
      extensions = "Buttons",
      options = list(pageLength = 12, dom = "Bftip", buttons = list("csv", "excel"),
                     order = list(list(6, "desc")),
                     columnDefs = list(list(className = "dt-right", targets = c(6, 8))))
    ) |>
      formatStyle("Stage", color = styleEqual(STAGES, unname(STAGE_COLS)))
  })

  observeEvent(input$sites_tbl_rows_selected, {
    r <- sites()[input$sites_tbl_rows_selected, ]
    leafletProxy("map") |>
      flyTo(r$lon, r$lat, zoom = 13, options = list(duration = 0.8)) |>
      clearGroup("focus") |>
      addCircleMarkers(lng = r$lon, lat = r$lat, radius = r$radius + 6, group = "focus",
                       stroke = TRUE, color = "#FFFFFF", weight = 1.5, opacity = 0.9,
                       fill = FALSE)
  })
}

shinyApp(ui, server)
