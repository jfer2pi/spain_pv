# =============================================================================
# Spain Solar PV Fleet: registered installations and pre-registered pipeline
# -----------------------------------------------------------------------------
# One self-contained file. No API keys anywhere.
#
# Data (combined at startup, cached to es_solar.rds next to this file):
#   1. RAIPEE, Registro Administrativo de Instalaciones de Producción de
#      Energía Eléctrica, published daily by the Ministry through ELECTRA
#      (https://energia.serviciosmin.gob.es/Electra/). The app downloads the
#      CSV export (Excel as a second try). If neither answers, it reads the
#      newest raipee*.csv / raipee*.xlsx saved next to app.R.
#      Solar PV rows (subgroup b.1.1 / "fotovoltaica") are kept.
#        inscripción definitiva -> Registered
#        inscripción previa     -> Pre-registered (authorised, not yet in service)
#   2. Wikidata (keyless SPARQL): coordinates of every Spanish municipality by
#      INE code, used to place installations at their municipality.
#      Cached in es_municipios.csv (commit it so a deployed app needs no lookup).
#   3. Red Eléctrica REData API (keyless): national installed solar PV
#      capacity by year, drawn on the trajectory chart as the reference.
#
# Map:   OpenStreetMap tiles (dark-filtered + standard). Light use only, per
#        https://operations.osmfoundation.org/policies/tiles/
# Theme: Linear-style dark UI, matching the US Solar PV Fleet app.
#
# install.packages(c("shiny","bslib","httr2","jsonlite","readxl","dplyr","tidyr",
#                    "leaflet","plotly","DT","htmltools"))
# shiny::runApp()
# =============================================================================

library(shiny)
library(bslib)
library(httr2)
library(jsonlite)
library(readxl)
library(dplyr)
library(tidyr)
library(leaflet)
library(plotly)
library(DT)
library(htmltools)

# ---- Config -----------------------------------------------------------------

CACHE_FILE      <- "es_solar.rds"
CACHE_TTL       <- 24 * 3600
CACHE_VERSION   <- 1L
CONNECT_TIMEOUT <- 20
# options(es_pv.offline = TRUE) skips all downloads and uses local files only

RAIPEE_CSV  <- "https://energia.serviciosmin.gob.es/Electra/descargarCSVProduccion.aspx"
RAIPEE_XLSX <- "https://energia.serviciosmin.gob.es/Electra/descargarExcelProduccion.aspx"
RAIPEE_PAGE <- "https://datos.gob.es/es/catalogo/e05068001-registro-de-productores-de-energia-electrica"
RAIPEE_LOCAL_PATTERN <- "(raipee|produccion|productores).*\\.(csv|xlsx)$"

MUNI_FILE   <- "es_municipios.csv"
WIKIDATA    <- "https://query.wikidata.org/sparql"
REE_API     <- "https://apidatos.ree.es/es/datos/generacion/potencia-instalada"

BROWSER_UA <- paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
                    "(KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36")
APP_UA <- "es-solar-fleet-dashboard/1.0 (R/httr2)"

STAGES <- c("Registered", "Pre-registered")
STAGE_COLS <- c("Registered" = "#5E6AD2", "Pre-registered" = "#F2994A")
PIPELINE <- "Pre-registered"
CY <- as.integer(format(Sys.Date(), "%Y"))

# INE province codes, with the name variants RAIPEE and Wikidata may use
PROVINCIAS <- c(
  "01" = "Araba/Álava/Alava", "02" = "Albacete", "03" = "Alicante/Alacant",
  "04" = "Almería", "05" = "Ávila", "06" = "Badajoz", "07" = "Illes Balears/Baleares/Islas Baleares",
  "08" = "Barcelona", "09" = "Burgos", "10" = "Cáceres", "11" = "Cádiz",
  "12" = "Castellón/Castelló/Castellón de la Plana", "13" = "Ciudad Real", "14" = "Córdoba",
  "15" = "A Coruña/La Coruña/Coruña", "16" = "Cuenca", "17" = "Girona/Gerona", "18" = "Granada",
  "19" = "Guadalajara", "20" = "Gipuzkoa/Guipúzcoa", "21" = "Huelva", "22" = "Huesca",
  "23" = "Jaén", "24" = "León", "25" = "Lleida/Lérida", "26" = "La Rioja/Rioja", "27" = "Lugo",
  "28" = "Madrid", "29" = "Málaga", "30" = "Murcia", "31" = "Navarra/Nafarroa", "32" = "Ourense/Orense",
  "33" = "Asturias", "34" = "Palencia", "35" = "Las Palmas/Palmas", "36" = "Pontevedra",
  "37" = "Salamanca", "38" = "Santa Cruz de Tenerife/Tenerife", "39" = "Cantabria",
  "40" = "Segovia", "41" = "Sevilla", "42" = "Soria", "43" = "Tarragona", "44" = "Teruel",
  "45" = "Toledo", "46" = "Valencia/València", "47" = "Valladolid", "48" = "Bizkaia/Vizcaya",
  "49" = "Zamora", "50" = "Zaragoza", "51" = "Ceuta", "52" = "Melilla")
PROV_NAME <- sub("/.*$", "", PROVINCIAS)
CCAA_OF_PROV <- c(
  "04" = "Andalucía", "11" = "Andalucía", "14" = "Andalucía", "18" = "Andalucía",
  "21" = "Andalucía", "23" = "Andalucía", "29" = "Andalucía", "41" = "Andalucía",
  "22" = "Aragón", "44" = "Aragón", "50" = "Aragón", "33" = "Asturias", "07" = "Illes Balears",
  "35" = "Canarias", "38" = "Canarias", "39" = "Cantabria",
  "05" = "Castilla y León", "09" = "Castilla y León", "24" = "Castilla y León",
  "34" = "Castilla y León", "37" = "Castilla y León", "40" = "Castilla y León",
  "42" = "Castilla y León", "47" = "Castilla y León", "49" = "Castilla y León",
  "02" = "Castilla-La Mancha", "13" = "Castilla-La Mancha", "16" = "Castilla-La Mancha",
  "19" = "Castilla-La Mancha", "45" = "Castilla-La Mancha",
  "08" = "Cataluña", "17" = "Cataluña", "25" = "Cataluña", "43" = "Cataluña",
  "03" = "C. Valenciana", "12" = "C. Valenciana", "46" = "C. Valenciana",
  "06" = "Extremadura", "10" = "Extremadura", "15" = "Galicia", "27" = "Galicia",
  "32" = "Galicia", "36" = "Galicia", "28" = "Madrid", "30" = "Murcia", "31" = "Navarra",
  "01" = "País Vasco", "20" = "País Vasco", "48" = "País Vasco", "26" = "La Rioja",
  "51" = "Ceuta", "52" = "Melilla")

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

# Municipality names: drop articles in either position ("El Puerto", "Puerto, El")
norm_muni <- function(x) {
  s <- norm_txt(x)
  s <- sub(" (el|la|los|las|l|lo|els|les|o|a|os|as)$", "", s)
  s <- sub("^(el|la|los|las|l|lo|els|les|o|a|os|as) ", "", s)
  s
}

pickr <- function(d, ...) {
  for (p in c(...)) {
    nm <- grep(p, names(d), value = TRUE, perl = TRUE)
    if (length(nm)) return(as.character(d[[nm[1]]]))
  }
  rep(NA_character_, nrow(d))
}
pick_name <- function(d, ...) {
  for (p in c(...)) { nm <- grep(p, names(d), value = TRUE, perl = TRUE); if (length(nm)) return(nm[1]) }
  NA_character_
}

# Spanish numbers: "1.234,56" or "1234,56"; a lone dot is a decimal point
num_es <- function(x) {
  x <- gsub("[^0-9,.-]", "", trimws(as.character(x)))
  has_comma <- grepl(",", x)
  x[has_comma] <- sub(",", ".", gsub(".", "", x[has_comma], fixed = TRUE), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# Dates as Excel serials, dd/mm/yyyy, yyyy-mm-dd
date_es <- function(x) {
  x <- trimws(as.character(x)); x[is.na(x)] <- ""
  out <- as.Date(rep(NA_character_, length(x)))
  ser <- grepl("^[0-9]{5}(\\.[0-9]+)?$", x)
  out[ser] <- as.Date(floor(as.numeric(x[ser])), origin = "1899-12-30")
  dmy <- is.na(out) & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)
  out[dmy] <- as.Date(sub("^([0-9]{1,2}/[0-9]{1,2}/[0-9]{4}).*$", "\\1", x[dmy]), format = "%d/%m/%Y")
  iso <- is.na(out) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  out[iso] <- as.Date(substr(x[iso], 1, 10), format = "%Y-%m-%d")
  out[!is.na(out) & (out < as.Date("1950-01-01") | out > as.Date("2100-01-01"))] <- NA
  out
}

prov_code <- function(x) {
  s <- norm_txt(x)
  keys <- lapply(PROVINCIAS, \(v) norm_txt(strsplit(v, "/", fixed = TRUE)[[1]]))
  vapply(s, function(v) {
    if (!nzchar(v)) return(NA_character_)
    if (grepl("^[0-9]{1,2}$", v)) return(sprintf("%02d", as.integer(v)))
    hit <- names(keys)[vapply(keys, \(k) v %in% k, TRUE)]
    if (!length(hit)) hit <- names(keys)[vapply(keys, \(k) any(startsWith(v, k) | startsWith(k, v)), TRUE)]
    if (length(hit)) hit[1] else NA_character_
  }, "", USE.NAMES = FALSE)
}

na_dash <- function(x) ifelse(is.na(x) | !nzchar(trimws(x)), "-", x)
is_offline <- function() isTRUE(getOption("es_pv.offline", FALSE))

get_url <- function(url, path = NULL, ua = BROWSER_UA, query = list(), timeout = 300) {
  r <- request(url) |> req_url_query(!!!query) |> req_user_agent(ua) |>
    req_options(connecttimeout = CONNECT_TIMEOUT) |> req_timeout(timeout) |>
    req_error(is_error = \(r) FALSE)
  if (is.null(path)) req_perform(r) else req_perform(r, path = path)
}

# ---- 1. RAIPEE --------------------------------------------------------------

# Delimiter and encoding vary between exports, so sniff both
read_csv_sniff <- function(path) {
  con <- file(path, "rb"); bytes <- readBin(con, "raw", 65536); close(con)
  bom <- length(bytes) >= 3 && identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))
  txt <- rawToChar(bytes[bytes != as.raw(0)])
  enc <- if (bom) "UTF-8-BOM" else if (validUTF8(txt)) "UTF-8" else "latin1"
  lines <- strsplit(txt, "\n", fixed = TRUE, useBytes = TRUE)[[1]]
  n_sep <- function(l, ch) sum(gregexpr(ch, l, fixed = TRUE, useBytes = TRUE)[[1]] > 0)
  # Header = first line with several separators (some exports start with a title line)
  sep <- if (sum(vapply(head(lines, 10), n_sep, 0, ";")) >= sum(vapply(head(lines, 10), n_sep, 0, ",")))
    ";" else ","
  skip <- which(vapply(head(lines, 20), n_sep, 0, sep) >= 3)[1] - 1
  d <- read.csv(path, sep = sep, fileEncoding = enc, colClasses = "character", skip = max(0, skip),
                check.names = FALSE, quote = "\"", na.strings = c("", "NA", "NULL"),
                strip.white = TRUE, comment.char = "")
  names(d) <- make.unique(clean_names(names(d)))
  as_tibble(d)
}

read_xlsx_find <- function(path) {
  for (sh in excel_sheets(path)) {
    h <- suppressMessages(read_excel(path, sheet = sh, col_names = FALSE, n_max = 30,
                                     col_types = "text", .name_repair = "minimal"))
    if (!nrow(h)) next
    hdr <- which(apply(h, 1, \(r) { r <- norm_txt(r)
      any(grepl("potencia", r)) && any(grepl("municipio|provincia", r)) }))[1]
    if (is.na(hdr)) next
    d <- suppressMessages(read_excel(path, sheet = sh, skip = hdr, col_names = FALSE,
                                     col_types = "text", .name_repair = "minimal"))
    nm <- clean_names(unlist(h[hdr, ]))
    nm <- c(nm, rep("", max(0, ncol(d) - length(nm))))[seq_len(ncol(d))]
    nm[is.na(nm) | !nzchar(nm)] <- paste0("x", which(is.na(nm) | !nzchar(nm)))
    names(d) <- make.unique(nm)
    return(d)
  }
  stop("No sheet with Potencia and Municipio columns in ", basename(path), call. = FALSE)
}

get_raipee <- function() {
  why <- character()
  if (!is_offline()) for (u in list(c(RAIPEE_CSV, ".csv"), c(RAIPEE_XLSX, ".xlsx"))) {
    tmp <- tempfile(fileext = u[2])
    ok <- tryCatch({
      r <- get_url(u[1], tmp)
      if (resp_status(r) != 200) stop("HTTP ", resp_status(r))
      if (file.size(tmp) < 10000) stop("file too small (", file.size(tmp), " bytes)")
      TRUE
    }, error = function(e) { why <<- c(why, paste0(basename(u[1]), ": ", conditionMessage(e))); FALSE })
    if (ok) return(list(path = tmp, name = paste0("ELECTRA download (", sub("^\\.", "", u[2]), ")"),
                        from = "download"))
  }
  local <- list.files(".", pattern = RAIPEE_LOCAL_PATTERN, ignore.case = TRUE)
  local <- local[!grepl("^~\\$", local)]
  if (length(local)) {
    f <- local[which.max(file.mtime(local))]
    message("RAIPEE: using local ", f, if (length(why)) paste0(" (", paste(why, collapse = "; "), ")"))
    return(list(path = f, name = f, from = "local file"))
  }
  stop("Could not get the RAIPEE export",
       if (length(why)) paste0(" (", paste(why, collapse = "; "), ")"),
       ". Download it from ", RAIPEE_CSV, " and save it next to app.R as raipee_produccion.csv.",
       call. = FALSE)
}

stage_from_raipee <- function(x) {
  s <- norm_txt(x)
  out <- case_when(
    grepl("baja|cancel|revoca|caduc|anulad", s)  ~ NA_character_,
    grepl("previ|provisional|preasign", s)        ~ "Pre-registered",
    TRUE                                          ~ "Registered")
  factor(out, levels = STAGES)
}

read_raipee <- function(path) {
  d <- if (grepl("\\.xlsx$", path, ignore.case = TRUE)) read_xlsx_find(path) else read_csv_sniff(path)

  tech_cols <- grep("tecnolog|subgrupo|grupo|categoria|tipo_de_instalacion|combustible|fuente|energia_primaria",
                    names(d), value = TRUE)
  tech_txt  <- if (length(tech_cols)) do.call(paste, c(lapply(tech_cols, \(c) coalesce(d[[c]], "")), sep = " "))
               else apply(d, 1, paste, collapse = " ")
  tt <- norm_txt(tech_txt)
  solar <- grepl("fotovolt|\\bfv\\b|\\bb 1 1\\b|\\bb11\\b|solar fv", tt) & !grepl("termo|b 1 2|\\bb12\\b", tt)
  d <- d[solar, ]; tt <- tt[solar]
  if (!nrow(d)) stop("No solar PV rows recognised in the RAIPEE export (columns: ",
                     paste(head(names(d), 30), collapse = ", "), ")", call. = FALSE)

  mw_col <- pick_name(d, "potencia.*instalada", "potencia.*neta", "potencia.*bruta", "^potencia")
  mw_raw <- num_es(d[[mw_col]])
  # Unit from the column name; otherwise kW if the typical value is large
  mw <- if (grepl("mw", mw_col)) mw_raw else if (grepl("kw", mw_col)) mw_raw / 1000 else
    if (isTRUE(median(mw_raw, na.rm = TRUE) > 50)) mw_raw / 1000 else mw_raw

  ine <- gsub("[^0-9]", "", pickr(d, "cod.*ine", "ine", "cod.*municipio", "codigo_municipio"))
  ine[!nchar(ine) %in% c(4, 5)] <- NA
  ine <- ifelse(is.na(ine), NA, sprintf("%05d", as.integer(ine)))
  prov <- coalesce(substr(ine, 1, 2), prov_code(pickr(d, "^provincia", "provincia")))

  r <- tibble(
    reg      = trimws(pickr(d, "^n_?(o_)?registro", "registro", "^numero", "^n_")),
    plant    = trimws(pickr(d, "nombre.*instalacion", "^instalacion", "^nombre", "denominacion")),
    holder   = trimws(pickr(d, "titular", "razon_social", "empresa", "propietario")),
    tech     = trimws(tech_txt),
    status   = trimws(pickr(d, "estado", "situacion", "tipo_de_inscripcion", "inscripcion", "fase")),
    municipio = trimws(pickr(d, "^municipio", "municipio", "localidad", "termino")),
    prov     = prov,
    ine      = ine,
    alta     = date_es(pickr(d, "fecha.*(inscripcion|alta|registro)", "^fecha")),
    servicio = date_es(pickr(d, "fecha.*(puesta|servicio|funcionamiento|explotacion|acta)")),
    mw       = mw
  ) |>
    mutate(stage = stage_from_raipee(paste(status, coalesce(reg, ""))),
           provincia = unname(PROV_NAME[prov]),
           ccaa = unname(CCAA_OF_PROV[prov]),
           holder = coalesce(na_if(holder, ""), "Not reported"),
           plant = coalesce(na_if(plant, ""), reg, "Unnamed"),
           online = coalesce(servicio, alta)) |>
    filter(!is.na(stage), !is.na(mw), mw > 0)
  attr(r, "columns") <- names(d)
  r
}

# ---- 2. Municipality coordinates (Wikidata) ---------------------------------

WIKIDATA_QUERY <- '
SELECT ?ine ?name ?coord WHERE {
  ?m wdt:P772 ?ine ; wdt:P625 ?coord .
  OPTIONAL { ?m rdfs:label ?name . FILTER(LANG(?name) = "es") }
}'

get_municipios <- function() {
  if (file.exists(MUNI_FILE)) {
    m <- read.csv(MUNI_FILE, colClasses = c("character", "character", "numeric", "numeric"),
                  encoding = "UTF-8")
    if (nrow(m) > 5000) return(as_tibble(m))
  }
  if (is_offline()) stop("No ", MUNI_FILE, " and offline", call. = FALSE)
  r <- get_url(WIKIDATA, ua = APP_UA, timeout = 120,
               query = list(query = WIKIDATA_QUERY, format = "json"))
  if (resp_status(r) != 200) stop("Wikidata returned HTTP ", resp_status(r), call. = FALSE)
  b <- resp_body_json(r, simplifyVector = TRUE)$results$bindings
  pt <- regmatches(b$coord$value, regexec("Point\\(([-0-9.eE]+) ([-0-9.eE]+)\\)", b$coord$value))
  m <- tibble(
    ine  = sprintf("%05d", suppressWarnings(as.integer(b$ine$value))),
    name = if (is.null(b$name)) NA_character_ else b$name$value,
    lon  = vapply(pt, \(p) if (length(p) == 3) as.numeric(p[2]) else NA_real_, 0),
    lat  = vapply(pt, \(p) if (length(p) == 3) as.numeric(p[3]) else NA_real_, 0)
  ) |>
    filter(!is.na(lat), !is.na(lon), lat > 27, lat < 44.5, lon > -18.5, lon < 4.5) |>
    distinct(ine, .keep_all = TRUE)
  if (nrow(m) < 5000) stop("Wikidata returned only ", nrow(m), " municipalities", call. = FALSE)
  tryCatch(write.csv(m, MUNI_FILE, row.names = FALSE, fileEncoding = "UTF-8"),
           error = \(e) warning("Could not save ", MUNI_FILE, call. = FALSE))
  m
}

# Place installations at their municipality: INE code, else name within the province
locate <- function(r, m) {
  m <- m |> mutate(prov = substr(ine, 1, 2))
  # Bilingual names ("Alicante/Alacant", "Donostia / San Sebastián") match on any part
  alt <- m |> mutate(part = strsplit(name, "\\s*/\\s*")) |> unnest(part) |>
    mutate(key = paste(prov, norm_muni(part))) |> distinct(key, .keep_all = TRUE)
  by_ine <- match(r$ine, m$ine)
  r$lat <- m$lat[by_ine]; r$lon <- m$lon[by_ine]; r$muni_how <- ifelse(is.na(by_ine), NA, "INE code")
  parts <- strsplit(coalesce(r$municipio, ""), "\\s*/\\s*")
  key1 <- paste(r$prov, norm_muni(vapply(parts, \(p) if (length(p)) p[1] else "", "")))
  key2 <- paste(r$prov, norm_muni(vapply(parts, \(p) if (length(p) > 1) p[2] else "", "")))
  j <- coalesce(match(key1, alt$key), match(key2, alt$key))
  use <- is.na(r$lat) & !is.na(j)
  r$lat[use] <- alt$lat[j[use]]; r$lon[use] <- alt$lon[j[use]]
  r$ine[use] <- alt$ine[j[use]]; r$muni_how[use] <- "name"
  r
}

# ---- 3. REE installed capacity (national reference) -------------------------

get_ree <- function() {
  if (is_offline()) return(NULL)
  r <- tryCatch(get_url(REE_API, ua = APP_UA, timeout = 60, query = list(
    start_date = "2010-01-01T00:00", end_date = sprintf("%d-12-31T23:59", CY),
    time_trunc = "year")), error = \(e) NULL)
  if (is.null(r) || resp_status(r) != 200) return(NULL)
  j <- tryCatch(resp_body_json(r, simplifyVector = FALSE), error = \(e) NULL)
  inc <- j$included
  k <- which(vapply(inc, \(x) grepl("fotovolt", tolower(coalesce(x$attributes$title, x$type, ""))), TRUE))[1]
  if (is.na(k)) return(NULL)
  v <- inc[[k]]$attributes$values
  tibble(year = as.integer(substr(vapply(v, \(x) x$datetime, ""), 1, 4)),
         mw = vapply(v, \(x) as.numeric(x$value), 0)) |>
    group_by(year) |> summarise(mw = last(mw), .groups = "drop")
}

# ---- Build + cache ----------------------------------------------------------

build_data <- function() {
  src <- get_raipee()
  r <- read_raipee(src$path)
  m <- tryCatch(get_municipios(), error = function(e) {
    warning("Municipality coordinates not available: ", conditionMessage(e), call. = FALSE); NULL })
  r <- if (!is.null(m)) locate(r, m) else mutate(r, lat = NA_real_, lon = NA_real_, muni_how = NA_character_)
  r <- r |> mutate(id = row_number(),
                   year = as.integer(format(online, "%Y")))
  list(version = CACHE_VERSION, inst = r, ree = get_ree(),
       raipee_name = src$name, raipee_from = src$from, raipee_cols = attr(r, "columns"),
       n_located = sum(!is.na(r$lat)), fetched = Sys.time())
}

load_data <- function() {
  cached <- if (file.exists(CACHE_FILE)) tryCatch(readRDS(CACHE_FILE), error = \(e) NULL)
  if (!is.null(cached) && !identical(cached$version, CACHE_VERSION)) cached <- NULL
  if (!is.null(cached)) {
    age <- difftime(Sys.time(), file.mtime(CACHE_FILE), units = "secs")
    if (age < CACHE_TTL) return(cached)
  }
  out <- tryCatch(build_data(), error = function(e) {
    if (is.null(cached)) stop(e)
    warning("Refresh failed, keeping cached data: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (is.null(out)) { Sys.setFileTime(CACHE_FILE, Sys.time()); return(cached) }
  saveRDS(out, CACHE_FILE)
  out
}

prep_data <- function(s) {
  I <- s$inst |> mutate(year_plot = ifelse(stage != "Registered" & !is.na(year) & year < CY, CY, year))
  yrs <- I$year_plot[!is.na(I$year_plot)]
  list(raw = s, I = I,
       yr_rng = if (length(yrs)) range(yrs) else c(CY - 15, CY),
       total_reg = sum(I$mw[I$stage == "Registered"]),
       mw_max = max(1, ceiling(max(I$mw))),
       ccaa = sort(unique(na.omit(I$ccaa))),
       provs = sort(unique(na.omit(I$provincia))))
}

current_data <- local({
  key <- NULL; val <- NULL
  function() {
    s <- load_data()
    k <- format(s$fetched, "%Y-%m-%d %H:%M:%OS3")
    if (!identical(k, key)) { val <<- prep_data(s); key <<- k }
    val
  }
})
data_or_error <- function() tryCatch(current_data(), error = function(e) e)
invisible(data_or_error())

# ---- Linear design tokens ---------------------------------------------------

lin <- list(
  bg = "#08090A", surface = "#0F1011", raised = "#141516",
  border = "rgba(255,255,255,0.08)", text = "#F7F8F8",
  muted = "#8A8F98", faint = "#62666D", accent = "#5E6AD2"
)

SIZE_BREAKS <- c(0, 5, 20, 100, 250, Inf)
SIZE_LABELS <- c("< 5 MW", "5 to 20", "20 to 100", "100 to 250", "> 250 MW")

# Bubble area proportional to MW (radius ~ sqrt(MW)), pixel radius
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
  /* OpenStreetMap standard tiles, recoloured to sit in the dark UI */
  .osm-dark { filter: invert(100%%) hue-rotate(180deg) grayscale(55%%) brightness(0.82) contrast(1.05); }
  .leaflet-container { background:%1$s; font-family:Inter, sans-serif; }
  .leaflet-popup-content-wrapper, .leaflet-popup-tip { background:#141516; color:%6$s;
       border:1px solid %4$s; box-shadow:0 8px 24px rgba(0,0,0,0.5); }
  .leaflet-popup-content { margin:12px 14px; font-size:0.78rem; line-height:1.55; }
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

fmt_date <- function(d) ifelse(is.na(d), "-", format(d, "%b %Y"))

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




SIZE_BREAKS_ES <- c(0, 0.1, 1, 5, 50, Inf)
SIZE_LABELS_ES <- c("< 100 kW", "0.1 to 1 MW", "1 to 5 MW", "5 to 50 MW", "> 50 MW")

no_data_page <- function(e) {
  page_fillable(
    theme = theme, title = "Spain Solar PV Fleet",
    card(card_header("Spain Solar PV Fleet: no data yet"),
         card_body(
           p("The app couldn't download the RAIPEE export from the Ministry's ELECTRA service."),
           tags$pre(conditionMessage(e)),
           p("Download it from ", a(RAIPEE_CSV, href = RAIPEE_CSV, target = "_blank"),
             " and save it next to app.R as raipee_produccion.csv, then restart."))))
}

# ---- UI ---------------------------------------------------------------------

ui <- function(req) {
d <- data_or_error()
if (inherits(d, "error")) return(no_data_page(d))

page_sidebar(
  theme = theme, fillable = FALSE, window_title = "Spain Solar PV Fleet",
  title = div(class = "brand", span(class = "logo"), "Spain Solar PV Fleet",
              span(class = "tag", "RAIPEE · ", textOutput("period_tag", inline = TRUE))),
  sidebar = sidebar(
    width = 290, bg = lin$bg,
    div(class = "sec", "Layers"),
    checkboxGroupInput("stages", NULL, choiceNames = stage_labels,
                       choiceValues = STAGES, selected = STAGES),
    div(class = "sec", "Filters"),
    selectizeInput("ccaa", "Comunidad autónoma", choices = d$ccaa, multiple = TRUE,
                   options = list(placeholder = "All")),
    selectizeInput("prov", "Provincia", choices = d$provs, multiple = TRUE,
                   options = list(placeholder = "All")),
    sliderInput("mw", "Installation size (MW)", min = 0, max = d$mw_max,
                value = c(1, d$mw_max), step = 0.1, ticks = FALSE),
    sliderInput("yr", "Year (in service or registered)", min = d$yr_rng[1], max = d$yr_rng[2],
                value = d$yr_rng, step = 1, sep = "", ticks = FALSE),
    checkboxInput("no_year", "Include installations without a date", value = TRUE),
    textInput("q", "Search installation or holder", placeholder = "e.g. Iberdrola"),
    actionButton("reset", "Reset filters", class = "btn-lin w-100"),
    hr(style = sprintf("border-color:%s; opacity:1; margin:14px 0 10px;", lin$border)),
    div(class = "sec", "Data"),
    actionButton("refresh", "Re-download from ELECTRA", icon = icon("rotate"),
                 class = "btn-lin w-100"),
    div(style = "height:10px"),
    uiOutput("meta")
  ),

  layout_columns(
    col_widths = c(3, 3, 3, 3), fill = FALSE,
    kpi("Registered",      STAGE_COLS[["Registered"]],     "k_reg", "k_reg_f"),
    kpi("Pre-registered",  STAGE_COLS[["Pre-registered"]], "k_pre", "k_pre_f"),
    kpi("Municipalities",  "#B59AF8",                      "k_mun", "k_mun_f"),
    kpi("Largest installation", "#D4D8FF",                 "k_big", "k_big_f")
  ),

  card(
    full_screen = TRUE, height = "640px",
    card_header("Installations by municipality", span(class = "sub",
      "one bubble per municipality and stage · placed at the municipality centroid · filled = registered, dashed ring = pre-registered")),
    card_body(padding = 0, leafletOutput("map", height = "100%"))
  ),

  card(
    full_screen = TRUE, height = "430px",
    card_header("Capacity trajectory", span(class = "sub",
      "RAIPEE additions by year (filtered) · REE national installed PV for reference")),
    card_body(plotlyOutput("trajectory", height = "100%"))
  ),

  layout_columns(
    col_widths = c(6, 6), fill = FALSE,
    card(height = "420px", full_screen = TRUE,
         card_header("Capacity by comunidad autónoma", span(class = "sub", "selected stages")),
         card_body(plotlyOutput("by_ccaa", height = "100%"))),
    card(height = "420px", full_screen = TRUE,
         card_header("Installation size distribution", span(class = "sub", "count by stage")),
         card_body(plotlyOutput("by_size", height = "100%")))
  ),

  card(full_screen = TRUE,
       card_header("Capacity by holder", span(class = "sub", "RAIPEE titular · MW · respects all filters")),
       card_body(DTOutput("owners"))),

  card(full_screen = TRUE,
       card_header("Installations", span(class = "sub", "select a row to fly to its municipality")),
       card_body(DTOutput("inst_tbl")))
)
}

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {
  d0 <- data_or_error()
  if (inherits(d0, "error")) return(invisible())
  D <- reactiveVal(d0)

  output$period_tag <- renderText(format(D()$raw$fetched, "%d %b %Y"))

  output$meta <- renderUI({
    s <- D()$raw
    div(class = "meta",
        "Installations: ", a("RAIPEE", href = RAIPEE_PAGE, target = "_blank"),
        " via the Ministry's ELECTRA service (", s$raipee_name,
        if (identical(s$raipee_from, "local file")) ", local copy", "). Solar PV rows only. ",
        "Pre-registered means inscripción previa: authorised but not yet in service.",
        br(), br(),
        sprintf("Locations: %s of %s installations placed at their municipality centroid (Wikidata); the rest are in the tables only.",
                format(s$n_located, big.mark = ","), format(nrow(s$inst), big.mark = ",")),
        br(), br(),
        if (!is.null(s$ree)) tagList("Reference line: national installed solar PV from ",
          a("Red Eléctrica REData", href = "https://www.ree.es/es/apidatos", target = "_blank"), ".")
        else "Red Eléctrica's REData API was not reachable this build, so there is no reference line.",
        br(), br(),
        "Map data © ", a("OpenStreetMap", href = "https://www.openstreetmap.org/copyright",
                         target = "_blank"), " contributors.",
        br(), sprintf("Built %s", format(s$fetched, "%Y-%m-%d %H:%M")))
  })

  observeEvent(input$refresh, {
    id <- showNotification("Downloading RAIPEE from ELECTRA…", duration = NULL, type = "message")
    on.exit(removeNotification(id), add = TRUE)
    res <- tryCatch(build_data(), error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(paste("Refresh failed:", conditionMessage(res)), type = "error", duration = 8)
    } else {
      saveRDS(res, CACHE_FILE); new <- prep_data(res); D(new)
      updateSelectizeInput(session, "ccaa", choices = new$ccaa, selected = intersect(input$ccaa, new$ccaa))
      updateSelectizeInput(session, "prov", choices = new$provs, selected = intersect(input$prov, new$provs))
      updateSliderInput(session, "mw", max = new$mw_max)
      updateSliderInput(session, "yr", min = new$yr_rng[1], max = new$yr_rng[2], value = new$yr_rng)
      showNotification("RAIPEE updated.", type = "message", duration = 5)
    }
  })

  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "stages", selected = STAGES)
    updateSelectizeInput(session, "ccaa", selected = character(0))
    updateSelectizeInput(session, "prov", selected = character(0))
    updateSliderInput(session, "mw", value = c(1, D()$mw_max))
    updateSliderInput(session, "yr", value = D()$yr_rng)
    updateCheckboxInput(session, "no_year", value = TRUE)
    updateTextInput(session, "q", value = "")
  })

  q_d <- debounce(reactive(input$q), 300)

  inst_all <- reactive({
    d <- D()$I |>
      filter((is.na(year_plot) & isTRUE(input$no_year)) |
             (!is.na(year_plot) & year_plot >= input$yr[1] & year_plot <= input$yr[2]),
             mw >= input$mw[1], mw <= input$mw[2])
    if (length(input$ccaa)) d <- filter(d, ccaa %in% input$ccaa)
    if (length(input$prov)) d <- filter(d, provincia %in% input$prov)
    q <- trimws(q_d())
    if (nzchar(q)) d <- filter(d, grepl(q, plant, ignore.case = TRUE) | grepl(q, holder, ignore.case = TRUE))
    d
  })
  inst <- reactive(inst_all() |> filter(stage %in% input$stages))

  # Municipality bubbles
  munis <- reactive({
    inst() |> filter(!is.na(lat)) |>
      group_by(ine, stage) |>
      summarise(municipio = first(municipio), provincia = first(provincia), lat = first(lat),
                lon = first(lon), mw = sum(mw), n = n(),
                top = paste(head(names(sort(tapply(mw, holder, sum), decreasing = TRUE)), 3),
                            collapse = "; "),
                largest = plant[which.max(mw)], largest_mw = max(mw), .groups = "drop") |>
      mutate(stage = as.character(stage), radius = bubble_r(mw))
  })

  ssum <- function(st) { s <- inst_all() |> filter(stage %in% st); list(mw = sum(s$mw), n = nrow(s)) }
  output$k_reg   <- renderText(fmt_mw(ssum("Registered")$mw))
  output$k_reg_f <- renderText(sprintf("%s installations · %.0f%% of registered PV",
    format(ssum("Registered")$n, big.mark = ","), 100 * ssum("Registered")$mw / max(D()$total_reg, 1)))
  output$k_pre   <- renderText(fmt_mw(ssum("Pre-registered")$mw))
  output$k_pre_f <- renderText({
    s <- ssum("Pre-registered"); r <- ssum("Registered")$mw
    sprintf("%s installations · %s of registered", format(s$n, big.mark = ","),
            if (r > 0) sprintf("%.0f%%", 100 * s$mw / r) else "-")
  })
  output$k_mun   <- renderText(format(n_distinct(inst_all()$ine[!is.na(inst_all()$ine)]), big.mark = ","))
  output$k_mun_f <- renderText({
    t <- inst_all() |> filter(!is.na(municipio)) |> group_by(municipio, provincia) |>
      summarise(mw = sum(mw), .groups = "drop") |> slice_max(mw, n = 1, with_ties = FALSE)
    if (nrow(t)) sprintf("most: %s (%s), %s", t$municipio, na_dash(t$provincia), fmt_mw(t$mw)) else ""
  })
  big <- reactive(inst_all() |> slice_max(mw, n = 1, with_ties = FALSE))
  output$k_big   <- renderText(if (nrow(big())) fmt_mw(big()$mw) else "-")
  output$k_big_f <- renderText(if (nrow(big())) sprintf("%s, %s", big()$plant, na_dash(big()$municipio)) else "")

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
    d <- munis()
    proxy <- leafletProxy("map") |> clearGroup("sites") |> clearGroup("focus")
    if (!nrow(d)) return()
    col <- unname(STAGE_COLS[d$stage])
    pop <- sprintf(
      "<span class='pop-stage' style='color:%s'>%s</span><br>
       <b>%s</b> <span class='pop-k'>(%s)</span><br>
       <span class='pop-k'>Capacity</span> %s MW &middot; %d installation%s<br>
       <span class='pop-k'>Largest</span> %s (%s MW)<br>
       <span class='pop-k'>Top holders</span> %s",
      col, d$stage, htmlEscape(na_dash(d$municipio)), htmlEscape(na_dash(d$provincia)),
      formatC(d$mw, format = "f", digits = 1, big.mark = ","), d$n, ifelse(d$n == 1, "", "s"),
      htmlEscape(d$largest), formatC(d$largest_mw, format = "f", digits = 1), htmlEscape(d$top))
    lab <- sprintf("%s · %s MW · %s", d$municipio, formatC(d$mw, format = "f", digits = 1), d$stage)
    reg <- d$stage == "Registered"
    if (any(reg)) proxy |> addCircleMarkers(
      lng = d$lon[reg], lat = d$lat[reg], radius = d$radius[reg], group = "sites",
      stroke = TRUE, weight = 0.8, color = col[reg], opacity = 0.9,
      fillColor = col[reg], fillOpacity = 0.45, popup = pop[reg], label = lab[reg])
    if (any(!reg)) proxy |> addCircleMarkers(
      lng = d$lon[!reg], lat = d$lat[!reg], radius = d$radius[!reg], group = "sites",
      stroke = TRUE, weight = 1.4, dashArray = "3,3", color = col[!reg], opacity = 0.95,
      fillColor = col[!reg], fillOpacity = 0.10, popup = pop[!reg], label = lab[!reg])
  })

  output$trajectory <- renderPlotly({
    g <- inst() |> filter(!is.na(year_plot))
    shiny::validate(shiny::need(nrow(g) > 0, "No installations with a date for the current filters."))
    yrs <- seq(min(g$year_plot), max(c(g$year_plot, CY)))
    annual <- g |> group_by(year_plot, stage) |> summarise(gw = sum(mw) / 1000, .groups = "drop") |>
      complete(year_plot = yrs, stage = factor(STAGES, STAGES), fill = list(gw = 0))
    p <- plot_ly()
    for (s in intersect(STAGES, input$stages)) {
      a <- filter(annual, stage == s, gw > 0)
      if (nrow(a)) p <- p |> add_bars(data = a, x = ~year_plot, y = ~gw, name = s,
        marker = list(color = STAGE_COLS[[s]], opacity = if (s == "Registered") 1 else 0.85),
        hovertemplate = paste0(s, " · %{x}: +%{y:.2f} GW<extra></extra>"))
    }
    cum <- annual |> filter(stage == "Registered") |> arrange(year_plot) |> mutate(c = cumsum(gw))
    if ("Registered" %in% input$stages)
      p <- p |> add_lines(data = cum, x = ~year_plot, y = ~c, yaxis = "y2",
                          name = "RAIPEE registered (cumulative, filtered)",
                          line = list(color = "#D4D8FF", width = 2),
                          hovertemplate = "%{x}: %{y:.1f} GW<extra></extra>")
    ree <- D()$raw$ree
    if (!is.null(ree) && nrow(ree))
      p <- p |> add_lines(data = filter(ree, year >= min(yrs)), x = ~year, y = ~I(mw / 1000),
                          yaxis = "y2", name = "REE national installed PV",
                          line = list(color = "#4CB7A5", width = 1.6, dash = "dash"),
                          hovertemplate = "REE %{x}: %{y:.1f} GW<extra></extra>")
    p |> plotly_lin(
      barmode = "stack", bargap = 0.22, showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = 1.13, font = list(size = 10, color = lin$muted),
                    bgcolor = "rgba(0,0,0,0)"),
      xaxis = list(title = "", dtick = 2, range = c(max(min(yrs), CY - 20) - 0.5, max(yrs) + 0.5)),
      yaxis = list(title = list(text = "GW added per year", font = list(size = 10))),
      yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE, rangemode = "tozero",
                    title = list(text = "cumulative GW", font = list(size = 10)),
                    tickfont = list(color = lin$faint)),
      margin = list(l = 8, r = 44, t = 34, b = 6))
  })

  output$by_ccaa <- renderPlotly({
    s <- inst() |> filter(!is.na(ccaa)) |> group_by(ccaa, stage) |>
      summarise(gw = sum(mw) / 1000, .groups = "drop")
    shiny::validate(shiny::need(nrow(s) > 0, "No installations for the current filters."))
    ord <- s |> group_by(ccaa) |> summarise(t = sum(gw)) |> arrange(t)
    s <- mutate(s, ccaa = factor(ccaa, ord$ccaa))
    p <- plot_ly()
    for (st in intersect(STAGES, unique(as.character(s$stage))))
      p <- p |> add_bars(data = filter(s, stage == st), x = ~gw, y = ~ccaa, orientation = "h",
                         name = st, marker = list(color = STAGE_COLS[[st]]),
                         hovertemplate = paste0(st, " · %{y}: %{x:.2f} GW<extra></extra>"))
    p |> plotly_lin(barmode = "stack", bargap = 0.3,
                    xaxis = list(title = list(text = "GW", font = list(size = 10))),
                    yaxis = list(title = "", tickfont = list(color = lin$muted)))
  })

  output$by_size <- renderPlotly({
    z <- inst() |> mutate(size_class = cut(mw, SIZE_BREAKS_ES, SIZE_LABELS_ES, right = FALSE)) |>
      count(size_class, stage, .drop = FALSE)
    shiny::validate(shiny::need(sum(z$n) > 0, "No installations for the current filters."))
    p <- plot_ly()
    for (st in intersect(STAGES, input$stages))
      p <- p |> add_bars(data = filter(z, stage == st), x = ~size_class, y = ~n, name = st,
                         marker = list(color = STAGE_COLS[[st]]),
                         hovertemplate = paste0(st, " · %{x}: %{y:,} installations<extra></extra>"))
    p |> plotly_lin(barmode = "group", bargap = 0.25,
                    xaxis = list(title = "", tickfont = list(color = lin$muted)), yaxis = list(title = ""))
  })

  output$owners <- renderDT({
    d <- inst_all() |> group_by(Holder = holder) |>
      summarise(Installations = n(),
                `Registered MW` = sum(mw[stage == "Registered"]),
                `Pre-registered MW` = sum(mw[stage == "Pre-registered"]),
                Provinces = n_distinct(provincia),
                `Largest` = sprintf("%s (%s MW)", plant[which.max(mw)], formatC(max(mw), format = "f", digits = 1)),
                .groups = "drop") |>
      mutate(`Share of registered` = `Registered MW` / max(sum(`Registered MW`), 1)) |>
      relocate(`Share of registered`, .after = `Registered MW`) |>
      arrange(desc(`Registered MW`))
    datatable(d, rownames = FALSE, selection = "none", extensions = "Buttons",
              style = "bootstrap5", class = "compact hover",
              options = list(pageLength = 15, dom = "Bftip", buttons = list("csv", "excel"),
                             order = list(list(2, "desc")),
                             columnDefs = list(list(className = "dt-right", targets = 1:5)))) |>
      formatRound(c("Registered MW", "Pre-registered MW"), digits = 1) |>
      formatPercentage("Share of registered", digits = 1) |>
      formatStyle("Registered MW",
                  background = styleColorBar(c(0, max(d$`Registered MW`, 1)), "rgba(94,106,210,0.35)"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  })

  inst_tbl_data <- reactive(inst() |> arrange(desc(mw)))
  output$inst_tbl <- renderDT({
    datatable(
      inst_tbl_data() |> transmute(Installation = plant, Stage = stage, Holder = holder,
                                   Municipality = municipio, Province = provincia, `C. A.` = ccaa,
                                   MW = round(mw, 2), Date = ifelse(is.na(online), "-", format(online, "%Y-%m-%d")),
                                   Registry = reg, Map = ifelse(is.na(lat), "not mapped", "")),
      rownames = FALSE, selection = "single", style = "bootstrap5", class = "compact hover",
      extensions = "Buttons",
      options = list(pageLength = 12, dom = "Bftip", buttons = list("csv", "excel"),
                     order = list(list(6, "desc")),
                     columnDefs = list(list(className = "dt-right", targets = 6)))) |>
      formatStyle("Stage", color = styleEqual(STAGES, unname(STAGE_COLS)))
  })

  observeEvent(input$inst_tbl_rows_selected, {
    r <- inst_tbl_data()[input$inst_tbl_rows_selected, ]
    if (is.na(r$lat)) { showNotification("This installation's municipality wasn't matched.", duration = 4); return() }
    leafletProxy("map") |> flyTo(r$lon, r$lat, zoom = 11, options = list(duration = 0.8)) |>
      clearGroup("focus") |>
      addCircleMarkers(lng = r$lon, lat = r$lat, radius = 16, group = "focus", stroke = TRUE,
                       color = "#FFFFFF", weight = 1.5, opacity = 0.9, fill = FALSE)
  })
}

shinyApp(ui, server)
