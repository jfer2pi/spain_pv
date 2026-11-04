# Refresh the data committed next to app.R, so a deployed app starts by reading
# files instead of downloading them (Posit Connect Cloud allows 60 s to start).
#   raipee_produccion.csv  RAIPEE export from ELECTRA, when it answers
#   es_municipios.csv      municipality coordinates from Wikidata
#   es_ree_pv.csv          REE national installed PV by year
#   es_solar.rds           the app's pre-built data
# Run from the repo root: Rscript scripts/refresh_data.R

src <- readLines("app.R", encoding = "UTF-8")
stop_at <- grep("^data_or_error <- function", src)[1]
eval(parse(text = src[seq_len(stop_at - 1)]))
options(es_pv.live = TRUE)

# RAIPEE: keep the download as raipee_produccion.csv when ELECTRA answers
res <- tryCatch(get_raipee(), error = function(e) { message("RAIPEE: ", conditionMessage(e)); NULL })
if (!is.null(res) && identical(res$from, "download")) {
  ext <- if (grepl("xlsx", res$name)) "xlsx" else "csv"
  old <- setdiff(list.files(".", pattern = RAIPEE_LOCAL_PATTERN, ignore.case = TRUE),
                 paste0("raipee_produccion.", ext))
  file.copy(res$path, paste0("raipee_produccion.", ext), overwrite = TRUE)
  unlink(old)
  message("RAIPEE: saved raipee_produccion.", ext, " (", format(file.size(res$path), big.mark = ","), " bytes)")
} else if (!is.null(res)) message("RAIPEE: ELECTRA unavailable, kept ", res$name)

m <- tryCatch(get_municipios(), error = function(e) { message("Wikidata: ", conditionMessage(e)); NULL })
if (!is.null(m)) message("Municipalities: ", nrow(m))
ree <- get_ree()
message("REE: ", if (is.null(ree)) "unavailable" else paste(nrow(ree), "years"))

# Pre-build the app's data from the files now on disk
tryCatch({
  options(es_pv.live = FALSE)
  out <- build_data()
  saveRDS(out, CACHE_FILE)
  message("Pre-built ", CACHE_FILE, ": ", nrow(out$inst), " installations, ",
          out$n_located, " placed; RAIPEE columns: ", paste(out$raipee_cols, collapse = ", "))
}, error = function(e) message("Pre-build failed: ", conditionMessage(e)))
