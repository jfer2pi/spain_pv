# Refresh the data committed next to app.R so a deployed app starts by reading
# files (Posit Connect Cloud allows 60 s to start):
#   es_ree_pv.csv  Red Eléctrica's national installed PV by year
#   es_solar.rds   the app's pre-built data from the GEM workbook
# GEM's download is form-gated, so a new tracker release is added by hand.
# Run from the repo root: Rscript scripts/refresh_data.R

src <- readLines("app.R", encoding = "UTF-8")
stop_at <- grep("^# ---- Linear design tokens", src)[1]
eval(parse(text = src[seq_len(stop_at - 1)]))

ree <- get_ree(live = TRUE)
message("REE: ", if (is.null(ree)) "unavailable" else paste(nrow(ree), "years, latest", max(ree$year)))

out <- build_data()
saveRDS(out, CACHE_FILE)
g <- out$gens
message("Pre-built ", CACHE_FILE, " from ", out$gem_file, ": ", nrow(g), " phases at ",
        length(unique(g$loc_id)), " sites; ",
        paste(sprintf("%s %.1f GW", levels(g$stage), tapply(g$mw, g$stage, sum) / 1000), collapse = ", "))
