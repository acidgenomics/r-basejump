# Google Sheets data sets
# 2018-11-19

library(googlesheets)
library(tidyverse)

# Must be interactive, requiring Google Sheets authentication.
stopifnot(interactive())
View(gs_ls())

# Gender markers ===============================================================
key <- "1ooX66V8sVC-wXlLF0ahtwHGh22PsIXIWcsyzzCpZj5U"
gs <- gs_key(key)
ws <- gs_ws_ls(gs)
gender_markers <- lapply(
    X = ws,
    FUN = function(ws) {
        gs %>%
            gs_read(ws = ws, na = naStrings) %>%
            filter(include == TRUE) %>%
            select(-include)
    }) %>%
    set_names(camel(ws))

# Organism mappings ============================================================
key <- "1IxM6wsbdE47SOEKXDw7DjHdi8m8BuTu-KB6aa8jypNU"
organism_mappings <-
    gs_key(key) %>%
    gs_read(na = naStrings) %>%
    camel() %>%
    select(-notes) %>%
    mutate(
        organismGrep = gsub(
            pattern = "^([[:upper:]])([[:lower:]]+)[[:space:]]([[:lower:]]+)$",
            replacement = "\\1(\\2)?([._[:space:]]+)?\\3",
            x = organism
        )
    )

# Save =========================================================================
usethis::use_data(
    gender_markers, organism_mappings,
    overwrite = TRUE, compress = "xz"
)
