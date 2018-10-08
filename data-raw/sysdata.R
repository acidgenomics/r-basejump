# Internal package data (sysdata.rda)
# 2018-10-08

library(googlesheets)
library(tidyverse)

# Must be interactive, requiring Google Sheets authentication.
stopifnot(interactive())
View(gs_ls())

# Define NA strings.
na <- c("", "#N/A")

# Gender markers ===============================================================
key <- "1ooX66V8sVC-wXlLF0ahtwHGh22PsIXIWcsyzzCpZj5U"
gs <- gs_key(key)
ws <- gs_ws_ls(gs)
gender_markers <- lapply(
    X = ws,
    FUN = function(ws) {
        gs %>%
            gs_read(ws = ws, na = na) %>%
            filter(include == TRUE) %>%
            select(-include)
    }) %>%
    set_names(camel(ws))

# Organism mappings ============================================================
key <- "1IxM6wsbdE47SOEKXDw7DjHdi8m8BuTu-KB6aa8jypNU"
organism_mappings <-
    gs_key(key) %>%
    gs_read(na = na) %>%
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
devtools::use_data(
    gender_markers, organism_mappings,
    internal = TRUE, overwrite = TRUE, compress = "xz"
)
