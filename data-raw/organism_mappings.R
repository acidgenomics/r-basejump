# Organism Mappings
# 2018-09-25

# Must be interactive, requiring Google Sheets authentication
stopifnot(interactive())

library(googlesheets)
library(tidyverse)

# View(gs_ls())
organism_mappings <-
    gs_key("1IxM6wsbdE47SOEKXDw7DjHdi8m8BuTu-KB6aa8jypNU") %>%
    gs_read(na = c("", "#N/A")) %>%
    camel() %>%
    select(-notes)

devtools::use_data(organism_mappings, compress = "xz", overwrite = TRUE)
