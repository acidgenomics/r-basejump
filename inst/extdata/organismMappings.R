library(basejump)
library(here)
library(usethis)
here <- here()
organismMappings <-
    file.path(here, "data-raw", "organismMappings.csv") %>%
    import() %>%
    as_tibble()
use_data(organismMappings, overwrite = TRUE)
