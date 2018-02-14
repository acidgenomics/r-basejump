# Note that `readr::write_*()` functions never write rownames
library(tidyverse)

extdataDir <- file.path("inst", "extdata")
dir.create(extdataDir, recursive = TRUE, showWarnings = FALSE)

save(
    mtcars,
    file = file.path(extdataDir, "mtcars.rda"),
    compress = "xz"
)

# Coerce to tibble before writing
mtcars <- mtcars %>%
    rownames_to_column() %>%
    as_tibble()

write_csv(mtcars, file.path(extdataDir, "mtcars.csv"))
write_csv(mtcars, file.path(extdataDir, "mtcars.csv.gz"))

write_tsv(mtcars, file.path(extdataDir, "mtcars.tsv"))
write_tsv(mtcars, file.path(extdataDir, "mtcars.tsv.gz"))
