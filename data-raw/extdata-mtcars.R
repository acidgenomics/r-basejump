# Note that `readr::write_*()` functions never write rownames
library(tidyverse)

# Coerce to tibble before writing
mtcars <- mtcars %>%
    rownames_to_column() %>%
    as_tibble()

extdataDir <- "inst/extdata"
write_csv(mtcars, file.path(extdataDir, "mtcars.csv"))
write_csv(mtcars, file.path(extdataDir, "mtcars.csv.gz"))
write_tsv(mtcars, file.path(extdataDir, "mtcars.tsv"))
write_tsv(mtcars, file.path(extdataDir, "mtcars.tsv.gz"))
