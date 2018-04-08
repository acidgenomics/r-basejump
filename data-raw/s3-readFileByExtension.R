# Note that `readr::write_*()` functions never write rownames
library(tidyverse)

# Coerce to tibble before writing
example <- datasets::mtcars %>%
    rownames_to_column() %>%
    as_tibble()

write_csv(mtcars, "~/example.csv")
write_csv(mtcars, "~/example.csv.gz")
write_tsv(mtcars, "~/example.tsv")
write_tsv(mtcars, "~/example.tsv.gz")
