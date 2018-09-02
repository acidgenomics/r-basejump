# Read/write file examples
# Last updated 2018-09-02
# Note that `readr::write_*()` functions never write rownames.

library(tidyverse)

# Coerce to tibble before writing.
example <- as(datasets::mtcars, "tbl_df")
stopifnot("rowname" %in% colnames(example))

save(example, file = "data-raw/example.rda", compress = FALSE)
saveRDS(example, file = "data-raw/example.rds", compress = FALSE)

write_csv(mtcars, "data-raw/example.csv")
write_csv(mtcars, "data-raw/example.csv.gz")
write_tsv(mtcars, "data-raw/example.tsv")
write_tsv(mtcars, "data-raw/example.tsv.gz")
