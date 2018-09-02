# Read/write file examples
# Last updated 2018-09-02
# Note that `readr::write_*()` functions never write rownames.

library(tidyverse)
dir <- "tests/testthat"

# Coerce to tibble before writing.
example <- as(datasets::mtcars, "tbl_df")
stopifnot("rowname" %in% colnames(example))

# Compression can cause some AppVeyor CI checks to fail.
save(example, file = file.path(dir, "example.rda"), compress = FALSE)
saveRDS(example, file = file.path(dir, "example.rds"), compress = FALSE)

write_csv(mtcars, file.path(dir, "example.csv"))
write_csv(mtcars, file.path(dir, "example.csv.gz"))
write_tsv(mtcars, file.path(dir, "example.tsv"))
write_tsv(mtcars, file.path(dir, "example.tsv.gz"))
