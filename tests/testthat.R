options(stringsAsFactors = FALSE)

library(testthat)
library(basejump)

# Download files required to test `loadData()` and `loadDataAsName()`
utils::download.file(
    url = "http://basejump.seq.cloud/mtcars.rda",
    destfile = "mtcars.rda",
    quiet = TRUE)
utils::download.file(
    url = "http://basejump.seq.cloud/starwars.rda",
    destfile = "starwars.rda",
    quiet = TRUE)

test_check("basejump")

# Clean up the downloaded R data files
unlink(c("mtcars.rda", "starwars.rda"))
