extdataDir <- file.path("inst", "extdata")
dir.create(extdataDir, recursive = TRUE, showWarnings = FALSE)

save(mtcars,
     file = file.path(extdataDir, "mtcars.rda"),
     compress = "xz")

readr::write_csv(mtcars, file.path(extdataDir, "mtcars.csv"))
readr::write_csv(mtcars, file.path(extdataDir, "mtcars.csv.gz"))
