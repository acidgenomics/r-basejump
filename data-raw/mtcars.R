save(mtcars,
     file = file.path(testDataDir, "mtcars.rda"),
     compress = "xz")
readr::write_csv(mtcars, file.path(testDataDir, "mtcars.csv"))
readr::write_csv(mtcars, file.path(testDataDir, "mtcars.csv.gz"))
