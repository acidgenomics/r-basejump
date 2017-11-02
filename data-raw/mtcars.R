save(mtcars,
     file = "~/Desktop/mtcars.rda",
     compress = "xz")
readr::write_csv(mtcars, "~/Desktop/mtcars.csv")
readr::write_csv(mtcars, "~/Desktop/mtcars.csv.gz")
