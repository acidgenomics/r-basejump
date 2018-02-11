extdataDir <- file.path("inst", "extdata")
dir.create(extdataDir, recursive = TRUE, showWarnings = FALSE)

save(
    mtcars,
    file = file.path(extdataDir, "mtcars.rda"),
    compress = "xz"
)

# Note that `readr::write_csv()` never writes rownames
write.csv(mtcars, file.path(extdataDir, "mtcars.csv"))
R.utils::gzip(
    filename = file.path(extdataDir, "mtcars.csv"),
    overwrite = TRUE,
    remove = FALSE
)
