options(
    basejump.save.dir = ".",
    basejump.save.ext = "rds",
    basejump.test = TRUE
)

extdata <- system.file("extdata", package = "basejump")
stopifnot(dir.exists(extdata))
