dir.create("cache", showWarnings = FALSE)
files <- c(
    "example.gtf",
    "example.gff3",
    "plotlist.rda",
    "sce_lanesplit.rds"
)
mapply(
    FUN = function(remoteDir, file, envir) {
        destfile <- file.path("cache", file)
        if (!file.exists(destfile)) {
            utils::download.file(
                url = paste(remoteDir, file, sep = "/"),
                destfile = destfile
            )
        }
    },
    file = files,
    MoreArgs = list(
        remoteDir = basejumpTestsURL,
        envir = environment()
    )
)
