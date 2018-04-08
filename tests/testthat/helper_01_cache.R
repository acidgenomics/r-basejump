cacheURL <- "http://basejump.seq.cloud"
files <- c(
    "dmelanogaster.gtf",
    "gr.rda",
    "mmusculus.gtf",
    "mn.rda",
    "mtcars.csv",
    "mtcars.tsv",
    "mtcars.txt",
    "multi.rda",
    "plotlist.rda",
    "renamed.rda",
    "rnaseqCounts.csv.gz",
    "singleCellCounts.mtx.gz",
    "singleCellCounts.mtx.gz.colnames",
    "singleCellCounts.mtx.gz.rownames",
    "summary.yaml"
)
mapply(
    FUN = function(cacheURL, file, envir) {
        if (!file.exists(file)) {
            utils::download.file(
                url = paste(cacheURL, file, sep = "/"),
                destfile = file)
        }
        # Load R Data file
        if (grepl("\\.rda$", file)) {
            message(paste("Loading", file))
            load(file, envir = envir)
        }
    },
    file = files,
    MoreArgs = list(cacheURL = cacheURL, envir = environment())
)
