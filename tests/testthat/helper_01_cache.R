cacheURL <- "http://basejump.seq.cloud"
files <- c(
    "dmelanogaster.gtf",
    "example.counts",
    "example.csv",
    "example.rda",
    "example.rds",
    "example.tsv",
    "example.txt",
    "example.yaml",
    "gr.rda",
    "mmusculus.gtf",
    "mn.rda",
    "multi.rda",
    "plotlist.rda",
    "renamed.rda",
    "rnaseqCounts.csv.gz",
    "serialized.rds",
    "singleCellCounts.mtx.gz",
    "singleCellCounts.mtx.gz.colnames",
    "singleCellCounts.mtx.gz.rownames"
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
