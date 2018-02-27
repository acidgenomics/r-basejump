cacheURL <- "http://basejump.seq.cloud"
files <- c(
    "annotable_AsIs.rda",
    "counts.rda",
    "dmelanogaster.gtf",
    "grch37.rda",
    "grch37Tx2gene.rda",
    "grch38.rda",
    "makeNames.rda",
    "mmusculus.gtf",
    "mtcars.csv",
    "mtcars.rda",
    "mtcars.tsv",
    "mtcars.txt",
    "mtcars.xlsx",
    "multi.rda",
    "plotlist.rda",
    "renamed.rda",
    "sparse.mtx",
    "starwars.rda",
    "summary.yaml",
    "test.colnames",
    "test.counts")
mapply(
    FUN = function(cacheURL, file, envir) {
        if (!file_exists(file)) {
            download.file(
                url = paste(cacheURL, file, sep = "/"),
                destfile = file)
        }
        # Load R Data file
        if (grepl("\\.rda$", file)) {
            inform(paste("Loading", file))
            load(file, envir = envir)
        }
    },
    file = files,
    MoreArgs = list(cacheURL = cacheURL, envir = environment())
)
