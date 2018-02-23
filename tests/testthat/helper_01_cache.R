cacheURL <- "http://basejump.seq.cloud"
mapply(
    FUN = function(cacheURL, file) {
        if (!file_exists(file)) {
            download.file(url = paste(cacheURL, file, sep = "/"), destfile = file)
        }
    },
    file = c(
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
        "test.counts"
    ),
    MoreArgs = list(cacheURL = cacheURL)
)
