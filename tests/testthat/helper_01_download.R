mapply(
    FUN = function(url, file) {
        if (!file_exists(file)) {
            download.file(url = paste(url, file, sep = "/"), destfile = file)
        }
    },
    file = c(
        "annotable_AsIs.rda",
        "counts.rda",
        "dmelanogaster.gtf",
        "grch38.rda",
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
        "test.colnames",
        "test.counts"
    ),
    MoreArgs = list(url = url)
)
