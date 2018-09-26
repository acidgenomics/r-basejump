files <- c(
    "example.counts",
    "example.csv",
    "example.gtf",
    "example.gff3",
    "example.json",
    "example.rda",
    "example.rds",
    "example.tsv",
    "example.txt",
    "example.yml",
    "gr.rda",
    "mn.rda",
    "multi.rda",
    "plotlist.rda",
    "renamed.rda",
    "rnaseq_counts.csv.gz",
    "serialized.rds",
    "single_cell_counts.mtx.gz",
    "single_cell_counts.mtx.gz.colnames",
    "single_cell_counts.mtx.gz.rownames"
)
mapply(
    FUN = function(cacheURL, file, envir) {
        if (!file.exists(file)) {
            utils::download.file(
                url = paste(cacheURL, file, sep = "/"),
                destfile = file
            )
        }
    },
    file = files,
    MoreArgs = list(cacheURL = cacheURL, envir = environment())
)
