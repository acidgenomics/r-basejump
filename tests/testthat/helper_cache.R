files <- c(
    "ensembl.gff3.gz",
    "ensembl.gtf.gz",
    "example.counts",
    "example.csv",
    "example.gtf",
    "example.gff3",
    "example.json",
    "example.R",
    "example.rda",
    "example.rds",
    "example.tsv",
    "example.txt",
    "example.yml",
    "flybase.gtf.gz",
    "gencode.gff3.gz",
    "gencode.gtf.gz",
    "gr.rda",
    "mn.rda",
    "multi.rda",
    "plotlist.rda",
    "refseq.gff.gz",
    "renamed.rda",
    "rnaseq_counts.csv.gz",
    "serialized.rds",
    "single_cell_counts.mtx.gz",
    "single_cell_counts.mtx.gz.colnames",
    "single_cell_counts.mtx.gz.rownames",
    "wormbase.gtf.gz"
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
    MoreArgs = list(
        cacheURL = basejumpCacheURL,
        envir = environment()
    )
)
