invisible(lapply(
    X = c(
        "cog.txt",
        "eunog.tsv.gz",
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
        "example.xlsx",
        "example.yml",
        "gr.rda",
        "hgnc.txt.gz",
        "homo_sapiens.gene_info.gz",
        "mgi.rpt.gz",
        "mn.rda",
        "multi.rda",
        "nog.tsv.gz",
        "pthr13.1_human.gz",
        "plotlist.rda",
        "renamed.rda",
        "rnaseq_counts.csv.gz",
        "serialized.rds",
        "single_cell_counts.mtx.gz",
        "single_cell_counts.mtx.gz.colnames",
        "single_cell_counts.mtx.gz.rownames"
    ),
    FUN = function(file, url) {
        if (!file.exists(file)) {
            utils::download.file(
                url = paste(url, file, sep = "/"),
                destfile = file
            )
        }
    },
    url = basejumpCacheURL
))
