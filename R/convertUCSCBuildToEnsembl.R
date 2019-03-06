#' Convert UCSC build to Ensembl
#'
#' @export
#'
#' @inheritParams params
#'
#' @return `character`. Ensembl genome build as the value, UCSC build as the
#'   name. Stops on match failure.
#'
#' @seealso
#' - [UCSC hgGateway](https://genome.ucsc.edu/cgi-bin/hgGateway)
#' - [bcbio genome recipes](https://github.com/bcbio/bcbio-nextgen/tree/master/config/genomes)
#'
#' @examples
#' from <- c("hg19", "hg38")
#' to <- convertUCSCBuildToEnsembl(from)
#' print(to)
convertUCSCBuildToEnsembl <- function(object) {
    assert(isCharacter(object))
    ucsc <- .ucscMatrix[, "ucsc"]
    ensembl <- .ucscMatrix[, "ensembl"]
    match <- match(x = object, table = ucsc)
    # Stop on any match failure.
    if (any(is.na(match))) {
        stop(paste(
            "Failed to match UCSC to Ensembl:",
            toString(object[which(is.na(match))], width = 100L)
        ))
    }
    out <- ensembl[match]
    names(out) <- ucsc[match]
    out
}



.ucscMatrix <- matrix(
    dimnames = list(NULL, c(
        "ucsc",       "ensembl",     "date",    "organism"
    )),
    data = c(
        "araTha1",    "TAIR10",      "2011-02", "Arabidopsis thaliana",
        "canFam3",    "CanFam3.1",   "2011-09", "Canis lupus familiaris",
        "ce10",       "WS220",       "2010-10", "Caenorhabditis elegans",
        "ce11",       "WBcel235",    "2013-02", "Caenorhabditis elegans",
        "danRer10",   "GRCz10",      "2014-09", "Danio rerio",
        "danRer11",   "GRCz11",      "2017-05", "Danio rerio",
        "dm3",        "BDGP5",       "2006-04", "Drosophila melanogaster",
        "dm6",        "BDGP6",       "2014-08", "Drosophila melanogaster",
        "galGal4",    "Galgal4",     "2011-11", "Gallus gallus",
        "galGal5",    "Galgal5",     "2015-12", "Gallus gallus",
        "galGal6",    "GRCg6a",      "2018-03", "Gallus gallus",
        "hg19",       "GRCh37",      "2009-02", "Homo sapiens",
        "hg38",       "GRCh38",      "2013-12", "Homo sapiens",
        "hg38-noalt", "GRCh38",      "2013-12", "Homo sapiens",
        "mm9",        "GRCm37",      "2007-07", "Mus musculus",
        "mm10",       "GRCm38",      "2011-12", "Mus musculus",
        "rn5",        "Rnor_5.0",    "2012-03", "Rattus norvegicus",
        "rn6",        "Rnor_6.0",    "2014-07", "Rattus norvegicus",
        "sacCer3",    "R64-1-1",     "2011-04", "Saccharomyces cerevisiae",
        "susScr11",   "Sscrofa11.1", "2017-02", "Sus scrofa",
        "xenTro2",    "JGI_4.1",     "2005-08", "Xenopus tropicalis",
        "xenTro3",    "JGI_4.2",     "2009-11", "Xenopus tropicalis"
    ),
    ncol = 4L, byrow = TRUE
)
