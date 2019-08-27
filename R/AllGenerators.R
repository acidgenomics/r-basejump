#' All generator functions
#' @include AllGenerics.R
#' @noRd
NULL



## Ensembl2Entrez ==============================================================
#' @inherit Ensembl2Entrez-class title description return
#' @name Ensembl2Entrez
#' @note Updated 2019-08-08.
#'
#' @inheritParams acidroxygen::params
#' @param format `character(1)`.
#'   Formatting method to apply:
#'
#'   - `"1:1"`: *Recommended.*
#'       Return with 1:1 mappings. For Ensembl genes that don't map 1:1 with
#'       Entrez, pick the oldest Entrez ID. Genes that don't map to Entrez will
#'       contain `NA` in `entrezID` column.
#'   - `"long"`:
#'       Return 1:many in long format.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' x <- Ensembl2Entrez(rse)
#' print(x)
NULL



## Updated 2019-08-16.
`Ensembl2Entrez,DataFrame` <-  # nolint
    function(object, format = c("1:1", "long")) {
        assert(hasRows(object))
        format <- match.arg(format)
        cols <- c("geneID", "entrezID")
        if (!all(cols %in% colnames(object))) {
            stop(sprintf(
                "Object does not contain Ensembl-to-Entrez mappings: %s.",
                toString(cols)
            ))
        }
        data <- DataFrame(
            geneID = as.character(decode(object[["geneID"]])),
            entrezID = I(object[["entrezID"]]),
            row.names = rownames(object)
        )
        ## Expand to long format.
        data <- expand(data)
        ## Inform the user about genes that don't map to Entrez.
        unmapped <- data[["geneID"]][which(is.na(data[["entrezID"]]))]
        assert(hasNoDuplicates(unmapped))
        if (length(unmapped) > 0L) {
            message(sprintf(
                "%d %s map to Entrez.",
                length(unmapped),
                ngettext(
                    n = length(unmapped),
                    msg1 = "gene doesn't",
                    msg2 = "genes don't"
                )
            ))
        }
        ## Inform the user about how many genes multi-map to Entrez.
        multimapped <- unique(data[["geneID"]][duplicated(data[["geneID"]])])
        if (length(multimapped) > 0L) {
            message(sprintf(
                "%d %s to multiple Entrez IDs.",
                length(multimapped),
                ngettext(
                    n = length(multimapped),
                    msg1 = "gene maps",
                    msg2 = "genes map"
                )
            ))
        }
        ## Return mode.
        if (format == "1:1") {
            message(
                "Returning with 1:1 mappings using oldest Entrez ID per gene."
            )
            entrez <- object[["entrezID"]]
            assert(is.list(entrez))
            names(entrez) <- object[["geneID"]]
            map <- bplapply(
                X = entrez,
                FUN = function(x) {
                    if (all(is.na(x))) {
                        NA_integer_
                    } else {
                        sort(x)[[1L]]
                    }
                }
            )
            entrez <- unlist(map, recursive = FALSE, use.names = TRUE)
            data <- DataFrame(
                geneID = names(entrez),
                entrezID = as.integer(entrez),
                row.names = rownames(object)
            )
        } else if (format == "long") {
            message("Returning 1:many in long format (not recommended).")
        }
        metadata(data) <- metadata(object)
        metadata(data)[["format"]] <- format
        new(Class = "Ensembl2Entrez", data)
    }



## Updated 2019-07-22.
`Ensembl2Entrez,GRanges` <-  # nolint
    function(object) {
        data <- as(object, "DataFrame")
        metadata(data) <- metadata(object)
        do.call(
            what = Ensembl2Entrez,
            args = list(
                object = data,
                format = format
            )
        )
    }

formals(`Ensembl2Entrez,GRanges`) <- formals(`Ensembl2Entrez,DataFrame`)



## Updated 2019-07-22.
`Ensembl2Entrez,SummarizedExperiment` <-  # nolint
    function(object) {
        object <- as.SummarizedExperiment(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        do.call(
            what = Ensembl2Entrez,
            args = list(
                object = data,
                format = format
            )
        )


    }

formals(`Ensembl2Entrez,SummarizedExperiment`) <-
    formals(`Ensembl2Entrez,DataFrame`)



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("DataFrame"),
    definition = `Ensembl2Entrez,DataFrame`
)



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("GRanges"),
    definition = `Ensembl2Entrez,GRanges`
)



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("SummarizedExperiment"),
    definition = `Ensembl2Entrez,SummarizedExperiment`
)



## Gene2Symbol =================================================================
#' @inherit Gene2Symbol-class title description return
#' @name Gene2Symbol
#'
#' @note For some organisms, gene names and gene symbols do not map 1:1 (e.g.
#'   *Homo sapiens* and *Mus musculus*). Refer to the `format` argument here in
#'   the documentation for approaches that deal with this issue.
#' @note For the `format` argument, note that "long" was used instead of
#'   "unmodified" prior to v0.10.10.
#' @note Updated 2019-08-08.
#'
#' @inheritParams acidroxygen::params
#' @param format `character(1)`.
#'   Formatting method to apply:
#'
#'   - `"makeUnique"`: *Recommended.* Apply [`make.unique()`][base::make.unique]
#'     to the `geneName` column. Gene symbols are made unique, while the gene
#'     IDs remain unmodified.
#'   - `"unmodified"`: Return `geneID` and `geneName` columns unmodified, in
#'     long format.
#'   - `"1:1"`: For gene symbols that map to multiple gene IDs, select only the
#'     first annotated gene ID.
#'
#' @seealso [makeGene2Symbol()].
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' x <- Gene2Symbol(rse)
#' print(x)
NULL



## Updated 2019-08-15.
`Gene2Symbol,DataFrame` <-  # nolint
    function(object, format = c("makeUnique", "unmodified", "1:1")) {
        assert(hasRows(object))
        format <- match.arg(format)
        ## Check for required columns.
        cols <- c("geneID", "geneName")
        if (!all(cols %in% colnames(object))) {
            stop(sprintf(
                "Object does not contain gene-to-symbol mappings: %s.",
                toString(cols)
            ))
        }
        data <- DataFrame(
            geneID = as.character(decode(object[["geneID"]])),
            geneName = as.character(decode(object[["geneName"]])),
            row.names = rownames(object)
        )
        ## Inform the user about how many symbols multi-map.
        ## Note that `duplicated` doesn't work on Rle, so we have to coerce
        ## columns to character first (see `as_tibble` call above).
        duplicated <- duplicated(data[["geneName"]])
        if (any(duplicated)) {
            dupes <- unique(data[["geneName"]][duplicated])
            message(sprintf(
                "%d non-unique gene %s detected.",
                length(dupes),
                ngettext(
                    n = length(dupes),
                    msg1 = "symbol",
                    msg2 = "symbols"
                )
            ))
        }
        ## Return mode.
        if (format == "makeUnique") {
            ## Returning 1:1 mappings with renamed gene symbols.
            ## This is the default, and including a message is too noisy, since
            ## it is used heavily in other functions.
            data[["geneName"]] <- make.unique(data[["geneName"]])
        } else if (format == "unmodified") {
            message(
                "Returning with unmodified gene symbols ",
                "(may contain duplicates)."
            )
        } else if (format == "1:1") {
            message("Returning 1:1 mappings using oldest gene ID per symbol.")
            x <- split(data, f = data[["geneName"]])
            x <- bplapply(
                X = x,
                FUN = function(x) {
                    x <- x[order(x[["geneID"]]), , drop = FALSE]
                    x <- head(x, n = 1L)
                    x
                }
            )
            x <- DataFrameList(x)
            x <- unlist(x, recursive = FALSE, use.names = FALSE)
            data <- x
            assert(is(data, "DataFrame"))
        }
        metadata(data) <- .slotGenomeMetadata(object)
        metadata(data)[["format"]] <- format
        new(Class = "Gene2Symbol", data)
    }



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("DataFrame"),
    definition = `Gene2Symbol,DataFrame`
)



## Updated 2019-07-22.
`Gene2Symbol,GRanges` <-  # nolint
    function(object, format) {
        data <- as(object, "DataFrame")
        data <- unique(data)
        metadata(data) <- metadata(object)
        do.call(what = Gene2Symbol, args = list(object = data, format = format))
    }

formals(`Gene2Symbol,GRanges`) <- formals(`Gene2Symbol,DataFrame`)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("GRanges"),
    definition = `Gene2Symbol,GRanges`
)



## Updated 2019-07-22.
`Gene2Symbol,SummarizedExperiment` <-  # nolint
    function(object, format) {
        object <- as.SummarizedExperiment(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        do.call(what = Gene2Symbol, args = list(object = data, format = format))
    }

formals(`Gene2Symbol,SummarizedExperiment`) <- formals(`Gene2Symbol,DataFrame`)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("SummarizedExperiment"),
    definition = `Gene2Symbol,SummarizedExperiment`
)



## HGNC2Ensembl ================================================================
#' @inherit HGNC2Ensembl-class title description return
#' @note Updated 2019-08-08.
#' @export
#' @inheritParams acidroxygen::params
#' @examples
#' options(acid.test = TRUE)
#' x <- HGNC2Ensembl()
#' print(x)
HGNC2Ensembl <-  # nolint
    function() {
        assert(hasInternet())
        if (isTRUE(getOption("acid.test"))) {
            file <- pasteURL(
                basejumpTestsURL, "hgnc.txt.gz",
                protocol = "none"
            )
        } else {
            ## This is unreliable on Travis, so cover locally instead.
            ## nocov start
            file <- pasteURL(
                "ftp.ebi.ac.uk",
                "pub",
                "databases",
                "genenames",
                "new",
                "tsv",
                "hgnc_complete_set.txt",
                protocol = "ftp"
            )
            ## nocov end
        }
        message("Importing HGNC to Ensembl gene ID mappings.")
        data <- withCallingHandlers(
            expr = import(file),
            message = function(m) {
                if (isTRUE(grepl(pattern = "syntactic", x = m))) {
                    invokeRestart("muffleMessage")
                } else {
                    m
                }
            }
        )
        data <- camel(data)
        data <- data[, c("hgncID", "ensemblGeneID")]
        colnames(data)[[2L]] <- "geneID"
        data <- data[!is.na(data[["geneID"]]), , drop = FALSE]
        data[["hgncID"]] <- as.integer(gsub("^HGNC\\:", "", data[["hgncID"]]))
        data <- data[order(data[["hgncID"]]), , drop = FALSE]
        data <- as(data, "DataFrame")
        metadata(data) <- .prototypeMetadata
        new(Class = "HGNC2Ensembl", data)
    }



## MGI2Ensembl =================================================================
#' @inherit MGI2Ensembl-class title description return
#' @note Updated 2019-08-08.
#' @export
#' @inheritParams acidroxygen::params
#' @examples
#' options(acid.test = TRUE)
#' x <- MGI2Ensembl()
#' print(x)
MGI2Ensembl <- function() {  # nolint
    assert(hasInternet())
    if (isTRUE(getOption("acid.test"))) {
        file <- pasteURL(basejumpTestsURL, "mgi.rpt.gz", protocol = "none")
    } else {
        file <- pasteURL(
            "www.informatics.jax.org",
            "downloads",
            "reports",
            "MGI_Gene_Model_Coord.rpt",
            protocol = "http"
        )
    }
    message("Importing MGI-to-Ensembl gene ID mappings.")
    data <- import(file, format = "tsv", colnames = FALSE)
    data <- as(data[, c(1L, 11L)], "DataFrame")
    colnames(data) <- c("mgiID", "geneID")
    data[["mgiID"]] <- as.integer(gsub("^MGI\\:", "", data[["mgiID"]]))
    data <- data[order(data[["mgiID"]]), , drop = FALSE]
    metadata(data) <- .prototypeMetadata
    new(Class = "MGI2Ensembl", data)
}



## Tx2Gene =====================================================================
#' @inherit Tx2Gene-class title description return
#' @name Tx2Gene
#'
#' @note No attempt is made to arrange the rows by transcript identifier.
#' @note Updated 2019-08-13.
#'
#' @inheritParams acidroxygen::params
#'
#' @seealso [makeTx2Gene()].
#'
#' @examples
#' ## SummarizedExperiment ====
#' data(SummarizedExperiment_transcripts, package = "acidtest")
#' txse <- SummarizedExperiment_transcripts
#'
#' ## SummarizedExperiment ====
#' x <- Tx2Gene(txse)
#' print(x)
NULL



## Updated 2019-08-13.
`Tx2Gene,DataFrame` <-  # nolint
    function(object) {
        assert(hasRows(object))
        cols <- c("transcriptID", "geneID")
        if (!all(cols %in% colnames(object))) {
            stop(sprintf(
                "Object does not contain transcript-to-gene mappings: %s.",
                toString(cols)
            ))
        }
        transcriptID <- as.character(decode(object[["transcriptID"]]))
        geneID <- as.character(decode(object[["geneID"]]))
        rownames <- rownames(object)
        if (is.null(rownames)) {
            rownames <- transcriptID
        }
        data <- DataFrame(
            transcriptID = transcriptID,
            geneID = geneID,
            row.names = rownames
        )
        metadata(data) <- .slotGenomeMetadata(object)
        new(Class = "Tx2Gene", data)
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("DataFrame"),
    definition = `Tx2Gene,DataFrame`
)



## Updated 2019-07-22.
`Tx2Gene,GRanges` <-  # nolint
    function(object) {
        data <- as(object, "DataFrame")
        ## This step is needed for handling raw GFF annotations.
        data <- unique(data)
        metadata(data) <- metadata(object)
        Tx2Gene(data)
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("GRanges"),
    definition = `Tx2Gene,GRanges`
)



## Updated 2019-07-22.
`Tx2Gene,SummarizedExperiment` <-  # nolint
    function(object) {
        object <- as.SummarizedExperiment(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        do.call(what = Tx2Gene, args = list(object = data))
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("SummarizedExperiment"),
    definition = `Tx2Gene,SummarizedExperiment`
)
