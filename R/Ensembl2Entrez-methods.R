#' @inherit Ensembl2Entrez-class title description return
#' @name Ensembl2Entrez
#' @note Updated 2020-10-01.
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
#' ## character ====
#' ## Ensembl-to-Entrez.
#' genes <- c("ENSG00000000003", "ENSG00000000005", "ENSG00000000419")
#' x <- Ensembl2Entrez(genes)
#' print(x)
#'
#' ## Entrez-to-Ensembl
#' genes <- c(1L, 2L, 3L).
#' x <- Entrez2Ensembl(genes)
#' print(x)
#'
#' ## SummarizedExperiment ====
#' x <- Ensembl2Entrez(rse)
#' print(x)
NULL



## Updated 2020-10-01.
.ncbiOrgDb <- function(organism) {
    assert(isString(organism))
    ah <- .annotationHub()
    ahs <- query(ah, pattern = c(organism, "NCBI", "OrgDb"))
    id <- tail(names(ahs), n = 1L)
    orgdb <- ah[[id]]
    assert(is(orgdb, "OrgDb"))
    orgdb
}



## FIXME Add character method.



## Updated 2020-10-01.
`Entrez2Ensembl,character` <-
    function(
        object,
        organism,
        format = c("1:1", "long"),
        strict = TRUE
    ) {
        assert(isFlag(strict))
        format <- match.arg(format)
        df <- select(
            x = .ncbiOrgDb(organism),
            keys = object,
            keytype = "ENTREZID",
            columns = "ENSEMBL", "SYMBOL"
        )

        ## FIXME STRICT MATCH MODE.

        ## FIXME RETURN 1:1 HERE.

        df
    }



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Entrez2Ensembl",
    signature = signature("character"),
    definition = `Entrez2Ensembl,character`
)



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
            cli_alert_info(sprintf(
                "%d %s map to Entrez IDs.",
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
            cli_alert_info(sprintf(
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
            cli_alert(
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
            cli_alert_warning(
                "Returning 1:many in long format {.emph (not recommended)}."
            )
        }
        metadata(data) <- metadata(object)
        metadata(data)[["format"]] <- format
        new(Class = "Ensembl2Entrez", data)
    }



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("DataFrame"),
    definition = `Ensembl2Entrez,DataFrame`
)



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



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("GRanges"),
    definition = `Ensembl2Entrez,GRanges`
)



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
    signature = signature("SummarizedExperiment"),
    definition = `Ensembl2Entrez,SummarizedExperiment`
)
