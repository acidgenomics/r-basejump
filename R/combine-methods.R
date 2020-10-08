#' @name combine
#' @inherit BiocGenerics::combine return title
#' @note Updated 2020-01-20.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @note We're attempting to make this as strict as possible, requiring:
#'
#' - Rows (genes) across objects must be identical.
#' - [rowRanges][SummarizedExperiment::rowRanges] and/or
#'   [rowData][SummarizedExperiment::rowData]
#'   [metadata][S4Vectors::metadata] must be identical.
#' - [colData][SummarizedExperiment::colData] must contain the same columns.
#' - Specific metadata must be identical (see `metadata` argument).
#'
#' @seealso
#' - `BiocGenerics::combine()`.
#' - `help("merge.Matrix", "Matrix.utils")`.
#'
#' @return `SummarizedExperiment`.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "AcidTest"
#' )
#' str_pad <- stringr::str_pad
#'
#' ## SummarizedExperiment ====
#' x <- RangedSummarizedExperiment
#' colnames(x) <- paste0(
#'     "sample",
#'     str_pad(
#'         string = seq_len(ncol(x)),
#'         width = 2L,
#'         pad = "0"
#'     )
#' )
#'
#' y <- x
#' colnames(y) <- paste0(
#'     "sample",
#'     str_pad(
#'         string = seq(from = ncol(y) + 1L, to = ncol(y) * 2L),
#'         width = 2L,
#'         pad = "0"
#'     )
#' )
#'
#' ## Combine the two objects.
#' c <- combine(x, y)
#' sampleData(c)
#' print(c)
#'
#' ## SingleCellExperiment ====
#' x <- SingleCellExperiment
#' colnames(x) <- paste0(
#'     "cell",
#'     str_pad(
#'         string = seq_len(ncol(x)),
#'         width = 4L,
#'         pad = "0"
#'     )
#' )
#'
#' y <- x
#' colnames(y) <- paste0(
#'     "cell",
#'     str_pad(
#'         string = seq_len(ncol(y)) + ncol(y),
#'         width = 4L,
#'         pad = "0"
#'     )
#' )
#'
#' ## Combine the two objects.
#' c <- combine(x, y)
#' sampleData(c)
#' print(c)
NULL



## Updated 2020-01-20.
`combine,SummarizedExperiment` <-  # nolint
    function(x, y) {
        validObject(x)
        validObject(y)
        assert(
            identical(class(x), class(y)),
            identical(assayNames(x), assayNames(y)),
            ## Require that there are no duplicate samples.
            areDisjointSets(colnames(x), colnames(y)),
            ## Currently we're being strict and requiring that the rows
            ## (features) are identical, otherwise zero counts may be
            ## misleading.
            identical(rownames(x), rownames(y))
        )
        ## Coerce the objects to SummarizedExperiment.
        ## Keep as RSE if the data is ranged.
        if (is(x, "RangedSummarizedExperiment")) {
            Class <- "RangedSummarizedExperiment"  # nolint
        } else {
            Class <- "SummarizedExperiment"  # nolint
        }
        cli_alert(sprintf("Combining objects into {.var %s}.", Class))
        x <- as(object = x, Class = Class)
        y <- as(object = y, Class = Class)

        ## Assays --------------------------------------------------------------
        cli_alert(sprintf(
            "Binding columns in {.fun assays}: {.var %s}.",
            toString(assayNames(x))
        ))
        assays <- mapply(
            x = assays(x),
            y = assays(y),
            FUN = cbind,
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        assert(is.list(assays))
        assays <- as(assays, "SimpleList")

        ## Row data ------------------------------------------------------------
        cli_alert("Checking row data.")
        ## Require that the gene annotations are identical.
        if (is(x, "RangedSummarizedExperiment")) {
            assert(identical(rowRanges(x), rowRanges(y)))
            rowRanges <- rowRanges(x)
            rowData <- NULL
        } else {
            assert(identical(rowData(x), rowData(y)))
            rowRanges <- NULL
            rowData <- rowData(x)
            ## This provides backward compatibility for BioC 3.7.
            rownames(rowData) <- rownames(x)
        }

        ## Column data ---------------------------------------------------------
        cli_alert("Updating column data.")
        cdx <- colData(x)
        cdy <- colData(y)
        ## Check for column mismatches and restore NA values, if necessary. This
        ## mismatch can occur because our metadata importer will drop columns
        ## with all NA values, which is useful for handling human metadata. This
        ## can create a column mismatch when we're subsetting large sequencing
        ## runs into batches.
        union <- union(names(cdx), names(cdy))
        intersect <- intersect(names(cdx), names(cdy))
        if (!isTRUE(identical(union, intersect))) {
            setdiff <- setdiff(union, intersect)
            cli_alert_warning(sprintf(
                "Fixing %d mismatched %s in {.fun colData}: {.var %s}.",
                length(setdiff),
                ngettext(
                    n = length(setdiff),
                    msg1 = "column",
                    msg2 = "columns"
                ),
                toString(setdiff, width = 100L)
            ))
            diffx <- setdiff(setdiff, names(cdx))
            for (col in diffx) {
                cdx[[col]] <- NA
            }
            diffy <- setdiff(setdiff, names(cdy))
            for (col in diffy) {
                cdy[[col]] <- NA
            }
            colData(x) <- cdx
            colData(y) <- cdy
        }
        assert(areSetEqual(
            x = colnames(colData(x)),
            y = colnames(colData(y))
        ))
        keep <- sort(intersect(
            x = colnames(colData(x)),
            y = colnames(colData(y))
        ))
        colData <- rbind(
            colData(x)[, keep, drop = FALSE],
            colData(y)[, keep, drop = FALSE]
        )

        ## Metadata ------------------------------------------------------------
        cli_alert("Updating metadata.")
        mx <- metadata(x)
        my <- metadata(y)
        ## We're keeping only metadata elements that are common in both objects.
        keep <- intersect(names(mx), names(my))
        if (!isTRUE(setequal(x = names(mx), y = names(my)))) {
            drop <- setdiff(x = union(names(mx), names(my)), y = keep)
            cli_alert_warning(sprintf(
                "Dropping %d disjoint metadata %s: {.var %s}.",
                length(drop),
                ngettext(
                    n = length(drop),
                    msg1 = "element",
                    msg2 = "elements"
                ),
                toString(drop, width = 100L)
            ))
        }
        mx <- mx[keep]
        my <- my[keep]
        ## Keep only metadata that is identical across both objects.
        keep <- mapply(
            x = mx,
            y = my,
            FUN = identical,
            SIMPLIFY = TRUE,
            USE.NAMES = TRUE
        )
        drop <- names(keep)[!keep]
        if (hasLength(drop)) {
            cli_alert_warning(sprintf(
                "Dropping %d non-identical metadata %s: {.var %s}.",
                length(drop),
                ngettext(
                    n = length(drop),
                    msg1 = "element",
                    msg2 = "elements"
                ),
                toString(drop, width = 100L)
            ))
        }
        assert(identical(x = mx[keep], y = my[keep]))
        metadata <- mx[keep]
        metadata[["combine"]] <- TRUE
        metadata <- Filter(Negate(is.null), metadata)

        ## Return --------------------------------------------------------------
        args <- list(
            assays = assays,
            rowRanges = rowRanges,
            rowData = rowData,
            colData = colData,
            metadata = metadata
        )
        args <- Filter(Negate(is.null), args)
        out <- do.call(what = SummarizedExperiment, args = args)
        validObject(out)
        out
    }



#' @rdname combine
#' @export
setMethod(
    f = "combine",
    signature = signature(
        x = "SummarizedExperiment",
        y = "SummarizedExperiment"
    ),
    definition = `combine,SummarizedExperiment`
)



## Updated 2019-08-27.
`combine,SingleCellExperiment` <-  # nolint
    function(x, y) {
        validObject(x)
        validObject(y)
        x <- as(object = x, Class = "RangedSummarizedExperiment")
        y <- as(object = y, Class = "RangedSummarizedExperiment")
        rse <- combine(x = x, y = y)
        validObject(rse)
        sce <- as(rse, "SingleCellExperiment")
        validObject(sce)
        sce
    }



#' @rdname combine
#' @export
setMethod(
    f = "combine",
    signature = signature(
        x = "SingleCellExperiment",
        y = "SingleCellExperiment"
    ),
    definition = `combine,SingleCellExperiment`
)
