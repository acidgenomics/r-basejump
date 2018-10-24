#' @inherit Ensembl2Entrez-class
#'
#' @name Ensembl2Entrez
#' @export
#'
#' @inheritParams general
#' @param format `string`. Formatting method to apply:
#' - `"1:1"`: Return with 1:1 mappings. For Ensembl genes that don't map 1:1
#'   with Entrez, pick the oldest Entrez ID. Genes that don't map to Entrez
#'   will contain `NA` in `entrezID` column.
#' - `"long"`: Return 1:many in long format.
#' - `"list"`: Return 1:many with `entrezID` as a `list` column.
#'
#' @return `Ensembl2Entrez`.
#'
#' @examples
#' data(rse_small)
#' x <- Ensembl2Entrez(rse_small)
#' print(x)
NULL



Ensembl2Entrez.DataFrame <-  # nolint
    function(
        object,
        format = c("1:1", "long", "list")
    ) {
        assert_has_rows(object)
        format <- match.arg(format)

        cols <- c("geneID", "entrezID")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain Ensembl-to-Entrez mappings.\n",
                "Requires: ", toString(cols)
            ))
        }

        data <- object[, cols, drop = FALSE]

        if (format == "1:1") {
            message("Returning with 1:1 mappings using oldest Entrez ID.")
            entrez <- object[["entrezID"]]
            assert_is_list(entrez)
            names(entrez) <- object[["geneID"]]
            map <- lapply(
                X = entrez,
                FUN = function(x) {
                    if (all(is.na(x))) {
                        NA_integer_
                    } else {
                        sort(x)[[1L]]
                    }
                }
            )
            entrez <- unlist(map)
            data <- DataFrame(
                geneID = names(entrez),
                entrezID = as.integer(entrez),
                row.names = rownames(data)
            )
        } else if (format == "long") {
            message("Returning 1:many in long format.")
            data <- data %>%
                as_tibble(rownames = NULL) %>%
                unnest(!!sym("entrezID")) %>%
                distinct() %>%
                arrange(!!!syms(cols)) %>%
                as("DataFrame")
        } else if (format == "list") {
            message("Returning 1:many using nested list column.")
        }

        metadata(data) <- metadata(object)
        metadata(data)[["format"]] <- format
        new(Class = "Ensembl2Entrez", data)
    }



Ensembl2Entrez.GRanges <-  # nolint
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
formals(Ensembl2Entrez.GRanges) <- formals(Ensembl2Entrez.DataFrame)



Ensembl2Entrez.SummarizedExperiment <-  # nolint
    function(object) {
        object <- as.SummarizedExperiment(object)
        do.call(
            what = Ensembl2Entrez,
            args = list(
                object = rowData(object),
                format = format
            )
        )


    }
formals(Ensembl2Entrez.SummarizedExperiment) <-
    formals(Ensembl2Entrez.DataFrame)



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("DataFrame"),
    definition = Ensembl2Entrez.DataFrame
)



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("GRanges"),
    definition = Ensembl2Entrez.GRanges
)



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("SummarizedExperiment"),
    definition = Ensembl2Entrez.SummarizedExperiment
)
