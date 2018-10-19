#' @inherit Gene2Symbol-class
#'
#' @note
#' - This function will make any duplicated symbols unique by applying
#'   [base::make.unique()], which will add ".1" to the end of the gene name.
#' - No attempt is made to arrange the rows by gene identifier.
#'
#' @name Gene2Symbol
#'
#' @inheritParams general
#'
#' @return `Gene2Symbol`.
#'
#' @examples
#' data(rse_small)
#' x <- Gene2Symbol(rse_small)
#' print(x)
NULL



Gene2Symbol.DataFrame <-  # nolint
    function(object) {
        assert_has_rows(object)

        # Require genome annotation metadata to be defined.
        metadata <- metadata(object)
        mcols <- c("organism", "build", "release")
        if (!all(mcols %in% names(metadata))) {
            stop(paste0(
                "Object does not contain genome information.\n",
                "Requires: ", toString(mcols)
            ))
        }
        metadata <- metadata[mcols]
        metadata <- c(.prototypeMetadata, metadata)

        # Check for required columns.
        cols <- c("geneID", "geneName")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain gene-to-symbol mappings.\n",
                "Requires: ", toString(cols)
            ))
        }

        data <- object %>%
            .[, cols, drop = FALSE] %>%
            as_tibble(rownames = "rowname") %>%
            # This step is needed for handling raw GFF annotations.
            unique() %>%
            mutate_all(as.character) %>%
            mutate(!!sym("geneName") := make.unique(!!sym("geneName"))) %>%
            as("DataFrame")

        metadata(data) <- metadata
        new(Class = "Gene2Symbol", data)
    }



Gene2Symbol.GRanges <-  # nolint
    function(object) {
        data <- as(object, "DataFrame")
        metadata(data) <- metadata(object)
        Gene2Symbol(data)
    }



Gene2Symbol.SummarizedExperiment <-  # nolint
    function(object) {
        rownames <- rownames(object)
        if (is(object, "RangedSummarizedExperiment")) {
            data <- rowRanges(object)
        } else {
            data <- rowData(object)
        }
        out <- Gene2Symbol(data)
        rownames(out) <- rownames
        out
    }



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("DataFrame"),
    definition = Gene2Symbol.DataFrame
)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("GRanges"),
    definition = Gene2Symbol.GRanges
)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("SummarizedExperiment"),
    definition = Gene2Symbol.SummarizedExperiment
)
