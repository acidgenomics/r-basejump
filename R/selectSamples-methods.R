#' Select Samples
#'
#' Sample selection utility function designed for interactive use.
#'
#' @name selectSamples
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param ... Selection arguments that map to the column names of
#'   [sampleData()]. `character` vectors are supported. Avoid using `logical` or
#'   `numeric` indices (e.g. [base::which()] calls) here, for improved code
#'   legibility and reproducibility.
#'
#' @return `SummarizedExperiment`.
#'
#' @examples
#' x <- selectSamples(rse_small, genotype = "wildtype")
#' print(x)
#' colnames(x)
NULL



#' @rdname selectSamples
#' @export
setMethod(
    f = "selectSamples",
    signature = signature("SummarizedExperiment"),
    definition = function(object, ...) {
        validObject(object)
        args <- list(...)
        assert_is_non_empty(args)
        assert_has_names(args)
        assert_all_are_non_missing_nor_empty_character(names(args))
        # Requiring the key-value pairs to be character.
        invisible(lapply(
            X = args,
            FUN = function(x) {
                assert_is_character(x)
                assert_all_are_non_missing_nor_empty_character(x)
            }
        ))

        # Match the arguments against the sample metadata.
        colData <- colData(object)
        assert_is_subset(names(args), colnames(colData))

        # Get the desired sample column index, using `which()`.
        list <- mapply(
            col = names(args),
            arg = args,
            MoreArgs = list(data = colData),
            FUN = function(col, arg, data) {
                which(data[[col]] %in% arg)
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )

        # Using a numeric index here.
        cols <- Reduce(f = intersect, x = list)
        assert_is_non_empty(cols)
        cols <- sort(cols)

        object[, cols]
    }
)
