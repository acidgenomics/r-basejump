#' @name collapseToString
#' @inherit bioverbs::collapseToString
#' @inheritParams params
#'
#' @param sep `character(1)`.
#'   Separator. Defaults to comma.
#' @param unique `logical(1)`.
#'   Unique values.
#' @param sort `logical(1)`.
#'   Sort values.
#' @param removeNA `logical(1)`.
#'   Remove NA values.
#'
#' @seealso `toString`.
#'
#' @return
#' - `atomic`: `character(1)`.
#' - `dim`: Object of same class, collapsed to a single row.
#'
#' @examples
#' ## character ====
#' groceries <- c(NA, NA, "milk", "eggs", "eggs", "veggies")
#' collapseToString(
#'     groceries,
#'     unique = TRUE,
#'     sort = TRUE,
#'     removeNA = TRUE
#' )
#' collapseToString(
#'     groceries,
#'     unique = FALSE,
#'     sort = FALSE,
#'     removeNA = FALSE
#' )
#'
#' ## numeric ====
#' collapseToString(seq(1:5))
#'
#' ## logical ====
#' collapseToString(c(TRUE, FALSE))
#' collapseToString(c(NA, NaN))
#'
#' ## data.frame ====
#' datasets::iris %>%
#'     head() %>%
#'     collapseToString(sort = TRUE, unique = TRUE) %>%
#'     t()
NULL



#' @rdname collapseToString
#' @name collapseToString
#' @importFrom bioverbs collapseToString
#' @export
NULL



collapseToString.atomic <-  # nolint
    function(
        object,
        sep = ", ",
        sort = FALSE,
        removeNA = FALSE,
        unique = FALSE
    ) {
        assert(
            isAny(object, classes = c("character", "factor", "vector")),
            isString(sep),
            isFlag(unique),
            isFlag(sort)
        )

        # Early return unmodified if scalar.
        if (isScalar(object)) {
            return(object)
        }

        # Sort, if desired.
        if (isTRUE(sort)) {
            object <- sort(object, na.last = TRUE)
        }

        # Remove NA values, if desired.
        if (!all(is.na(object))) {
            if (isTRUE(removeNA)) {
                object <- removeNA(object)
            } else {
                object <- sanitizeNA(object)
            }
        }

        # Make unique, if desired.
        if (isTRUE(unique)) {
            object <- unique(object)
        }

        object %>%
            as.character() %>%
            paste(collapse = sep)
    }



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("atomic"),
    definition = collapseToString.atomic
)



collapseToString.matrix <-  # nolint
    function(
        object,
        sep = ", ",
        sort = FALSE,
        removeNA = FALSE,
        unique = FALSE
    ) {
        # Passthrough to atomic method: sep, unique, sort.
        assert(hasLength(object))

        # Coerce to tibble to perform the collapse.
        collapse <- object %>%
            as.data.frame() %>%
            as_tibble(rownames = NULL) %>%
            mutate_all(funs(sanitizeNA)) %>%
            summarise_all(funs(
                collapseToString(
                    object = .,
                    sep = sep,
                    sort = sort,
                    removeNA = removeNA,
                    unique = unique
                )
            ))

        # Coerce collapsed tibble back to original object class.
        as(object = collapse, Class = class(object)[[1L]])
    }



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("matrix"),
    definition = collapseToString.matrix
)



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("data.frame"),
    definition = collapseToString.matrix
)



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("DataFrame"),
    definition = collapseToString.matrix
)
