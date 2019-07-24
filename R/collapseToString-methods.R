#' @name collapseToString
#' @inherit bioverbs::collapseToString
#'
#' @inheritParams params
#' @param sep `character(1)`.
#'   Separator. Defaults to comma.
#' @param unique `logical(1)`.
#'   Unique values.
#' @param sort `logical(1)`.
#'   Sort values.
#' @param removeNA `logical(1)`.
#'   Remove NA values.
#' @param ... Additional arguments.
#'
#' @seealso [`toString()`][base::toString].
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
#' @usage collapseToString(object, ...)
#' @export
NULL



## Updated 2019-07-22.
`collapseToString,atomic` <-  # nolint
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

        ## Early return unmodified if scalar.
        if (isScalar(object)) {
            return(object)
        }

        ## Sort, if desired.
        if (isTRUE(sort)) {
            object <- sort(object, na.last = TRUE)
        }

        ## Remove NA values, if desired.
        if (!all(is.na(object))) {
            if (isTRUE(removeNA)) {
                object <- removeNA(object)
            } else {
                object <- sanitizeNA(object)
            }
        }

        ## Make unique, if desired.
        if (isTRUE(unique)) {
            object <- unique(object)
        }

        out <- as.character(object)
        out <- paste(out, collapse = sep)
        out
    }



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("atomic"),
    definition = `collapseToString,atomic`
)



## Updated 2019-07-22.
`collapseToString,matrix` <-  # nolint
    function(
        object,
        sep = ", ",
        sort = FALSE,
        removeNA = FALSE,
        unique = FALSE
    ) {
        ## Passthrough to atomic method: sep, unique, sort.
        assert(hasLength(object))

        ## Coerce to tibble to perform the collapse.
        collapse <- object %>%
            as.data.frame() %>%
            as_tibble(rownames = NULL) %>%
            mutate_all(.funs = sanitizeNA) %>%
            summarise_all(
                .funs = ~ collapseToString(
                    object = .,
                    sep = sep,
                    sort = sort,
                    removeNA = removeNA,
                    unique = unique
                )
            )

        ## Coerce collapsed tibble back to original object class.
        as(object = collapse, Class = class(object)[[1L]])
    }



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("matrix"),
    definition = `collapseToString,matrix`
)



## Updated 2019-07-22.
`collapseToString,data.frame` <-  # nolint
    `collapseToString,matrix`



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("data.frame"),
    definition = `collapseToString,data.frame`
)



## Updated 2019-07-22.
`collapseToString,DataFrame` <-  # nolint
    `collapseToString,matrix`



#' @rdname collapseToString
#' @export
setMethod(
    f = "collapseToString",
    signature = signature("DataFrame"),
    definition = `collapseToString,DataFrame`
)
