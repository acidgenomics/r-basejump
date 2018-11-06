# headtail =====================================================================
#' @name headtail
#' @inherit basejump.generics::headtail
#'
#' @param n `scalar integer`. Positive integer denoting the number of first and
#'   last items to include.
#' @param ascii `boolean`. Require separators to use ASCII instead of Unicode.
#'
#' @examples
#' data(rse)
#'
#' ## data.frame ====
#' headtail(datasets::mtcars, ascii = TRUE)
#' headtail(dplyr::starwars)
#'
#' ## SummarizedExperiment ====
#' headtail(rse, ascii = TRUE)
NULL



headtail.atomic <-  # nolint
    function(x, n = 2L) {
        assert_is_atomic(x)
        assertIsAnImplicitInteger(n)
        assert_all_are_positive(n)
        if (length(x) <= n) {
            out <- x
        } else {
            out <- paste(
                c(
                    head(x, n = n),
                    "...",
                    tail(x, n = n)
                ),
                collapse = " "
            )
        }
        cat(out)
        invisible()
    }



#' @describeIn headtail Paste collapse to a `string`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("atomic"),
    definition = headtail.atomic
)



headtail.matrix <-  # nolint
    function(x, n = 2L, ascii = FALSE) {
        assert_has_dims(x)
        assertIsAnImplicitInteger(n)
        assert_all_are_positive(n)
        assert_is_a_bool(ascii)

        if (nrow(x) <= n * 2L || ncol(x) <= n * 2L) {
            message("Object can't be split into quadrants.")
            out <- x[
                head(rownames(x), n = n * 2L),
                head(colnames(x), n = n * 2L),
                drop = FALSE
                ]
            out <- as.data.frame(out)
        } else {
            # Ensure that we're performing subset operation before coercion to
            # data.frame, as this can blow up in memory for sparse matrices.
            square <- x[
                c(
                    head(rownames(x), n = n),
                    tail(rownames(x), n = n)
                ),
                c(
                    head(colnames(x), n = n),
                    tail(colnames(x), n = n)
                ),
                drop = FALSE
                ]

            # Coerce to data.frame, for consistency.
            square <- as.data.frame(square)

            # Sanitize all non-atomic columns to placeholder symbol.
            list <- lapply(
                X = square,
                FUN = function(x) {
                    if (is.factor(x)) {
                        as.character(x)
                    } else if (is.atomic(x)) {
                        x
                    } else {
                        "<>"
                    }
                }
            )
            # Now safe to coerce to atomic data.frame.
            square <- data.frame(
                list,
                row.names = rownames(square),
                check.rows = FALSE,
                check.names = FALSE,
                stringsAsFactors = FALSE
            )

            # Check that we have square dimensions.
            assert_that(nrow(square) == n * 2L)
            assert_that(ncol(square) == n * 2L)

            # Split into quadrants, so we can add vertical separators.
            # upper/lower, left/right.
            ul <- square[seq_len(n), seq_len(n), drop = FALSE]
            ur <- square[seq_len(n), seq_len(n) + n, drop = FALSE]
            ll <- square[seq_len(n) + n, seq_len(n), drop = FALSE]
            lr <- square[seq_len(n) + n, seq_len(n) + n, drop = FALSE]

            head <- data.frame(
                ul,
                "\u2502" = rep("\u2502", times = n),
                ur,
                check.rows = FALSE,
                check.names = FALSE,
                stringsAsFactors = FALSE
            )
            tail <- data.frame(
                ll,
                "\u2502" = rep("\u2502", times = n),
                lr,
                check.rows = FALSE,
                check.names = FALSE,
                stringsAsFactors = FALSE
            )
            out <- rbind(
                head,
                "\u2500" = c(
                    rep("\u2500", times = n),
                    "\u253C",
                    rep("\u2500", times = n)
                ),
                tail,
                stringsAsFactors = FALSE
            )
        }

        # Substitute Unicode characters for ASCII, if desired.
        if (isTRUE(ascii)) {
            dimnames(out) <- lapply(
                X = dimnames(out),
                FUN = iconv,
                from = "UTF-8",
                to = "ASCII",
                sub = "."
            )
            out <- as.data.frame(apply(
                X = out,
                MARGIN = c(1L, 2L),  # rows and columns
                FUN = iconv,
                from = "UTF-8",
                to = "ASCII",
                sub = "."
            ))
        }

        print(out)
        invisible()
    }



#' @describeIn headtail Show first and last rows.
#' @export
setMethod(
    f = "headtail",
    signature = signature("matrix"),
    definition = headtail.matrix
)



#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("sparseMatrix"),
    definition = headtail.matrix
)



#' @describeIn headtail Same method as `matrix`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("data.frame"),
    definition = headtail.matrix
)



#' @describeIn headtail Same method as `data.frame`.
#' @export
setMethod(
    f = "headtail",
    signature = signature("DataFrame"),
    definition = headtail.matrix
)



headtail.GRanges <-  # nolint
    function() {
        headtail(
            x = as(x, "data.frame"),
            n = n,
            ascii = ascii
        )
    }
formals(headtail.GRanges) <- formals(headtail.matrix)



#' @describeIn headtail Summarize the ranges.
#' @export
setMethod(
    f = "headtail",
    signature = signature("GRanges"),
    definition = headtail.GRanges
)



headtail.SummarizedExperiment <-  # nolint
    function() {
        headtail(
            x = assay(x),
            n = n,
            ascii = ascii
        )
    }
formals(headtail.SummarizedExperiment) <- formals(headtail.matrix)



#' @describeIn headtail Summarize the primary [SummarizedExperiment::assay()].
#' @export
setMethod(
    f = "headtail",
    signature = signature("SummarizedExperiment"),
    definition = headtail.SummarizedExperiment
)



# printString ==================================================================
#' Print String
#'
#' Capture [base::print()] output of an `atomic` vector. Useful for returning
#' informative messages inside a function.
#'
#' @export
#'
#' @param x `atomic`.
#' @param max `scalar integer`. Maximum length of vector.
#'
#' @return `string`.
#'
#' @seealso [base::cat()].
#'
#' @examples
#' printString(c("hello", "world"))
printString <- function(x, max = 100L) {
    assert_is_atomic(x)
    assert_that(length(x) <= max)
    x <- capture.output(print(x))
    x <- paste(x, collapse = "\n")
    # Remove leading and trailing line breaks.
    x <- gsub("^[\n]+|[\n]+$", "", x)
    x
}



# separator ====================================================================
#' Separator Bar
#'
#' Maximum of 72 characters wide.
#'
#' @note Bioconductor HTML vignettes don't render correctly when printing > 76
#'   characters, even though the default width is set at 80.
#'
#' @export
#'
#' @param sep `string` of length 1. Separator character.
#' @param times `scalar integer`. Number of times to repeat.
#'
#' @return `string`.
#'
#' @examples
#' cat(separator(sep = "=", times = 10L))
separator <- function(
    sep = c("\u2500", "=", "-", "+"),
    times = min(c(getOption("width", 72L), 72L))
) {
    sep <- match.arg(sep)
    assertIsAnImplicitInteger(times)
    paste0(rep(x = sep, times = times), collapse = "")
}



# show =========================================================================
#' @importFrom methods show
#' @aliases NULL
#' @export
methods::show



#' @inherit methods::show
#' @name show
#'
#' @examples
#' data(rse)
#' options(basejump.test = TRUE)
#'
#' ## Gene2Symbol ====
#' x <- Gene2Symbol(rse)
#' show(x)
#'
#' ## PANTHER ====
#' x <- PANTHER("Homo sapiens", progress = FALSE)
#' show(x)
NULL



.showHeader <- function(object) {
    cat(paste(class(object), metadata(object)[["version"]]), sep = "\n")
}



show.DataFrame <-  # nolint
    function(object) {
        .showHeader(object)
        data <- as(object, "DataFrame")
        show(data)
    }



show.EggNOG <-  # nolint
    function(object) {
        .showHeader(object)
        showSlotInfo(list(
            ids = object %>%
                .[["annotations"]] %>%
                .[["eggnogID"]] %>%
                sort(),
            categories = object %>%
                .[["cogFunctionalCategories"]] %>%
                .[["description"]] %>%
                sort()
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("EggNOG"),
    definition = show.EggNOG
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("Ensembl2Entrez"),
    definition = show.DataFrame
)



show.Gene2Symbol <-  # nolint
    function(object) {
        show.DataFrame(object)
        cat(paste0(
            length(unique(object[["geneID"]])), " genes; ",
            length(unique(object[["geneName"]])), " symbols"
        ), sep = "\n")
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("Gene2Symbol"),
    definition = show.Gene2Symbol
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("HGNC2Ensembl"),
    definition = show.DataFrame
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("MGI2Ensembl"),
    definition = show.DataFrame
)



show.PANTHER <-  # nolint
    function(object) {
        .showHeader(object)
        showSlotInfo(list(
            organism = metadata(object)[["organism"]],
            release = metadata(object)[["release"]],
            genes = object[["geneID"]]
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("PANTHER"),
    definition = show.PANTHER
)



show.Tx2Gene <-  # nolint
    function(object) {
        show.DataFrame(object)
        cat(paste0(
            length(unique(object[["transcriptID"]])), " transcripts; ",
            length(unique(object[["geneID"]])), " genes"
        ), sep = "\n")
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("Tx2Gene"),
    definition = show.Tx2Gene
)



# showSlotInfo =================================================================
#' Show Slot Information
#' @param list `list`. Named list containing slot metadata.
#' @export
showSlotInfo <- function(list) {
    assert_is_list(list)
    assert_that(all(vapply(
        X = list,
        FUN = is.atomic,
        FUN.VALUE = logical(1L)
    )))
    list <- Filter(f = Negate(is.null), x = list)
    list <- Filter(f = has_length, x = list)
    # Standardize to Bioconductor `show()` conventions.
    # Refer to SummarizedExperiment method for example.
    out <- mapply(
        name = names(list),
        x = list,
        FUN = function(name, x) {
            if (has_length(x, n = 1L)) {
                paste0(name, ": ", x)
            } else {
                if (has_names(x) && length(x) <= 4L) {
                    prefix <- paste0("[", names(x), "]")
                    info <- paste(prefix, x, sep = " ", collapse = "; ")
                } else {
                    info <- capture.output(headtail(x))
                }
                paste0(name, "(", length(x), "): ", info)
            }
        },
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    cat(out, sep = "\n")
}
