# removeNA =====================================================================
#' @name removeNA
#' @examples
#' ## atomic ====
#' removeNA(c("hello", "world", NA))
#' removeNA(c(1, 2, NA))
#'
#' ## matrix ====
#' from <- matrix(
#'     data = c(1, NA, 3, NA, NA, NA, 2, NA, 4),
#'     nrow = 3,
#'     ncol = 3
#' )
#' print(from)
#' to <- removeNA(from)
#' print(to)
#'
#' ## DataFrame ====
#' from <- S4Vectors::DataFrame(
#'     a = c("A", NA, "C"),
#'     b = c(NA, NA, NA),
#'     c = c("B", NA, "D")
#' )
#' print(from)
#' to <- removeNA(from)
#' print(to)
NULL



removeNA.atomic <- function(object) {
    na.omit(object)
}



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("atomic"),
    definition = removeNA.atomic
)



removeNA.matrix <-  # nolint
    function(object) {
        object %>%
            # Drop rows that are all `NA`.
            .[apply(., 1L, function(a) !all(is.na(a))), , drop = FALSE] %>%
            # Drop columns that are all `NA`.
            .[, apply(., 2L, function(a) !all(is.na(a))), drop = FALSE]
    }



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("matrix"),
    definition = removeNA.matrix
)



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("sparseMatrix"),
    definition = removeNA.matrix
)



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("data.frame"),
    definition = removeNA.matrix
)



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("DataFrame"),
    definition = removeNA.matrix
)



# sanitizeNA ===================================================================
#' @name sanitizeNA
#' @examples
#' ## character ====
#' from <- as.character(c(1L, "x", "", "NA", "NULL"))
#' print(from)
#' to <- sanitizeNA(from)
#' print(to)
#'
#' ## DataFrame ====
#' from <- S4Vectors::DataFrame(
#'     a = c("foo", ""),
#'     b = c(NA, "bar"),
#'     row.names = c("c", "d")
#' )
#' print(from)
#' to <- sanitizeNA(from)
#' print(to)
NULL



sanitizeNA.atomic <-  # nolint
    function(object) {
        # Return unmodified.
        object
    }


#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("atomic"),
    definition = sanitizeNA.atomic
)



# Note that names will be kept here after the gsub call.
sanitizeNA.character <-  # nolint
    function(object) {
        patterns <- c(
            "^$",
            "^\\s+$",
            "^NA$",
            "^NULL$",
            "^none available$"
        )
        gsub(
            pattern = paste(patterns, collapse = "|"),
            replacement = NA,
            x = object
        )
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("character"),
    definition = sanitizeNA.character
)



sanitizeNA.factor <-  # nolint
    function(object) {
        x <- sanitizeNA(as.character(object))
        levels(x) <- unique(sanitizeNA(levels(object)))
        names(x) <- names(object)
        x
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("factor"),
    definition = sanitizeNA.factor
)



sanitizeNA.data.frame <-  # nolint
    function(object) {
        if (hasRownames(object)) {
            rownames <- rownames(object)
        } else {
            rownames <- NULL
        }
        object <- mutate_if(object, is.character, funs(sanitizeNA))
        rownames(object) <- rownames
        object
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("data.frame"),
    definition = sanitizeNA.data.frame
)



sanitizeNA.DataFrame <-  # nolint
    function(object) {
        rownames <- rownames(object)
        list <- lapply(
            X = object,
            FUN = function(col) {
                sanitizeNA(col)
            })
        DataFrame(list, row.names = rownames)
    }



#' @rdname sanitizeNA
#' @export
setMethod(
    f = "sanitizeNA",
    signature = signature("DataFrame"),
    definition = sanitizeNA.DataFrame
)



# sanitizePercent ==============================================================
#' @name sanitizePercent
#' @examples
#' object <- c("100%", "10.0%", "1%", "0.1%", "0.01%")
#' class(object)
#' print(object)
#' x <- sanitizePercent(object)
#' class(x)
#' print(x)
NULL



sanitizePercent.atomic <-  # nolint
    function(object) {
        # Return unmodified.
        object
    }



#' @rdname sanitizePercent
#' @export
setMethod(
    f = "sanitizePercent",
    signature = signature("atomic"),
    definition = sanitizePercent.atomic
)



sanitizePercent.character <-  # nolint
    function(object) {
        if (all(grepl("%$", object))) {
            as.numeric(sub("%$", "", object)) / 100L
        } else {
            object
        }
    }



#' @rdname sanitizePercent
#' @export
setMethod(
    f = "sanitizePercent",
    signature = signature("character"),
    definition = sanitizePercent.character
)



# sanitizeRowData ==============================================================
#' Sanitize Row Data
#'
#' Coerce gene annotations to `DataFrame`, and keep only `atomic` columns.
#' Complex columns (e.g. Entrez ID `list`) will fail to write to disk as CSVs.
#'
#' @note Supports `GRanges` and `DataFrame` class objects.
#'
#' @export
#'
#' @inheritParams params
#'
#' @return `DataFrame`. Contains only `character` and `factor` columns.
#'
#' @examples
#' data(rse)
#' from <- SummarizedExperiment::rowRanges(rse)
#' colnames(S4Vectors::mcols(from))
#' to <- sanitizeRowData(from)
#' vapply(to, is.atomic, logical(1L))
#' print(to)
sanitizeRowData <- function(object) {
    assert_is_any_of(object, classes = c("GRanges", "DataFrame"))
    validObject(object)

    # Coerce to S3 data frame.
    # This step helps coerce nested S4 data to atomic columns.
    data <- as.data.frame(object)

    # Enforce camel case.
    data <- camel(data)

    # Keep only atomic columns. Complex columns won't write to disk as CSVs
    # or work with R Markdown functions.
    keep <- vapply(
        X = data,
        FUN = is.atomic,
        FUN.VALUE = logical(1L)
    )
    data <- data[, keep, drop = FALSE]
    assert_is_non_empty(data)

    # Return.
    as(data, "DataFrame")
}



# sanitizeSampleData ===========================================================
# TODO Update sampleData reference in working example.

#' Sanitize Sample Data
#'
#' This function will standardize sample metadata.
#'
#' The following conventions are enforced:
#'
#' - Column names will be converted to camel case, using [camel()].
#' - Required columns: `sampleName`.
#' - Blacklisted columns: `interestingGroups`, `sampleID`. These columns are
#'   generated internally, and should not be user defined.
#' - All columns will be coerced to factor.
#' - Non-atomic columns will be dropped.
#'
#' Currently supports `DataFrame` or `data.frame` input.
#'
#' @export
#'
#' @inheritParams params
#'
#' @return `DataFrame`.
#'
#' @examples
#' data(rse)
#' from <- sampleData(rse)
#' print(from)
#' to <- sanitizeSampleData(from)
#' all(vapply(to, is.factor, logical(1L)))
#' print(to)
sanitizeSampleData <- function(object) {
    assert_is_any_of(object, c("DataFrame", "data.frame"))
    assert_is_non_empty(object)
    assert_has_colnames(object)

    # Require `sampleName` column.
    assert_is_subset(
        x = "sampleName",
        y = colnames(object)
    )
    # And check for any duplicate rows.
    assert_has_no_duplicates(object[["sampleName"]])

    # Error if any non-atomic columns are detected.
    invisible(lapply(object, assert_is_atomic))

    # Coerce to DataFrame class.
    data <- as(object, "DataFrame")
    assert_is_non_empty(data)
    assert_has_rows(data)
    assertHasRownames(data)

    # Require that dimnames are valid.
    # This checks to ensure we don't have duplicate row or column names too.
    assertHasValidDimnames(data)

    # Coerce all columns to factor, and ensure levels are updated, in case
    # samples have been subset.
    rownames <- rownames(data)
    list <- lapply(
        X = data,
        FUN = function(x) {
            droplevels(as.factor(x))
        }
    )
    data <- as(list, "DataFrame")
    rownames(data) <- rownames

    # Sanitize to camel case.
    data <- camel(data)

    # Drop any blacklisted columns before return.
    blacklist <- c("interestingGroups", "sampleID")
    data <- data[, setdiff(colnames(data), blacklist), drop = FALSE]

    data
}



# stripTranscriptVersions ======================================================
#' @name stripTranscriptVersions
#' @examples
#' ## Ensembl (modify; contains versions)
#' stripTranscriptVersions(c(
#'     "ENSMUST00000000001.1",
#'     "ENSMUST00000000001-1",
#'     "ENSMUST00000000001_1"
#' ))
#'
#' ## WormBase (keep; doesn't contain versions)
#'stripTranscriptVersions("cTel79B.1")
NULL



stripTranscriptVersions.character <-  # nolint
    function(object) {
        # Pattern matching against Ensembl transcript IDs.
        # http://www.ensembl.org/info/genome/stable_ids/index.html
        # Examples: ENST (human); ENSMUST (mouse).
        assert_is_character(object)
        assert_all_are_not_na(object)
        assert_all_are_non_missing_nor_empty_character(object)
        # punct will match `-` or `_` here.
        gsub(
            pattern = "^(ENS.*[GT][[:digit:]]{11})[[:punct:]][[:digit:]]+$",
            replacement = "\\1",
            object
        )
    }



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("character"),
    definition = stripTranscriptVersions.character
)



stripTranscriptVersions.matrix <-  # nolint
    function(object) {
        rownames <- rownames(object)
        rownames <- stripTranscriptVersions(rownames)
        rownames(object) <- rownames
        object
    }



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("matrix"),
    definition = stripTranscriptVersions.matrix
)



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("sparseMatrix"),
    definition = stripTranscriptVersions.matrix
)



#' @rdname stripTranscriptVersions
#' @export
setMethod(
    f = "stripTranscriptVersions",
    signature = signature("SummarizedExperiment"),
    definition = stripTranscriptVersions.matrix
)
