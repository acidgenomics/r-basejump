## FIXME Deprecating in favor of `gather()`.
## FIXME Need to nuke `reshape2::melt()` dependency.
## FIXME Consider requiring numeric matrix for method.



#' @name gather
#' @inherit bioverbs::gather
#' @note Updated 2019-08-26.
#'
#' @inheritParams acidroxygen::params
#' @param minCounts `integer(1)` or `NULL`.
#'   Minimum count threshold to apply. Disable with `NULL`. Filters using
#'   "greater than or equal to" logic internally. Note that this threshold gets
#'   applied prior to logarithmic transformation, when `trans` argument applies.
#' @param minCountsMethod `character(1)`.
#'   Uses [`match.arg()`][base::match.arg].
#'
#'   - `perFeature`: *Recommended*. Applies cutoff per row feature (i.e. gene).
#'     Internally, [`rowSums()`][base::rowSums] values are checked against this
#'     cutoff threshold prior to the melt operation.
#'   - `absolute`: Applies hard cutoff to `counts` column after the melt
#'     operation. This applies to all counts, not per feature.
#' @param trans `character(1)`.
#'   Apply a log transformation (e.g. `log2(x + 1L)`) to the count matrix prior
#'   to melting, if desired. Use `"identity"` to return unmodified (default).
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#' dim(rse)
#' x <- gather(rse, minCounts = NULL)
#' nrow(x)
#' print(x)
NULL



#' @rdname gather
#' @name gather
#' @importFrom bioverbs gather
#' @usage gather(object, ...)
#' @export
NULL



## nolint start
##
## > reshape2::melt(
## >     data = object,
## >     varnames = c("rowname", "colname"),
## >     value.name = "counts"
## > )
##
## > methods("melt")
## > getS3method(f = "melt", class = "array")
## > getS3method(f = "melt", class = "data.frame")
##
## > methods("gather")
## > getS3method(f = "gather", class = "default")
## > getS3method(f = "gather", class = "data.frame")
##
## > methods("gather_")
## > getS3method(f = "gather_", class = "data.frame")
##
## nolint end



## tidyr ====
gather.data.frame <-
    function(
        data,
        key = "key",
        value = "value",
        ...,
        na.rm = FALSE,
        convert = FALSE,
        factor_key = FALSE
    ) {
        key_var <- as_string(ensym2(key))
        value_var <- as_string(ensym2(value))
        quos <- quos(...)
        if (is_empty(quos)) {
            gather_vars <- setdiff(names(data), c(key_var, value_var))
        }
        else {
            gather_vars <- unname(tidyselect::vars_select(names(data), !!!quos))
        }
        if (is_empty(gather_vars)) {
            return(data)
        }
        gather_idx <- match(gather_vars, names(data))
        id_idx <- setdiff(seq_along(data), gather_idx)
        dup_indx <- match(c(key_var, value_var), names(data))
        id_idx <- setdiff(id_idx, dup_indx)
        args <- normalize_melt_arguments(data, gather_idx)
        valueAsFactor <- "factor" %in% class(args$attr_template)
        ## FIXME This still calls Rcpp internally.
        out <- melt_dataframe(
            data,
            id_idx - 1L,
            gather_idx - 1L,
            as.character(key_var),
            as.character(value_var),
            args$attr_template,
            args$factorsAsStrings,
            as.logical(valueAsFactor),
            as.logical(factor_key)
        )
        if (na.rm && anyNA(out)) {
            missing <- is.na(out[[value_var]])
            out <- out[!missing, ]
        }
        if (convert) {
            out[[key_var]] <- type.convert(as.character(out[[key_var]]),
                                           as.is = TRUE)
        }
        reconstruct_tibble(data, out, gather_vars)
    }



normalize_melt_arguments <-
    function(data, measure.ind) {
        measure.attributes <- map(measure.ind, function(i) {
            attributes(data[[i]])
        })
        measure.attrs.equal <- all_identical(measure.attributes)
        if (measure.attrs.equal) {
            attr_template <- data[[measure.ind[1]]]
        }
        else {
            warn(glue(
                "attributes are not identical across measure variables;\n       they will be dropped"
            ))
            attr_template <- NULL
        }
        any.factors <- any(map_lgl(measure.ind, function(i) is.factor(data[[i]])))
        if (any.factors) {
            attr_template <- NULL
        }
        list(attr_template = attr_template, factorsAsStrings = TRUE)
    }



reconstruct_tibble <-
    function(input, output, ungrouped_vars = chr())
    {
        if (inherits(input, "grouped_df")) {
            regroup(output, input, ungrouped_vars)
        }
        else if (inherits(input, "tbl_df")) {
            as_tibble(output)
        }
        else {
            output
        }
    }



## reshape2 ====
melt.array <-
    function(
        data,
        varnames = names(dimnames(data)),
        ...,
        na.rm = FALSE,
        as.is = FALSE,
        value.name = "value"
    ) {
        var.convert <- function(x) {
            if (!is.character(x))
                return(x)
            x <- type.convert(x, as.is = TRUE)
            if (!is.character(x))
                return(x)
            factor(x, levels = unique(x))
        }
        dn <- amv_dimnames(data)
        names(dn) <- varnames
        if (!as.is) {
            dn <- lapply(dn, var.convert)
        }
        labels <- expand.grid(
            dn,
            KEEP.OUT.ATTRS = FALSE,
            stringsAsFactors = FALSE
        )
        if (na.rm) {
            missing <- is.na(data)
            data <- data[!missing]
            labels <- labels[!missing, ]
        }
        value_df <- setNames(
            object = data.frame(as.vector(data)),
            nm = value.name
        )
        cbind(labels, value_df)
    }



melt.data.frame <-
    function(
        data,
        id.vars,
        measure.vars,
        variable.name = "variable",
        ...,
        na.rm = FALSE,
        value.name = "value",
        factorsAsStrings = TRUE
    ) {
        vars <- melt_check(
            data,
            id.vars,
            measure.vars,
            variable.name,
            value.name
        )
        id.ind <- match(vars$id, names(data))
        measure.ind <- match(vars$measure, names(data))
        if (!length(measure.ind)) {
            return(data[id.vars])
        }
        args <- normalize_melt_arguments(data, measure.ind, factorsAsStrings)
        measure.attributes <- args$measure.attributes
        factorsAsStrings <- args$factorsAsStrings
        valueAsFactor <- "factor" %in% measure.attributes$class
        ## FIXME This calls Rcpp.
        df <- melt_dataframe(
            data,
            as.integer(id.ind - 1),
            as.integer(measure.ind - 1),
            as.character(variable.name),
            as.character(value.name),
            as.pairlist(measure.attributes),
            as.logical(factorsAsStrings),
            as.logical(valueAsFactor)
        )
        if (na.rm) {
            return(df[!is.na(df[[value.name]]), ])
        }
        else {
            return(df)
        }
    }



melt_check <-
    function(data, id.vars, measure.vars, variable.name, value.name)
    {
        varnames <- names(data)
        if (!missing(id.vars) && is.numeric(id.vars)) {
            id.vars <- varnames[id.vars]
        }
        if (!missing(measure.vars) && is.numeric(measure.vars)) {
            measure.vars <- varnames[measure.vars]
        }
        if (!missing(id.vars)) {
            unknown <- setdiff(id.vars, varnames)
            if (length(unknown) > 0) {
                vars <- paste(unknown, collapse = ", ")
                stop("id variables not found in data: ", vars, call. = FALSE)
            }
        }
        if (!missing(measure.vars)) {
            unknown <- setdiff(measure.vars, varnames)
            if (length(unknown) > 0) {
                vars <- paste(unknown, collapse = ", ")
                stop("measure variables not found in data: ", vars,
                     call. = FALSE)
            }
        }
        if (missing(id.vars) && missing(measure.vars)) {
            discrete <- sapply(data, is.discrete)
            id.vars <- varnames[discrete]
            measure.vars <- varnames[!discrete]
            if (length(id.vars) != 0) {
                message("Using ", paste(id.vars, collapse = ", "),
                        " as id variables")
            }
            else {
                message("No id variables; using all as measure variables")
            }
        }
        else if (missing(id.vars)) {
            id.vars <- setdiff(varnames, measure.vars)
        }
        else if (missing(measure.vars)) {
            measure.vars <- setdiff(varnames, id.vars)
        }
        if (!is.string(variable.name))
            stop("'variable.name' should be a string", call. = FALSE)
        if (!is.string(value.name))
            stop("'value.name' should be a string", call. = FALSE)
        list(id = id.vars, measure = measure.vars)
    }



## basejump ====
.gather <- function(object) {
    stop("IN PROGRESS")
}



## Updated 2019-08-26.
`gather,matrix` <-  # nolint
    function(
        object,
        minCounts = 1L,
        minCountsMethod = c("perFeature", "absolute"),
        trans = c("identity", "log2", "log10")
    ) {
        assert(
            is.numeric(object),
            isInt(minCounts, nullOK = TRUE)
        )
        minCountsMethod <- match.arg(minCountsMethod)
        trans <- match.arg(trans)
        ## Filter rows that don't pass our `minCounts` expression cutoff. Note
        ## that we're ensuring rows containing all zeros are always dropped,
        ## even when `minCountsMethod = "absolute"`.
        if (isInt(minCounts)) {
            assert(isGreaterThanOrEqualTo(minCounts, 1L))
            if (identical(minCountsMethod, "perFeature")) {
                rowCutoff <- minCounts
            } else {
                rowCutoff <- 1L
            }
            keep <- rowSums(object) >= rowCutoff
            if (identical(minCountsMethod, "perFeature")) {
                message(sprintf(
                    "%d / %d %s passed minimum 'rowSums()' >= %s cutoff.",
                    sum(keep, na.rm = TRUE), nrow(object),
                    ngettext(
                        n = nrow(object),
                        msg1 = "feature",
                        msg2 = "features"
                    ),
                    rowCutoff
                ))
            }
            object <- object[keep, , drop = FALSE]
            ## Ensure that no zero rows propagate.
            assert(!any(rowSums(object) == 0L))
        }
        ## Using reshape2 array (matrix) method here.
        ## > help(topic = "melt.array", package = "reshape2")
        ## nolint end
        ## FIXME Remove reshape2 dependency, not maintained.
        data <- .gather(data)
        stop("IN PROGRESS")
        ## When applying an absolute threshold using `minCountsMethod`, apply
        ## this cutoff prior to logarithmic transformation.
        if (
            isInt(minCounts) &&
            identical(minCountsMethod, "absolute")
        ) {
            nPrefilter <- nrow(data)
            keep <- data[["counts"]] >= minCounts
            data <- data[keep, , drop = FALSE]
            message(sprintf(
                "%d / %d melted %s passed minimum >= %d expression cutoff.",
                nrow(data),
                nPrefilter,
                ngettext(
                    n = nPrefilter,
                    msg1 = "feature",
                    msg2 = "features"
                ),
                minCounts
            ))
        }
        ## Log transform the counts, if desired.
        if (!identical(trans, "identity")) {
            assert(isInt(minCounts))
            message(sprintf("Applying '%s(x + 1L)' transformation.", trans))
            fun <- get(
                x = trans,
                envir = asNamespace("base"),
                inherits = FALSE
            )
            assert(is.function(fun))
            data[["counts"]] <- fun(data[["counts"]] + 1L)
        }
        data <- droplevels(data)
        data
    }



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("matrix"),
    definition = `gather,matrix`
)



## Updated 2019-08-24.
`gather,data.frame` <-  # nolint
    function(object, ...) {
        assert(requireNamespace("tidyr", quietly = TRUE))
        tidyr::gather(data = object, ...)
    }



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("data.frame"),
    definition = `gather,data.frame`
)



## FIXME Matrix method. Use Rle?
## FIXME DataFrame method. Use Rle?.



## Updated 2019-08-24.
`gather,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        minCounts,
        minCountsMethod,
        trans
    ) {
        validObject(object)
        assert(isScalar(assay))
        minCountsMethod <- match.arg(minCountsMethod)
        trans <- match.arg(trans)
        ## Prepare the count matrix.
        counts <- assay(object, i = assay)
        assert(hasLength(counts))
        counts <- as.matrix(counts)
        ## Passing to matrix method.
        data <- gather(
            object = counts,
            minCounts = minCounts,
            minCountsMethod = minCountsMethod,
            trans = trans
        )
        ## Get the sample metadata.
        sampleData <- sampleData(object)
        sampleData[["colname"]] <- rownames(sampleData)
        data <- left_join(data, sampleData, by = "colname")
        data <- droplevels(data)
        data
    }

args <- c("minCounts", "minCountsMethod", "trans")
formals(`gather,SummarizedExperiment`)[args] <-
    formals(`gather,matrix`)[args]
rm(args)



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("SummarizedExperiment"),
    definition = `gather,SummarizedExperiment`
)



## Updated 2019-08-24.
`gather,SingleCellExperiment` <-  # nolint
    function(object) {
        validObject(object)
        assert(isScalar(assay))
        minCountsMethod <- match.arg(minCountsMethod)
        trans <- match.arg(trans)
        ## Prepare the count matrix.
        counts <- assay(object, i = assay)
        assert(hasLength(counts))
        ## Note that this will deparse, and can be memory intensive.
        counts <- as.matrix(counts)
        ## Passing to matrix method.
        data <- gather(
            object = counts,
            minCounts = minCounts,
            minCountsMethod = minCountsMethod,
            trans = trans
        )
        ## Get the cell-level metrics, which contains sample metadata.
        metrics <- metrics(object, return = "DataFrame")
        keep <- which(bapply(metrics, is.factor))
        metrics <- metrics[, keep, drop = FALSE]
        metrics[["colname"]] <- rownames(metrics)
        ## Join the cell-level metadata.
        data <- left_join(data, metrics, by = "colname")
        data <- droplevels(data)
        data <- encode(data)
        data
    }

formals(`gather,SingleCellExperiment`) <-
    formals(`gather,SummarizedExperiment`)



#' @rdname gather
#' @export
setMethod(
    f = "gather",
    signature = signature("SingleCellExperiment"),
    definition = `gather,SingleCellExperiment`
)
