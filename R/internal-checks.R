.checkAnnotationCol <- function(annotationCol) {
    if (!(is.data.frame(annotationCol) || is.na(annotationCol))) {
        abort("`annotationCol` must be a data.frame or NA")
    }
}



.checkAsis <- function(asis) {
    if (!is.logical(asis)) {
        abort("`asis` must be logical")
    }
}



.checkBase <- function(base) {
    if (!(is.numeric(base) && length(base) == 1L)) {
        abort("`base` must be a numeric string")
    }
}



.checkCharacter <- function(object) {
    if (!is.character(object)) {
        abort(paste(
            deparse(substitute(object)),
            "must be a character vector"
        ))
    }
}



.checkColorFunction <- function(object) {
    if (!(is(object, "function") || is.null(object))) {
        abort(paste(
            paste0("`", deparse(substitute(object)), "`"),
            "must be a color palette function or NULL"
        ))
    }
}



.checkColorVector <- function(color) {
    if (!(is.character(color) || is.null(color))) {
        abort("`color` must be a character vector or NULL")
    }
    if (is.character(color)) {
        if (!any(grepl("^#", color))) {
            abort("`color` must contain hexadecimal values (e.g. #FFFFFF)")
        }
    }
}



#' @seealso base::save
#' @noRd
.checkCompress <- function(compress) {
    if (!(is_string(compress) || is.logical(compress))) {
        abort("`compress` must be a string or logical")
    }
    if (is_string(compress)) {
        validStrings <- c("bzip2", "gzip", "xz")
        if (!compress %in% validString) {
            abort(paste(
                "Valid strings for `compress`:",
                toString(validStrings)
            ))
        }
    }
}



.checkEnvir <- function(envir) {
    if (!is.environment(envir)) {
        abort("`envir` must be an environment")
    }
}



.checkFeatureGroupings <- function(object, groupings) {
    if (!is.factor(groupings)) {
        abort("`groupings` must be a factor")
    }
    if (!identical(names(groupings), rownames(object))) {
        abort("`groupings` must match object rownames")
    }
}



.checkExt <- function(ext) {
    if (!is_string(ext)) {
        abort("`ext` must be a string")
    }
}



.checkGenomeBuild <- function(genomeBuild) {
    if (!(is_string(genomeBuild) || is.null(genomeBuild))) {
        abort("`genomeBuild` must be a string or NULL")
    }
}



.checkLevel <- function(level) {
    if (!(is.numeric(level) && length(level) == 1L)) {
        abort("Markdown header `level` must be a numeric string")
    }
    if (!level %in% seq(1L:7L)) {
        abort("Markdown supports 1-7 header levels")
    }
}



.checkOrdered <- function(ordered) {
    if (!is.logical(ordered)) {
        abort("`ordered` must be logical")
    }
}



.checkNumericString <- function(object) {
    if (!(is.numeric(object) && length(object) == 1L)) {
        abort(paste(
            deparse(substitute(object)),
            "must be a numeric string"
        ))
    }
}



.checkQuiet <- function(quiet) {
    if (!is.logical(quiet)) {
        abort("`quiet` must be logical")
    }
}



.checkRelease <- function(release) {
    if (!(
        (is.numeric(release) && length(release) == 1L) ||
        is.null(release)
    )) {
        abort("`release` must be a numeric string or NULL")
    }
}



.checkReplace <- function(replace) {
    if (!is.logical(replace)) {
        abort("`replace` must be logical")
    }
}



.checkReplicateGroupings <- function(object, groupings) {
    if (!is.factor(groupings)) {
        abort("`groupings` must be a factor")
    }
    if (!identical(names(groupings), colnames(object))) {
        abort("`groupings` must match object colnames")
    }
}



.checkScale <- function(scale) {
    if (!is_string(scale)) {
        abort("`scale` must be a string")
    }
    validScale <- c("row", "column", "none")
    if (!scale %in% validScale) {
        abort(paste(
            "`scale` must contain:",
            toString(validScale)
        ))
    }
}



.checkSep <- function(sep) {
    if (!is_string(sep)) {
        abort("`sep` must be a string")
    }
}



.checkStrict <- function(strict) {
    if (!is.logical(strict)) {
        abort("`strict` must be logical")
    }
}



.checkTabset <- function(tabset) {
    if (!is.logical(tabset)) {
        abort("`tabset` must be logical")
    }
}



.checkTitle <- function(title) {
    if (!(is_string(title) || is.null(title))) {
        abort("`title` must be a string or NULL")
    }
}



.checkUniqueSymbol <- function(uniqueSymbol) {
    if (!is.logical(uniqueSymbol)) {
        abort("`uniqueSymbol` must be logical")
    }
}



.sanitizeDir <- function(dir) {
    if (!is_string(dir)) {
        abort("`dir` must be a string")
    }
    if (!dir.exists(dir)) {
        abort(paste("No directory exists at", dir))
    }
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    normalizePath(dir)
}
