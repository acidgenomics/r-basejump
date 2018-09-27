#' Make Gene-to-Symbol Mappings
#'
#' @section GFF/GTF file:
#'
#' Remote URLs and compressed files are supported.
#'
#' @name makeGene2symbol
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @include makeGRanges.R
#'
#' @inheritParams makeGRanges
#' @inheritParams gene2symbol
#'
#' @seealso [makeGRanges].
#'
#' @return `gene2symbol`.
#'
#' @examples
#' # makeGene2symbolFromEnsembl ====
#' x <- makeGene2symbolFromEnsembl(organism = "Homo sapiens")
#' print(x)
#'
#' # makeGene2symbolFromGFF ====
#' # GTF
#' file <- file.path(basejumpCacheURL, "example.gtf")
#' x <- makeGene2symbolFromGFF(file)
#' print(x)
#'
#' # GFF3
#' file <- file.path(basejumpCacheURL, "example.gff3")
#' x <- makeGene2symbolFromGFF(file)
#' print(x)
NULL



.makeGeneNamesUnique <- function(object) {
    assert_is_subset(
        x = c("geneID", "geneName"),
        y = colnames(object)
    )
    if (any(duplicated(object[["geneName"]]))) {
        object[["geneName"]] <- make.unique(object[["geneName"]])
    }
    object
}



.makeGene2symbol <- function(object) {
    assert_is_any_of(
        x = object,
        classes = c("DataFrame", "GRanges", "tbl_df")
    )

    # Coerce to tibble.
    data <- as(object, "tbl_df")
    if (!all(c("geneID", "geneName") %in% colnames(data))) {
        stop("Object does not contain gene-to-symbol mappings")
    }
    assert_has_rows(data)
    assertHasRownames(data)
    cols <- c("rowname", "geneID", "geneName")
    assert_is_subset(cols, colnames(data))

    # Sanitize using tidyverse chain.
    data <- data %>%
        select(!!!syms(cols)) %>%
        .[complete.cases(.), , drop = FALSE] %>%
        mutate_all(as.character) %>%
        unique() %>%
        .makeGeneNamesUnique() %>%
        as("DataFrame")

    new("gene2symbol", data)
}



#' @rdname makeGene2symbol
#' @export
makeGene2symbolFromEnsembl <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall(args = list(level = "genes"))
        )
        .makeGene2symbol(gr)
    }
f <- formals(makeGRangesFromEnsembl)
f <- f[setdiff(names(f), c("level"))]
formals(makeGene2symbolFromEnsembl) <- f



#' @rdname makeGene2symbol
#' @export
makeGene2symbolFromGFF <- function(file) {
    message("Making gene2symbol from GFF...")
    gff <- import(file)

    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected."))

    # Coerce to tibble.
    data <- camel(as(gff, "tbl_df"))

    # Require `geneID` column.
    assert_is_subset("geneID", colnames(data))

    # Filter rows that don't contain gene annotations.
    data <- filter(data, !is.na(!!sym("geneID")))

    if (type == "GTF") {
        if (
            !"geneName" %in% colnames(data) &&
            "geneSymbol" %in% colnames(data)
        ) {
            # Needed for FlyBase
            data[["geneName"]] <- data[["geneSymbol"]]  # nocov
        }
    } else if (type == "GFF") {
        if (
            !"geneName" %in% colnames(data) &&
            "name" %in% colnames(data)
        ) {
            data[["geneName"]] <- data[["name"]]
        }
    }

    data[["rowname"]] <- data[["geneID"]]
    .makeGene2symbol(data)
}



# Aliases ======================================================================
#' @rdname makeGene2symbol
#' @export
makeGene2symbolFromGTF <- makeGene2symbolFromGFF
