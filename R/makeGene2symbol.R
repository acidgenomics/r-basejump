#' Make Gene-to-Symbol Mappings
#'
#' @name makeGene2symbol
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @include makeGRanges.R
#'
#' @inheritParams makeGRanges
#' @inheritParams gene2symbol
#'
#' @seealso [makeGRanges].
#'
#' @return `DataFrame`.
#'
#' @examples
#' # makeGene2symbolFromEnsembl ====
#' x <- makeGene2symbolFromEnsembl("Homo sapiens")
#' print(x)
#'
#' # makeGene2symbolFromGFF ====
#' # GTF
#' x <- makeGene2symbolFromGFF("http://basejump.seq.cloud/example.gtf")
#' print(x)
#'
#' # GFF3
#' x <- makeGene2symbolFromGFF("http://basejump.seq.cloud/example.gff3")
#' print(x)
NULL



.makeGeneNamesUnique <- function(data) {
    assert_is_subset(
        x = c("geneID", "geneName"),
        y = colnames(data)
    )
    if (any(duplicated(data[["geneName"]]))) {
        x <- data[["geneName"]]
        n <- length(unique(x[duplicated(x)]))
        message(paste(
            "Sanitizing", n, "duplicated symbols using `make.unique()`"
        ))
        data[["geneName"]] <- make.unique(data[["geneName"]])
    }
    data
}



.makeGene2symbol <- function(data) {
    assert_is_any_of(
        x = data,
        classes = c("DataFrame", "GRanges", "tbl_df")
    )
    assert_is_non_empty(data)

    # Coerce to tibble if necessary.
    if (!is_tibble(data)) {
        data <- as(data, "tbl_df")
    }

    # Prepare the minimal columns necessary.
    assert_is_tbl_df(data)
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
makeGene2symbolFromEnsembl <- function(
    # Setting formals below.
) {
    args <- as.list(match.call())[-1L]
    args[["format"]] <- "genes"
    gr <- do.call(
        what = makeGRangesFromEnsembl,
        args = args
    )
    .makeGene2symbol(gr)
}

# Set the formals.
f <- formals(makeGRangesFromEnsembl)
f <- f[setdiff(names(f), c("format", "metadata"))]
formals(makeGene2symbolFromEnsembl) <- as.pairlist(f)



#' @rdname makeGene2symbol
#' @export
makeGene2symbolFromGFF <- function(file) {
    message("Making gene2symbol from GFF")
    gff <- readGFF(file)

    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected"))

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



#' @rdname makeGene2symbol
#' @usage NULL
#' @export
makeGene2symbolFromGFF -> makeGene2symbolFromGTF
