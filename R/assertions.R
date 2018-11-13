# assertFormalGene2Symbol ======================================================
#' Check Gene-to-Symbol Mappings
#'
#' @inherit params
#' @export
#'
#' @param x Object class supporting `rownames`. All rownames in this object
#'   must intersect with the rownames defined in the `gene2symbol` argument.
#'
#' @examples
#' DataFrame <- S4Vectors::DataFrame
#'
#' x <- DataFrame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(x)
#'
#' gene2symbol <- Gene2Symbol(
#'     object = DataFrame(
#'         geneID = c("ENSG00000000003", "ENSG00000000005"),
#'         geneName = c("TSPAN6", "TNMD"),
#'         row.names = rownames(x)
#'     )
#' )
#' print(gene2symbol)
#'
#' geneIDs <- gene2symbol[["geneID"]]
#' print(geneIDs)
#'
#' geneNames <- gene2symbol[["geneName"]]
#' print(geneNames)
#'
#' assertFormalGene2Symbol(
#'     x = x,
#'     genes = geneIDs,
#'     gene2symbol = gene2symbol
#' )
#' assertFormalGene2Symbol(
#'     x = x,
#'     genes = geneNames,
#'     gene2symbol = gene2symbol
#' )
assertFormalGene2Symbol <- function(x, genes, gene2symbol) {
    assertHasRownames(x)
    assert_is_character(genes)
    assert_is_non_empty(genes)
    assert_is_all_of(gene2symbol, "Gene2Symbol")
    assert_are_identical(x = nrow(x), y = nrow(gene2symbol))
    if (is.null(rownames(gene2symbol))) {
        rownames(gene2symbol) <- rownames(x)
    }
    # Map genes to x rownames, using gene2symbol.
    rownames <- mapGenesToRownames(object = gene2symbol, genes = genes)
    assert_is_subset(rownames, rownames(x))
    invisible()
}



# assertFormalInterestingGroups ================================================
#' Check Interesting Groups
#'
#' Prevent unwanted downstream behavior when a missing interesting group
#' is requested by the user.
#'
#' @inherit params
#' @export
#'
#' @param x S4 class x.
#' @param interestingGroups `character`. Interesting groups.
#'
#' @examples
#' data(rse, package = "basejump")
#' assertFormalInterestingGroups(rse, "treatment")
#' assertFormalInterestingGroups(rse, NULL)
assertFormalInterestingGroups <- function(x, interestingGroups) {
    assert_that(isS4(x))
    data <- sampleData(x)

    # Check `interestingGroups` argument.
    if (is.null(interestingGroups)) {
        # Early return clean on `NULL` value (e.g. DESeqDataSet).
        return(invisible())
    } else {
        # Otherwise, require that `interestingGroups` is a character.
        assert_is_character(interestingGroups)
    }

    # Check intersection with sample data.
    assert_is_subset(interestingGroups, colnames(data))

    # Check that interesting groups columns are factors.
    invisible(lapply(
        X = data[, interestingGroups, drop = FALSE],
        FUN = assert_is_factor
    ))
}



# Consider deprecating this in a future release.
#' Check Classes
#' @export
#'
#' @inheritParams params
#' @param expected `list`. Named list of expected classes per slot.
#' @param subset `boolean`. Only check a subset of slots in the object.
#'
#' @examples
#' data(rse)
#' object <- rse
#' metadata <- S4Vectors::metadata(object)
#' checkClasses(
#'     object = metadata,
#'     expected = list(
#'         version = c("package_version", "numeric_version"),
#'         date = "Date",
#'         interestingGroups = "character"
#'     )
#' )
checkClasses <- function(
    object,
    expected,
    subset = FALSE
) {
    assert_is_list(expected)
    assert_has_names(expected)
    assert_is_a_bool(subset)
    if (isTRUE(subset)) {
        assert_is_subset(names(expected), names(object))
    } else {
        assert_are_set_equal(names(expected), names(object))
    }
    valid <- mapply(
        slot = names(expected),
        classes = expected,
        MoreArgs = list(object = object),
        FUN = function(slot, classes, object) {
            intersect <- intersect(
                x = classes,
                y = class(object[[slot]])
            )
            if (!has_length(intersect)) {
                FALSE
            } else {
                TRUE
            }
        },
        SIMPLIFY = TRUE,
        USE.NAMES = TRUE
    )
    if (!all(valid)) {
        stop(paste(
            "Class checks failed.",
            "Run `updateObject()` to update your object.",
            printString(valid),
            sep = "\n"
        ))
    }
    TRUE
}
