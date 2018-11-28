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
    TRUE
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
#' assertFormalInterestingGroups(rse, "condition")
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

    TRUE
}



#' Validate Classes
#'
#' Validity check capable of validating multiple slots in a single call.
#'
#' To be used inside S4 `methods::setValidity()` call or with
#' `assertthat::validate_that()`. Particularly useful for checking multiple
#' slotted objects inside `metadata()`.
#'
#' @export
#'
#' @inheritParams params
#' @param expected `list`. Named list of expected classes per slot.
#' @param subset `boolean`. Only check a subset of slots in the object.
#'
#' @seealso `assertthat::validate_that()`.
#'
#' @return `boolean` (TRUE) on sucess or `string` containing informative
#'   message on failure.
#'
#' @examples
#' data(rse)
#' validateClasses(
#'     object = S4Vectors::metadata(rse),
#'     expected = list(
#'         version = c("package_version", "numeric_version"),
#'         date = "Date",
#'         interestingGroups = "character"
#'     )
#' )
validateClasses <- function(
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
    assert_is_logical(valid)
    ifelse(
        test = all(valid),
        yes = TRUE,
        no = paste(
            "Class checks failed:",
            printString(names(valid)[!valid]),
            sep = "\n"
        )
    )
}
