#' Ensembl Annotations (Legacy Method)
#'
#' @rdname annotable
#' @name annotable
#' @family Gene Functions
#'
#' @inherit ensembl
#'
#' @return [data.frame].
#'
#' @examples
#' annotable("Homo sapiens") %>% glimpse()
#'
#' # Legacy GRCh37/hg19 genome build support
#' annotable("Homo sapiens", genomeBuild = "GRCh37") %>% glimpse()
NULL



# Methods ======================================================================
#' @rdname annotable
#' @export
setMethod(
    "annotable",
    signature("character"),
    function(
        object,
        format = "genes",
        genomeBuild = NULL,
        release = NULL,
        uniqueSymbol = FALSE) {
        # LEGACY Now using `genes` instead of `gene` for format
        if (format == "gene") {
            warning("Use 'genes' instead of 'gene' for `format` argument")
            format <- "genes"
        }
        ensembl(
            organism = object,
            format = format,
            genomeBuild = genomeBuild,
            release = release,
            uniqueSymbol = uniqueSymbol,
            broadClass = TRUE,
            sanitizeColnames = TRUE,
            return = "data.frame")
    })



#' @rdname annotable
#' @importFrom dplyr distinct everything group_by left_join mutate rename select
#'   summarize_all ungroup
#' @importFrom rlang !! !!! sym syms
#' @importFrom S4Vectors aggregate
#' @importFrom stats formula
#' @importFrom tibble as_tibble
#' @export
setMethod(
    "annotable",
    signature("data.frame"),
    function(object) {
        object <- .sanitizeAnnotationCols(object)

        # Inform the user if NA gene rows are present
        if (hasRownames(object)) {
            if (!identical(rownames(object), object[["ensgene"]])) {
                setdiff <- setdiff(rownames(object), object[["ensgene"]])
                warn(paste(
                    "Genes without annotations:", toString(sort(setdiff))
                ))
                object[["ensgene"]] <- rownames(object)
            }
        }
        assert_all_are_not_na(object[["ensgene"]])

        # Fix any `AsIs` columns resulting from DataFrame to data frame coercion
        is.AsIs <- function(x) is(x, "AsIs")  # nolint
        asIsCol <- any(vapply(
            X = object,
            FUN = is.AsIs,
            FUN.VALUE = logical(1L),
            USE.NAMES = TRUE
        ))
        if (isTRUE(asIsCol)) {
            as.list <- function(x) as(x, "list")  # nolint
            object <- mutate_if(object, is.AsIs, as.list)
            rm(as.list)
        }
        rm(is.AsIs)

        # Check for Entrez identifier column rename to `entrez`.
        # ensembldb outputs as `entrezid`.
        entrezCol <- grep("entrez", colnames(object), value = TRUE)
        if (length(entrezCol) && !identical(entrezCol, "entrez")) {
            assert_is_a_string(entrezCol)
            object <- rename(object, entrez = !!sym(entrezCol))
        }

        # Collapse (nest) Entrez identifiers from long format, if necessary
        if (
            any(duplicated(object[["ensgene"]])) &&
            !is.null(object[["entrez"]]) &&
            !is.list(object[["entrez"]])
        ) {
            inform("Nesting Entrez identifiers")
            # Alternatively can use `tidyr::nest()` approach here instead but
            # the output structure won't be consistent with the ensembl return.
            entrez <- aggregate(
                formula = formula("entrez~ensgene"),
                data = object,
                FUN = list)
            # Now drop the `entrez` column and add the aggregated list version
            object <- object %>%
                mutate(entrez = NULL) %>%
                distinct() %>%
                left_join(entrez, by = "ensgene")
        }
        assert_has_no_duplicates(object[["ensgene"]])

        object %>%
            .addBroadClassCol() %>%
            select(c(geneAnnotationCols, "broadClass"), everything()) %>%
            arrange(!!sym("ensgene")) %>%
            as.data.frame() %>%
            set_rownames(.[["ensgene"]])
    })
