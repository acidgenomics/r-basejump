#' Generator functions
#' @include AllGenerics.R
#' @noRd
NULL



#' @inherit EggNOG-class title description return
#' @note Updated 2019-08-08.
#' @export
#' @inheritParams acidroxygen::params
#' @examples
#' options(acid.test = TRUE)
#' x <- EggNOG()
#' print(x)
EggNOG <-  # nolint
    function() {
        assert(hasInternet())
        if (isTRUE(getOption("acid.test"))) {
            categoriesFile <- pasteURL(
                basejumpTestsURL, "cog.txt",
                protocol = "none"
            )
            eunogFile <- pasteURL(
                basejumpTestsURL, "eunog.tsv.gz",
                protocol = "none"
            )
            nogFile <- pasteURL(
                basejumpTestsURL, "nog.tsv.gz",
                protocol = "none"
            )
        } else {
            ## This is slow and unreliable on Travis, so cover locally.
            ## EggNOG database doesn't support HTTPS currently.
            ## nocov start
            url <- pasteURL(
                "eggnog5.embl.de", "download", "latest",
                protocol = "http"
            )
            categoriesFile <- pasteURL(
                url, "COG_functional_categories.txt",
                protocol = "none"
            )
            eunogFile <- pasteURL(
                url, "data", "euNOG", "euNOG.annotations.tsv.gz",
                protocol = "none"
            )
            nogFile <- pasteURL(
                url, "data", "NOG", "NOG.annotations.tsv.gz",
                protocol = "none"
            )
            ## nocov end
        }
        assert(
            isString(categoriesFile),
            isString(eunogFile),
            isString(nogFile)
        )

        ## Categories ----------------------------------------------------------
        pattern <- "^\\s\\[([A-Z])\\]\\s([A-Za-z\\s]+)\\s$"
        categories <- read_lines(file = categoriesFile) %>%
            str_subset(pattern) %>%
            str_match(pattern) %>%
            as.data.frame() %>%
            select(-1L) %>%
            set_colnames(c("letter", "description")) %>%
            arrange(!!sym("letter")) %>%
            as("DataFrame")

        ## Annotations ---------------------------------------------------------
        colnames <- c(
            "taxonomicLevel",
            "groupName",
            "proteinCount",
            "speciesCount",
            "cogFunctionalCategory",
            "consensusFunctionalDescription"
        )

        ## euNOG: Eukaryota
        eunog <- read_tsv(
            file = eunogFile,
            col_names = colnames,
            col_types = cols(),
            progress = FALSE
        )

        ## NOG: LUCA
        nog <- read_tsv(
            file = nogFile,
            col_names = colnames,
            col_types = cols(),
            progress = FALSE
        )

        annotations <- bind_rows(eunog, nog) %>%
            select(!!!syms(c(
                "groupName",
                "consensusFunctionalDescription",
                "cogFunctionalCategory"
            ))) %>%
            rename(eggnogID = !!sym("groupName")) %>%
            arrange(!!sym("eggnogID")) %>%
            as("DataFrame")

        ## Return --------------------------------------------------------------
        data <- List(
            cogFunctionalCategories = categories,
            annotations = annotations
        )
        metadata(data) <- .prototypeMetadata
        new(Class = "EggNOG", data)
    }



#' @inherit Ensembl2Entrez-class title description return
#' @name Ensembl2Entrez
#' @note Updated 2019-08-08.
#'
#' @inheritParams acidroxygen::params
#' @param format `character(1)`.
#'   Formatting method to apply:
#'
#'   - `"1:1"`: *Recommended.*
#'       Return with 1:1 mappings. For Ensembl genes that don't map 1:1 with
#'       Entrez, pick the oldest Entrez ID. Genes that don't map to Entrez will
#'       contain `NA` in `entrezID` column.
#'   - `"long"`:
#'       Return 1:many in long format.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' x <- Ensembl2Entrez(rse)
#' print(x)
NULL



## Updated 2019-07-22.
`Ensembl2Entrez,DataFrame` <-  # nolint
    function(object, format = c("1:1", "long")) {
        assert(hasRows(object))
        format <- match.arg(format)

        cols <- c("geneID", "entrezID")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain Ensembl-to-Entrez mappings.\n",
                "Requires: ", toString(cols)
            ))
        }

        data <- DataFrame(
            geneID = as.character(decode(object[["geneID"]])),
            entrezID = I(object[["entrezID"]]),
            row.names = rownames(object)
        )

        ## Expand to long format.
        data <- expand(data)

        ## Inform the user about genes that don't map to Entrez.
        unmapped <- data[["geneID"]][which(is.na(data[["entrezID"]]))]
        assert(hasNoDuplicates(unmapped))
        if (length(unmapped) > 0L) {
            message(paste(length(unmapped), "genes don't map to Entrez."))
        }

        ## Inform the user about how many genes multi-map to Entrez.
        multimapped <- unique(data[["geneID"]][duplicated(data[["geneID"]])])
        if (length(multimapped) > 0L) {
            message(paste(
                length(multimapped), "genes map to multiple Entrez IDs."
            ))
        }

        if (format == "1:1") {
            message(paste(
                "Returning with 1:1 mappings using oldest Entrez ID per gene."
            ))
            entrez <- object[["entrezID"]]
            assert(is.list(entrez))
            names(entrez) <- object[["geneID"]]
            map <- lapply(
                X = entrez,
                FUN = function(x) {
                    if (all(is.na(x))) {
                        NA_integer_
                    } else {
                        sort(x)[[1L]]
                    }
                }
            )
            entrez <- unlist(map)
            data <- DataFrame(
                geneID = names(entrez),
                entrezID = as.integer(entrez),
                row.names = rownames(object)
            )
        } else if (format == "long") {
            message("Returning 1:many in long format (not recommended).")
        }

        metadata(data) <- metadata(object)
        metadata(data)[["format"]] <- format
        new(Class = "Ensembl2Entrez", data)
    }



## Updated 2019-07-22.
`Ensembl2Entrez,GRanges` <-  # nolint
    function(object) {
        data <- as(object, "DataFrame")
        metadata(data) <- metadata(object)
        do.call(
            what = Ensembl2Entrez,
            args = list(
                object = data,
                format = format
            )
        )
    }

formals(`Ensembl2Entrez,GRanges`) <- formals(`Ensembl2Entrez,DataFrame`)



## Updated 2019-07-22.
`Ensembl2Entrez,SummarizedExperiment` <-  # nolint
    function(object) {
        object <- as.SummarizedExperiment(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        do.call(
            what = Ensembl2Entrez,
            args = list(
                object = data,
                format = format
            )
        )


    }

formals(`Ensembl2Entrez,SummarizedExperiment`) <-
    formals(`Ensembl2Entrez,DataFrame`)



#' @rdname Ensembl2Entrez-class
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("DataFrame"),
    definition = `Ensembl2Entrez,DataFrame`
)



#' @rdname Ensembl2Entrez-class
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("GRanges"),
    definition = `Ensembl2Entrez,GRanges`
)



#' @rdname Ensembl2Entrez-class
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("SummarizedExperiment"),
    definition = `Ensembl2Entrez,SummarizedExperiment`
)
