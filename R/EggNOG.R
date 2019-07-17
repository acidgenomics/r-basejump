#' @rdname EggNOG-class
#' @export
#' @inheritParams params
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
            # This is slow and unreliable on Travis, so cover locally.
            # EggNOG database doesn't support HTTPS currently.
            # nocov start
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
            # nocov end
        }
        assert(
            isString(categoriesFile),
            isString(eunogFile),
            isString(nogFile)
        )

        # Categories -----------------------------------------------------------
        pattern <- "^\\s\\[([A-Z])\\]\\s([A-Za-z\\s]+)\\s$"
        categories <- read_lines(file = categoriesFile) %>%
            str_subset(pattern) %>%
            str_match(pattern) %>%
            as.data.frame() %>%
            select(-1L) %>%
            set_colnames(c("letter", "description")) %>%
            arrange(!!sym("letter")) %>%
            as("DataFrame")

        # Annotations ----------------------------------------------------------
        colnames <- c(
            "taxonomicLevel",
            "groupName",
            "proteinCount",
            "speciesCount",
            "cogFunctionalCategory",
            "consensusFunctionalDescription"
        )

        # euNOG: Eukaryota
        eunog <- read_tsv(
            file = eunogFile,
            col_names = colnames,
            col_types = cols(),
            progress = FALSE
        )

        # NOG: LUCA
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

        # Return ---------------------------------------------------------------
        data <- List(
            cogFunctionalCategories = categories,
            annotations = annotations
        )
        metadata(data) <- .prototypeMetadata
        new(Class = "EggNOG", data)
    }
