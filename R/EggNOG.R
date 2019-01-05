#' @inherit EggNOG-class
#' @export
#' @inheritParams params
#' @examples
#' options(basejump.test = TRUE)
#' x <- EggNOG()
#' print(x)
EggNOG <-  # nolint
    function() {
        assert(hasInternet())
        if (isTRUE(getOption("basejump.test"))) {
            categoriesFile <- pasteURL(
                basejumpCacheURL,
                "cog.txt",
                protocol = "none"
            )
            eunogFile <- pasteURL(
                basejumpCacheURL,
                "eunog.tsv.gz",
                protocol = "none"
            )
            nogFile <- pasteURL(
                basejumpCacheURL,
                "nog.tsv.gz",
                protocol = "none"
            )
        } else {
            # EggNOG database doesn't support HTTPS currently.
            url <- pasteURL(
                "eggnog5.embl.de",
                "download",
                "latest",
                protocol = "http"
            )
            categoriesFile <- pasteURL(
                url,
                "COG_functional_categories.txt",
                protocol = "none"
            )
            eunogFile <- pasteURL(
                url,
                "data",
                "euNOG",
                "euNOG.annotations.tsv.gz",
                protocol = "none"
            )
            nogFile <- pasteURL(
                url,
                "data",
                "NOG",
                "NOG.annotations.tsv.gz",
                protocol = "none"
            )
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
            as_tibble() %>%
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
