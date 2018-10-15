#' @inherit EggNOG-class
#'
#' @family Annotation Functions
#' @export
#'
#' @inheritParams general
#'
#' @return `EggNOG`.
#'
#' @examples
#' x <- eggnog(.test = TRUE)
#' print(x)
eggnog <- function(.test = FALSE) {
    stopifnot(has_internet())
    assert_is_a_bool(.test)
    if (isTRUE(.test)) {
        categoriesFile <- file.path(basejumpCacheURL, "cog.txt")
        eunogFile <- file.path(basejumpCacheURL, "eunog.tsv.gz")
        nogFile <- file.path(basejumpCacheURL, "nog.tsv.gz")
    } else {
        url <- paste(
            "http://eggnogdb.embl.de",
            "download",
            "latest",
            sep = "/"
        )
        categoriesFile <- paste(
            url,
            "COG_functional_categories.txt",
            sep = "/"
        )
        eunogFile <- paste(
            url,
            "data",
            "euNOG",
            "euNOG.annotations.tsv.gz",
            sep = "/"
        )
        nogFile <- paste(
            url,
            "data",
            "NOG",
            "NOG.annotations.tsv.gz",
            sep = "/"
        )
    }
    assert_is_a_string(categoriesFile)
    assert_is_a_string(eunogFile)
    assert_is_a_string(nogFile)

    # Categories ---------------------------------------------------------------
    pattern <- "^\\s\\[([A-Z])\\]\\s([A-Za-z\\s]+)\\s$"
    categories <- read_lines(file = categoriesFile) %>%
        str_subset(pattern) %>%
        str_match(pattern) %>%
        as_tibble() %>%
        select(-1L) %>%
        set_colnames(c("letter", "description")) %>%
        arrange(!!sym("letter"))

    # Annotations --------------------------------------------------------------
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
        arrange(!!sym("eggnogID"))

    # Return -------------------------------------------------------------------
    new(
        Class = "EggNOG",
        list(
            cogFunctionalCategories = categories,
            annotations = annotations
        )
    )
}



#' @rdname eggnog
#' @usage NULL
#' @export
EggNOG <- eggnog  # nolint
