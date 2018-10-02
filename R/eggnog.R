# FIXME Return as `EggNOG` class.



#' EggNOG Annotations
#'
#' EggNOG is a hierarchical orthology framework with functional annotations for
#' eukaryotic, prokaryotic, and viral genomes.
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param categoriesFile `string`. Functional categories file.
#' @param eunogFile `string`. euNOG file. Eukaryota.
#' @param nogFile `string`. NOG file. LUCA.
#'
#' @seealso [EggNOG README](http://eggnogdb.embl.de/download/latest/README.txt).
#'
#' @return `list` containing:
#'
#' 1. "`cogFunctionalCategories`": **C**luster of **O**rthologous **G**roups
#'    (COG) functional category information.
#' 2. "`annotations`": up-to-date functional descriptions and categories
#'    for **Eu**karyotes **N**on-supervised **O**rthologous **G**roups (euNOG)
#'    and **N**on-supervised **O**rthologous **G**roups (NOG) protein
#'    identifiers.
#'
#' @examples
#' x <- eggnog(.test = TRUE)
#' print(x)
eggnog <- function(.test = FALSE) {
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
    list(
        cogFunctionalCategories = categories,
        annotations = annotations
    )
}
