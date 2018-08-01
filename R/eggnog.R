#' EggNOG Annotations
#'
#' EggNOG is a hierarchical orthology framework with functional annotations for
#' eukaryotic, prokaryotic, and viral genomes.
#'
#' @family Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @seealso [EggNOG README](http://eggnogdb.embl.de/download/latest/README.txt)
#'
#' @return `list` containing:
#'
#' 1. "`cogFunctionalCategories`": **C**luster of **O**rthologous **G**roups
#'    (COG) functional category information.
#' 2. "`annotations`": up-to-date functional descriptions and categories
#'    for **Eu**karyotes **N**on-supervised **O**rthologous **G**roups (euNOG)
#'    and **N**on-supervised **O**rthologous **G**roups (NOG) protein
#'    identifiers.
#' @export
#'
#' @examples
#' x <- eggnog()
#' glimpse(x)
eggnog <- function() {
    # Categories ===============================================================
    pattern <- "^\\s\\[([A-Z])\\]\\s([A-Za-z\\s]+)\\s$"
    categories <- read_lines(
        file = paste(
            "http://eggnogdb.embl.de",
            "download",
            "latest",
            "COG_functional_categories.txt",
            sep = "/"
        )
    ) %>%
        str_subset(pattern) %>%
        str_match(pattern) %>%
        as_tibble() %>%
        select(-1L) %>%
        set_colnames(c("letter", "description")) %>%
        arrange(!!sym("letter"))

    # Annotations ==============================================================
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
        file = paste(
            "http://eggnogdb.embl.de",
            "download",
            "latest",
            "data",
            "euNOG",
            "euNOG.annotations.tsv.gz",
            sep = "/"
        ),
        col_names = colnames
    )

    ## NOG: LUCA
    nog <- read_tsv(
        file = paste(
            "http://eggnogdb.embl.de",
            "download",
            "latest",
            "data",
            "NOG",
            "NOG.annotations.tsv.gz",
            sep = "/"
        ),
        col_names = colnames
    )

    annotations <- bind_rows(eunog, nog) %>%
        select(!!!syms(c(
            "groupName",
            "consensusFunctionalDescription",
            "cogFunctionalCategory"
        ))) %>%
        rename(eggnogID = !!sym("groupName")) %>%
        arrange(!!sym("eggnogID"))

    # Return ====
    list(
        cogFunctionalCategories = categories,
        annotations = annotations
    )
}
