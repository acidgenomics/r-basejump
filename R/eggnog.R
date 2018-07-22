#' EggNOG Annotations
#'
#' EggNOG is a hierarchical orthology framework with functional annotations for
#' eukaryotic, prokaryotic, and viral genomes.
#'
#' @family Database Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @seealso [EggNOG README](http://eggnogdb.embl.de/download/latest/README.txt)
#'
#' @return `list` containing (1; "`cogFunctionalCategories`") **C**luster of
#'   **O**rthologous **G**roups (COG) functional category information and (2;
#'   "`annotations`") up-to-date functional descriptions and categories for
#'   **Eu**karyotes **N**on-supervised **O**rthologous **G**roups (euNOG) and
#'   **N**on-supervised **O**rthologous **G**roups (NOG) protein identifiers.
#' @export
#'
#' @examples
#' x <- eggnog()
#' glimpse(x)
eggnog <- function() {
    # Categories ===============================================================
    file <- localOrRemoteFile(paste(
        "http://eggnogdb.embl.de",
        "download",
        "latest",
        "COG_functional_categories.txt",
        sep = "/"
    ))
    pattern <- "^\\s\\[([A-Z])\\]\\s([A-Za-z\\s]+)\\s$"
    categories <- read_lines(file) %>%
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
    file <- localOrRemoteFile(paste(
        "http://eggnogdb.embl.de",
        "download",
        "latest",
        "data",
        "euNOG",
        "euNOG.annotations.tsv.gz",
        sep = "/"
    ))
    eunog <- read_tsv(file, col_names = colnames)

    ## NOG: LUCA
    file <- localOrRemoteFile(paste(
        "http://eggnogdb.embl.de",
        "download",
        "latest",
        "data",
        "NOG",
        "NOG.annotations.tsv.gz",
        sep = "/"
    ))
    nog <- read_tsv(file, col_names = colnames)

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
