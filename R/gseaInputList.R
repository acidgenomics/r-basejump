#' @import dplyr
#' @importFrom tibble as_tibble
gseaInput <- function(foreground, background) {
    foreground <- sortUnique(foreground)
    background <- sortUnique(background)

    tibble <- tibble %>%
        tibble::as_tibble(.) %>%
        .[, c("sequence",
              "gene",
              "blastpHsapiensGene")] %>%
        dplyr::distinct(.)
    list(sequence = sortUnique(tibble$sequence),
         gene = sortUnique(tibble$gene),
         blastpHsapiensGene = sortUnique(tibble$blastpHsapiensGene))
}
