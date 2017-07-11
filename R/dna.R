#' DNA sequence utilites
#'
#' Generate complement or reverse complement sequences.
#'
#' @rdname dna
#'
#' @param dna DNA sequence (`ATGC` nucleotides).
#'
#' @return Matching DNA sequence.
#'
#' @examples
#' dna <- "ATGCATGC"
#' comp(dna)
#' revcomp(dna)



#' @rdname dna
#' @export
comp <- function(dna) {
    dna %>%
        toupper %>%
        # AT base pair swap
        str_replace_all("A", "A1") %>%
        str_replace_all("T", "A") %>%
        str_replace_all("A1", "T") %>%
        # GC base pair swap
        str_replace_all("G", "G1") %>%
        str_replace_all("C", "G") %>%
        str_replace_all("G1", "C")
}



#' @rdname dna
#' @export
revcomp <- function(dna) {
    dna <- toupper(dna)
    comp <- comp(dna)
    revcomp <- str_split(comp, "") %>%
        .[[1L]] %>%
        .[order(seq_along(.), decreasing = TRUE)] %>%
        str_c(collapse = "")
    revcomp
}
